# ondemand.R — Modo "a demanda": buscás un @handle y devuelve el % en vivo.
#
# Flujo:  fetch_perfil(handle, fuente)  ->  extraer features  ->  calcular_score
#         ->  guarda en data/registro.csv (la "base de datos")  ->  devuelve %.
#
# Fuentes de DATOS DE TWITTER (no confundir con la API de GPT, que NO trae datos):
#   - "x_api"          : X API v2 oficial (requiere X_BEARER_TOKEN)
#   - "twitterapi_io"  : proveedor tercero barato por consulta (requiere TWITTERAPI_IO_KEY)
#   - "mock"           : datos simulados, para probar el flujo SIN gastar ni conectar nada

suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' AMPLIFICA = a quién RETUITEA (RT @x). Eso sí es amplificar — NO contamos las menciones
#' en respuestas (ahí están atacando, no amplificando). El ataque va aparte en top_respuestas.
#' @return data.frame: cuenta, n (desc).
top_interacciones <- function(tweets, n = 6) {
  if (is.null(tweets) || nrow(tweets) == 0) return(data.frame())
  m <- regmatches(tweets$text, gregexpr("(?i)RT @\\w{2,}", tweets$text, perl = TRUE))
  rts <- tolower(gsub("(?i)^rt ", "", unlist(m), perl = TRUE))     # "RT @x" -> "@x"
  rts <- rts[nchar(rts) > 2]
  if (length(rts) == 0) return(data.frame())
  tb <- sort(table(rts), decreasing = TRUE)
  utils::head(data.frame(cuenta = names(tb), n = as.integer(tb), stringsAsFactors = FALSE), n)
}

#' A quién le RESPONDE/ATACA el perfil (solo respuestas), constante y repetitivo.
#' @return data.frame: cuenta, n (nº de respuestas dirigidas), ordenado desc.
top_respuestas <- function(tweets, n = 6) {
  if (is.null(tweets) || nrow(tweets) == 0 || !"reply_to" %in% names(tweets)) return(data.frame())
  rt <- tolower(tweets$reply_to[!is.na(tweets$reply_to) & nzchar(tweets$reply_to) & tweets$reply_to != "@"])
  if (length(rt) == 0) return(data.frame())
  tb <- sort(table(rt), decreasing = TRUE)
  utils::head(data.frame(cuenta = names(tb), n = as.integer(tb), stringsAsFactors = FALSE), n)
}

#' Mensajes que el perfil REPITE (mismo texto, copia-pega). Señal de comentario automatizado.
#' @return data.frame: texto, veces (>=2), ordenado desc.
mensajes_repetidos <- function(tweets, n = 5) {
  if (is.null(tweets) || nrow(tweets) == 0) return(data.frame())
  norm <- tolower(trimws(gsub("\\s+", " ", gsub("https?://\\S+", "", tweets$text))))
  norm <- norm[nchar(norm) >= 12]
  if (length(norm) == 0) return(data.frame())
  tb <- sort(table(norm), decreasing = TRUE); tb <- tb[tb >= 2]
  if (length(tb) == 0) return(data.frame())
  utils::head(data.frame(texto = names(tb), veces = as.integer(tb), stringsAsFactors = FALSE), n)
}

#' Imágenes que el perfil REPITE (misma URL de imagen muchas veces).
#' @return data.frame: imagen (url), veces (>=2), ordenado desc.
imagenes_repetidas <- function(tweets, n = 5) {
  if (is.null(tweets) || nrow(tweets) == 0 || !"media" %in% names(tweets)) return(data.frame())
  m <- tweets$media[!is.na(tweets$media) & nzchar(tweets$media)]
  if (length(m) == 0) return(data.frame())
  tb <- sort(table(m), decreasing = TRUE); tb <- tb[tb >= 2]
  if (length(tb) == 0) return(data.frame())
  utils::head(data.frame(imagen = names(tb), veces = as.integer(tb), stringsAsFactors = FALSE), n)
}

#' Caracteriza un vector de @cuentas con la lista curada (data/cuentas_conocidas.csv).
#' @return data.frame con columnas nombre y bando (NA si desconocida).
caracterizar <- function(cuentas, ruta = "data/cuentas_conocidas.csv") {
  con <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(con) || nrow(con) == 0) return(data.frame(nombre = NA, bando = NA)[rep(1, length(cuentas)), ])
  con$key <- tolower(paste0("@", gsub("^@", "", trimws(con$handle))))
  m <- match(tolower(cuentas), con$key)
  data.frame(nombre = con$nombre[m], bando = con$bando[m], stringsAsFactors = FALSE)
}

# --- recupera tweets recientes de un user_id por la X API v2 --------------------
x_fetch_tweets <- function(user_id, bearer = Sys.getenv("X_BEARER_TOKEN"), max_n = 100) {
  if (bearer == "") stop("Falta X_BEARER_TOKEN.")
  resp <- request(sprintf("https://api.twitter.com/2/users/%s/tweets", user_id)) |>
    req_url_query(max_results = min(max_n,100), `tweet.fields`="created_at,source") |>
    req_auth_bearer_token(bearer) |> req_timeout(25) |> req_perform()
  js <- resp_body_json(resp)
  if (is.null(js$data)) return(NULL)
  do.call(rbind, lapply(js$data, function(t) data.frame(
    handle = user_id, created_at = t$created_at, text = t$text, stringsAsFactors = FALSE)))
}

# --- conector tercero (esquema twitterapi.io; ajustar al proveedor que uses) ----
fetch_twitterapi_io <- function(handle, key = Sys.getenv("TWITTERAPI_IO_KEY")) {
  if (key == "") stop("Falta TWITTERAPI_IO_KEY.")
  handle <- gsub("@","",handle)
  resp <- request("https://api.twitterapi.io/twitter/user/info") |>
    req_url_query(userName = handle) |>
    req_headers(`X-API-Key` = key) |> req_timeout(25) |> req_perform()
  u <- resp_body_json(resp)$data
  cuenta <- data.frame(
    handle = u$userName, display_name = u$name, created_at = u$createdAt,
    followers = u$followers, following = u$following, n_tweets = u$statusesCount,
    bio = ifelse(is.null(u$description), "", u$description),
    verified = isTRUE(u$isBlueVerified),
    default_avatar = grepl("default_profile", ifelse(is.null(u$profilePicture),"",u$profilePicture)),
    stringsAsFactors = FALSE)
  # tweets vía BÚSQUEDA from:cuenta (sí incluye RESPUESTAS, a diferencia de last_tweets).
  tweets <- tryCatch(ta_io_buscar_tweets(handle, key), error = function(e) NULL)
  list(cuenta = cuenta, tweets = tweets)
}

# Trae tweets de una cuenta con el endpoint de búsqueda (incluye respuestas) y pagina.
# Prueba "Latest"; si sale vacío (cuentas que solo responden), cae a "Top".
ta_io_buscar_tweets <- function(handle, key = Sys.getenv("TWITTERAPI_IO_KEY"),
    max_pages = { mp <- suppressWarnings(as.integer(Sys.getenv("MAX_PAGES"))); if (is.na(mp)) 5 else mp }) {
  h <- gsub("^@", "", handle)
  un_tipo <- function(qt) {
    acc <- list(); cursor <- ""
    for (pg in seq_len(max_pages)) {
      r <- request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
        req_url_query(query = paste0("from:", h), queryType = qt, cursor = cursor) |>
        req_headers(`X-API-Key` = key) |> req_timeout(25) |> req_perform()
      d <- resp_body_json(r); arr <- d$tweets %||% list()
      if (length(arr) == 0) break
      acc <- c(acc, arr)
      if (!isTRUE(d$has_next_page) || is.null(d$next_cursor) || identical(d$next_cursor, "")) break
      cursor <- d$next_cursor
    }
    acc
  }
  arr <- un_tipo("Latest"); if (length(arr) == 0) arr <- un_tipo("Top")
  if (length(arr) == 0) return(NULL)
  do.call(rbind, lapply(arr, function(t) data.frame(
    handle = h, created_at = t$createdAt %||% t$created_at %||% NA,
    text = t$text %||% "",
    es_respuesta = isTRUE(t$isReply),
    reply_to = tolower(paste0("@", gsub("^@", "", t$inReplyToUsername %||% ""))),
    media = tryCatch(t$extendedEntities$media[[1]]$media_url_https %||%
                     (t$entities$media[[1]]$media_url_https %||% NA_character_),
                     error = function(e) NA_character_),
    source = t$source %||% NA_character_,
    stringsAsFactors = FALSE)))
}

# --- conector MOCK: datos deterministas a partir del handle (para probar) -------
fetch_mock <- function(handle) {
  h <- gsub("@","",handle)
  set.seed(sum(utf8ToInt(h)))                       # mismo handle => mismo resultado
  parece_granja <- grepl("[0-9]{4,}$", h) || grepl("^(user|fan|bot|voz|real)", tolower(h))
  if (parece_granja) {
    cuenta <- data.frame(handle=h, display_name=h,
      created_at=format(Sys.time()-sample(c(18,19,20,21),1)*86400,"%Y-%m-%dT%H:%M:%SZ"),  # cohorte (mismo lote)
      followers=round(runif(1,0,40)), following=round(runif(1,300,900)),
      n_tweets=round(runif(1,5000,20000)), bio="", verified=FALSE,
      default_avatar=TRUE, default_profile=TRUE, stringsAsFactors=FALSE)
    amplifica <- c("@VoceroDerecha","@CampanaDerecha","@MedioAfin","@InfluencerDer")
    ataca     <- c("@ivancepedacast","@mafecarrascal","@petrogustavo","@gustavobolivar","@susanamuhamad")
    mensajes  <- c("puro show, este señor no representa a nadie","otro corrupto del régimen, despierten Colombia",
                   "este personaje es un peligro para el país","no se dejen engañar por este farsante",
                   "vendepatria, eso es lo que son")
    mi_msg <- sample(mensajes, 1)                                    # cada cuenta repite SU mensaje
    mis_obj <- sample(ataca, sample(2:3, 1))                         # se enfoca en 2-3 víctimas
    mi_amp <- sample(amplifica, sample(1:2, 1))
    tipo <- sample(c("rt","reply"), 20, replace=TRUE, prob=c(.35,.65))
    drep <- sample(mis_obj, 20, replace=TRUE); drt <- sample(mi_amp, 20, replace=TRUE)
    tw <- data.frame(handle=h,
      created_at=format(Sys.time()-runif(20,0,3)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      text=ifelse(tipo=="rt",
        paste0("RT ", drt, ": el cambio es imparable, todos unidos vamos!"),
        paste0(drep, " ", mi_msg)),
      es_respuesta = tipo=="reply",
      reply_to = ifelse(tipo=="reply", tolower(drep), NA_character_),
      media = ifelse(tipo=="reply" & runif(20) < .5, sample(c("https://pbs.twimg.com/media/MEME_A.jpg",
              "https://pbs.twimg.com/media/MEME_B.jpg"), 20, TRUE), NA_character_),
      source = "AutoPoster Pro",                                   # fuente automatizada (bodega)
      stringsAsFactors=FALSE)
  } else {
    cuenta <- data.frame(handle=h, display_name=h,
      created_at=format(Sys.time()-runif(1,500,3500)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      followers=round(rlnorm(1,6,1)), following=round(rlnorm(1,5.5,1)),
      n_tweets=round(rlnorm(1,7,1)), bio="Persona real, opiniones propias.",
      verified=FALSE, default_avatar=FALSE, default_profile=FALSE, stringsAsFactors=FALSE)
    tw <- data.frame(handle=h,
      created_at=format(Sys.time()-runif(8,0,40)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      # texto ÚNICO por humano (frase + handle + número) -> no genera falsa coordinación
      text=paste0(sample(c("Qué día tan bonito en la ciudad","Vamos Colombia con toda la fe",
                    "Buen partido anoche del equipo","Café y a trabajar como siempre","Feliz finde a la familia linda",
                    "El agua es vida cuidémosla entre todos","Hoy llueve fuerte en Bogotá"),8,TRUE), " ", h, " ", sample(1000:99999,8)),
      es_respuesta=FALSE, reply_to=NA_character_,
      source=sample(c("Twitter for iPhone","Twitter for Android","Twitter Web App"),8,TRUE),
      stringsAsFactors=FALSE)
  }
  list(cuenta = cuenta, tweets = tw)
}

# --- dispatcher -----------------------------------------------------------------
fetch_perfil <- function(handle, fuente = c("mock","x_api","twitterapi_io")) {
  fuente <- match.arg(fuente)
  if (fuente == "mock") return(fetch_mock(handle))
  if (fuente == "twitterapi_io") return(fetch_twitterapi_io(handle))
  if (fuente == "x_api") {
    cuenta <- x_lookup_usuarios(handle)
    uid <- tryCatch(x_lookup_id(handle), error=function(e) NA)
    tweets <- if (!is.na(uid)) tryCatch(x_fetch_tweets(uid), error=function(e) NULL) else NULL
    return(list(cuenta = cuenta, tweets = tweets))
  }
}

# helper: resolver user_id de un handle (X API v2)
x_lookup_id <- function(handle, bearer = Sys.getenv("X_BEARER_TOKEN")) {
  h <- gsub("@","",handle)
  resp <- request(sprintf("https://api.twitter.com/2/users/by/username/%s", h)) |>
    req_auth_bearer_token(bearer) |> req_timeout(25) |> req_perform()
  resp_body_json(resp)$data$id
}

#' Audita UN handle a demanda y lo guarda en la base de datos local.
#' @param handle p.ej. "@jvievi"
#' @param fuente "mock" | "x_api" | "twitterapi_io"
#' @param usar_llm si TRUE, añade la señal de contenido vía OpenAI (ver R/llm.R)
#' @return lista con: handle, pct (0-100), banda, n_flags, senales (texto), detalle.
auditar_handle <- function(handle, fuente = "mock", usar_llm = FALSE,
                           registro = "data/registro.csv") {
  p <- fetch_perfil(handle, fuente)
  feats <- extraer_features_cuenta(p$cuenta)
  if (!is.null(p$tweets) && nrow(p$tweets) > 0) {
    ftw <- extraer_features_tweets(p$tweets)
    if (!is.null(ftw)) feats <- dplyr::left_join(feats, ftw, by = "handle")
  }
  if (usar_llm && exists("analizar_texto_llm") && !is.null(p$tweets)) {
    llm <- tryCatch(analizar_texto_llm(p$tweets$text), error = function(e) NA)
    if (!is.na(llm)) feats$llm_contenido <- llm
  }
  scored <- calcular_score(feats)

  # nombres legibles de las señales activas
  etiquetas <- c(flag_cuenta_muy_nueva="cuenta muy nueva", flag_cuenta_recien_creada="cuenta recién creada (<30 días)",
    flag_casi_solo_respuestas="casi solo responde/comenta (torpedo)", flag_hiperactividad="hiperactividad",
    flag_actividad_extrema="actividad extrema", flag_ff_desbalanceado="sigue a muchos / pocos seguidores",
    flag_handle_aleatorio="handle aleatorio", flag_sin_bio="sin biografía",
    flag_avatar_default="avatar por defecto", flag_perfil_default="perfil sin personalizar",
    flag_casi_solo_rt="casi solo retweets", flag_contenido_repetido="contenido repetido",
    flag_sin_descanso="actividad sin descanso (24h)", flag_texto_automatizado="texto parece automatizado (LLM)")
  activos <- names(etiquetas)[sapply(names(etiquetas), function(c) c %in% names(scored) && scored[[c]][1]==1)]
  senales <- unname(etiquetas[activos])

  res <- list(
    handle  = scored$handle[1],
    pct     = round(100 * scored$score_inaut[1]),
    banda   = as.character(scored$banda[1]),
    n_flags = scored$n_flags[1],
    senales = senales,
    fuente  = fuente,
    detalle = scored)
  top <- top_interacciones(p$tweets)                       # top de cuentas que amplifica/ataca
  if (nrow(top) > 0) top <- cbind(top, caracterizar(top$cuenta))
  res$top <- top
  resp <- top_respuestas(p$tweets)                         # a quién le responde/ataca
  if (nrow(resp) > 0) resp <- cbind(resp, caracterizar(resp$cuenta))
  res$respuestas <- resp
  res$repetidos  <- mensajes_repetidos(p$tweets)           # mensajes que repite (copia-pega)
  res$imagenes   <- imagenes_repetidas(p$tweets)           # imágenes que repite
  res$narrativa  <- if (exists("extraer_narrativa")) extraer_narrativa(p$tweets) else NULL
  res$textos     <- if (!is.null(p$tweets) && nrow(p$tweets) > 0) utils::head(p$tweets$text, 30) else character(0)
  res$cuenta_creada  <- p$cuenta$created_at[1]
  res$tweets_muestra <- if (!is.null(p$tweets) && nrow(p$tweets) > 0)
    utils::head(p$tweets[, intersect(c("handle","created_at","text","source"), names(p$tweets)), drop=FALSE], 40) else NULL

  # guardar en la "base de datos"
  fila <- data.frame(fecha=as.character(Sys.time()), handle=res$handle, pct=res$pct,
    banda=res$banda, n_flags=res$n_flags, fuente=fuente,
    senales=paste(senales, collapse="; "), stringsAsFactors=FALSE)
  tryCatch({
    dir.create(dirname(registro), showWarnings=FALSE, recursive=TRUE)
    write.table(fila, registro, sep=",", row.names=FALSE,
      col.names=!file.exists(registro), append=file.exists(registro), qmethod="double")
  }, error=function(e) NULL)   # en servidores de solo-lectura no debe romper
  res$fila <- fila
  res
}
