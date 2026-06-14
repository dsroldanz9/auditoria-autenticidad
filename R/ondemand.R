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

#' Top de cuentas con las que un perfil interactúa (RT, citas, menciones, respuestas).
#' Se extrae del texto de sus tweets. @return data.frame: cuenta, n (desc), ordenado.
top_interacciones <- function(tweets, n = 5) {
  if (is.null(tweets) || nrow(tweets) == 0) return(data.frame())
  ms <- tolower(unlist(regmatches(tweets$text, gregexpr("@\\w{2,}", tweets$text))))
  if (length(ms) == 0) return(data.frame())
  tb <- sort(table(ms), decreasing = TRUE)
  utils::head(data.frame(cuenta = names(tb), n = as.integer(tb), stringsAsFactors = FALSE), n)
}

# --- recupera tweets recientes de un user_id por la X API v2 --------------------
x_fetch_tweets <- function(user_id, bearer = Sys.getenv("X_BEARER_TOKEN"), max_n = 100) {
  if (bearer == "") stop("Falta X_BEARER_TOKEN.")
  resp <- request(sprintf("https://api.twitter.com/2/users/%s/tweets", user_id)) |>
    req_url_query(max_results = min(max_n,100), `tweet.fields`="created_at,source") |>
    req_auth_bearer_token(bearer) |> req_perform()
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
    req_headers(`X-API-Key` = key) |> req_perform()
  u <- resp_body_json(resp)$data
  cuenta <- data.frame(
    handle = u$userName, display_name = u$name, created_at = u$createdAt,
    followers = u$followers, following = u$following, n_tweets = u$statusesCount,
    bio = ifelse(is.null(u$description), "", u$description),
    verified = isTRUE(u$isBlueVerified),
    default_avatar = grepl("default_profile", ifelse(is.null(u$profilePicture),"",u$profilePicture)),
    stringsAsFactors = FALSE)
  # tweets recientes (otro endpoint; ~+créditos). Best-effort: si el esquema difiere, no rompe.
  tweets <- tryCatch({
    r2 <- request("https://api.twitterapi.io/twitter/user/last_tweets") |>
      req_url_query(userName = handle) |> req_headers(`X-API-Key` = key) |> req_perform()
    d <- resp_body_json(r2); arr <- d$data$tweets %||% d$tweets %||% d$data
    if (is.null(arr)) NULL else do.call(rbind, lapply(arr, function(t) data.frame(
      handle = handle, created_at = t$createdAt %||% t$created_at %||% NA,
      text = t$text %||% "", stringsAsFactors = FALSE)))
  }, error = function(e) NULL)
  list(cuenta = cuenta, tweets = tweets)
}

# --- conector MOCK: datos deterministas a partir del handle (para probar) -------
fetch_mock <- function(handle) {
  h <- gsub("@","",handle)
  set.seed(sum(utf8ToInt(h)))                       # mismo handle => mismo resultado
  parece_granja <- grepl("[0-9]{4,}$", h) || grepl("^(user|fan|bot|voz|real)", tolower(h))
  if (parece_granja) {
    cuenta <- data.frame(handle=h, display_name=h,
      created_at=format(Sys.time()-runif(1,10,80)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      followers=round(runif(1,0,40)), following=round(runif(1,300,900)),
      n_tweets=round(runif(1,5000,20000)), bio="", verified=FALSE,
      default_avatar=TRUE, default_profile=TRUE, stringsAsFactors=FALSE)
    objetivos <- c("@CandidatoOficial","@VoceroDeCampana","@PrensaAfin")
    tw <- data.frame(handle=h,
      created_at=format(Sys.time()-runif(20,0,2)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      text=paste0("RT ", sample(objetivos, 20, replace=TRUE, prob=c(.55,.30,.15)),
                  ": El cambio es imparable, todos unidos vamos!"), stringsAsFactors=FALSE)
  } else {
    cuenta <- data.frame(handle=h, display_name=h,
      created_at=format(Sys.time()-runif(1,500,3500)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      followers=round(rlnorm(1,6,1)), following=round(rlnorm(1,5.5,1)),
      n_tweets=round(rlnorm(1,7,1)), bio="Persona real, opiniones propias.",
      verified=FALSE, default_avatar=FALSE, default_profile=FALSE, stringsAsFactors=FALSE)
    tw <- data.frame(handle=h,
      created_at=format(Sys.time()-runif(8,0,40)*86400,"%Y-%m-%dT%H:%M:%SZ"),
      text=sample(c("Qué día tan bonito en la ciudad","Vamos Colombia @SeleccionCol",
                    "Buen partido anoche","Café y a trabajar","Feliz finde a la familia",
                    "El agua es vida, cuidémosla","Hoy llueve en Bogotá @ElTiempo"),8,TRUE),
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
    req_auth_bearer_token(bearer) |> req_perform()
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
  etiquetas <- c(flag_cuenta_muy_nueva="cuenta muy nueva", flag_hiperactividad="hiperactividad",
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
    top     = top_interacciones(p$tweets),   # top de cuentas que amplifica
    detalle = scored)

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
