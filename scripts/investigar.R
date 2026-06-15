# investigar.R — Investigación masiva + exporta data para el sitio (GitHub Pages).
#
# data/objetivos.txt admite dos tipos de línea:
#   @cuenta        -> analiza esa cuenta
#   >@victima      -> EXPANDE: trae a TODOS los que le responden/atacan y los analiza (snowball)
#
# Modo REAL: setear USAR_REAL=1 y TWITTERAPI_IO_KEY. Si no, usa demo (mock, lista grande generada).
# Control de costo: MAX_CUENTAS (default 80) limita cuántas cuentas se analizan.

if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
for (f in c("features.R","score.R","coordination.R","connectors.R","llm.R","ondemand.R","colectivo.R","bodega.R","pipeline.R"))
  source(file.path("R", f))
suppressPackageStartupMessages(library(jsonlite))

REAL <- Sys.getenv("USAR_REAL") == "1" && Sys.getenv("TWITTERAPI_IO_KEY") != ""
fuente <- if (REAL) "twitterapi_io" else "mock"
MAX <- suppressWarnings(as.integer(Sys.getenv("MAX_CUENTAS"))); if (is.na(MAX)) MAX <- 80

# trae a quienes RESPONDEN/ATACAN a una cuenta (snowball, solo modo real)
atacantes_de <- function(target, max_pages = 5) {
  t <- gsub("^[>@]+", "", target); acc <- character(0); cursor <- ""
  for (pg in seq_len(max_pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0("to:", t), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = Sys.getenv("TWITTERAPI_IO_KEY")) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    acc <- c(acc, vapply(arr, function(x) x$author$userName %||% NA_character_, character(1)))
    if (!isTRUE(d$has_next_page) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  unique(paste0("@", acc[!is.na(acc) & nzchar(acc)]))
}

# trae a quienes AMPLIFICAN (mencionan/RT) a una cuenta — para rastrear la red de la derecha
amplificadores_de <- function(target, max_pages = 5) {
  t <- gsub("^[+@]+", "", target); acc <- character(0); cursor <- ""
  for (pg in seq_len(max_pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0("@", t), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = Sys.getenv("TWITTERAPI_IO_KEY")) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    acc <- c(acc, vapply(arr, function(x) x$author$userName %||% NA_character_, character(1)))
    if (!isTRUE(d$has_next_page) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  unique(paste0("@", acc[!is.na(acc) & nzchar(acc)]))
}

# trae a quienes responden a una cuenta CON CONTENIDO HOSTIL (atacantes reales, no seguidores)
HOSTIL <- '(castrochavismo OR comunista OR "fuera petro" OR dictador OR narcopresidente OR vendepatria OR "petro corrupto" OR castrochavista OR "petro ladron" OR regimen OR mermelada)'
atacantes_hostiles <- function(target, max_pages = 6) {
  t <- gsub("^[!@]+", "", target); acc <- character(0); cursor <- ""
  for (pg in seq_len(max_pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0("to:", t, " ", HOSTIL), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = Sys.getenv("TWITTERAPI_IO_KEY")) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    acc <- c(acc, vapply(arr, function(x) x$author$userName %||% NA_character_, character(1)))
    if (!isTRUE(d$has_next_page) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  unique(paste0("@", acc[!is.na(acc) & nzchar(acc)]))
}

# postura por LÉXICO (fallback gratis cuando no hay IA): usa la muestra de textos + mensajes repetidos
L_ANTI <- c("castrochav","comunis","dictadu","narcopresident","fuera petro","petro corrupto","petro ladron",
  "régimen","regimen","vendepatr","mermelada","expropi","venezolaniz","chavis","terrorist","narcoestado",
  "narco estado","castro","farc","payaso petro","renuncie","sapo petro","petro mentiroso","robaron","tirano")
L_PRO <- c("narco fico","paraco","banda narco","falsos positivos","defender narco","defiende narco","ñeñe",
  "genocida","uribe preso","8000","narcoparamilitar","abelardo narco","fico narco","uribe paraco",
  "fuerza petro","vamos petro","petro presidente","verguenza abelardo","narco abelardo")
postura_lexico <- function(r) {
  partes <- r$textos %||% character(0)
  if (!is.null(r$repetidos) && nrow(r$repetidos)) partes <- c(partes, r$repetidos$texto)
  txt <- tolower(paste(partes, collapse=" "))
  if (nchar(txt) < 5) return("indeterminado")
  a <- sum(vapply(L_ANTI, function(k) grepl(k, txt, fixed=TRUE), logical(1)))
  p <- sum(vapply(L_PRO,  function(k) grepl(k, txt, fixed=TRUE), logical(1)))
  if (a > p) "ataca_pacto" else if (p > a) "apoya_pacto" else "indeterminado"
}

# BÚSQUEDA POR CONSIGNA (lo más eficiente): quien postea el texto EXACTO es bodega.
# Línea en objetivos.txt que empieza con "  -> se busca esa frase y se traen sus autores.
cuentas_por_frase <- function(frase, max_pages = 5) {
  q <- paste0('"', substr(trimws(gsub('^"', '', frase)), 1, 70), '"')   # frase exacta, chunk distintivo
  acc <- character(0); cursor <- ""
  for (pg in seq_len(max_pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = q, queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = Sys.getenv("TWITTERAPI_IO_KEY")) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    acc <- c(acc, vapply(arr, function(x) x$author$userName %||% NA_character_, character(1)))
    if (!isTRUE(d$has_next_page) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  unique(paste0("@", acc[!is.na(acc) & nzchar(acc)]))
}

# ---- arma la lista de cuentas a investigar ----
lineas <- if (file.exists("data/objetivos.txt")) trimws(readLines("data/objetivos.txt", warn = FALSE)) else character(0)
lineas <- lineas[nchar(lineas) > 0]
if (length(lineas) == 0) {                                  # demo MASIVO: ~1500 granja + 120 humanas
  lineas <- c(sprintf("@fanpatriota%04d", 1:60), sprintf("@ciudadano_real_%d", 1:200))  # demo: minoría bodega + mayoría reales
}
objetivos <- character(0)
for (l in lineas) {
  if (startsWith(l, '"')) { if (REAL) { cat("Buscando consigna:", substr(l,1,50), "...\n"); objetivos <- c(objetivos, cuentas_por_frase(l)) } }
  else if (startsWith(l, "!")) { if (REAL) { cat("Atacantes HOSTILES de", l, "...\n"); objetivos <- c(objetivos, atacantes_hostiles(l)) } }
  else if (startsWith(l, ">")) { if (REAL) { cat("Responden a", l, "(todos)...\n"); objetivos <- c(objetivos, atacantes_de(l)) } }
  else if (startsWith(l, "+")) { if (REAL) { cat("Amplificadores de", l, "...\n"); objetivos <- c(objetivos, amplificadores_de(l)) } }
  else objetivos <- c(objetivos, l)
}
objetivos <- unique(objetivos)
if (length(objetivos) == 0) {                               # fallback (mock con solo semillas !/>/+)
  objetivos <- c(sprintf("@fanpatriota%04d", 1:60), sprintf("@ciudadano_real_%d", 1:200))  # minoría bodega + mayoría reales
}
if (length(objetivos) > MAX) { cat("Limitando a", MAX, "cuentas (de", length(objetivos), ")\n"); objetivos <- objetivos[1:MAX] }
cat("Investigando", length(objetivos), "cuentas | fuente:", fuente, "\n")

res <- lapply(objetivos, function(h) tryCatch(auditar_handle(h, fuente = fuente), error = function(e) NULL))
res <- res[!vapply(res, is.null, logical(1))]
names(res) <- vapply(res, function(x) x$handle, character(1))

# ---- clasificación NUEVA: BODEGA por COORDINACIÓN + FUENTE (no metadata) + POSTURA con IA ----
USAR_IA <- Sys.getenv("OPENAI_API_KEY") != ""
cat("Postura con IA:", USAR_IA, "\n")
# análisis de GRUPO: junta los tweets de todas las cuentas y mide coordinación / fuente / cohortes
grp <- do.call(rbind, lapply(res, function(r) r$tweets_muestra))
creacion <- setNames(vapply(res, function(r) r$cuenta_creada %||% NA_character_, character(1)),
                     vapply(res, function(r) r$handle, character(1)))
bodres <- if (!is.null(grp) && nrow(grp) > 0) analizar_bodega(grp, creacion) else list(cuentas=data.frame(), clusters=data.frame())
bod <- bodres$cuentas; clusters <- bodres$clusters
bmap <- if (nrow(bod)) split(bod, bod$handle) else list()
cat("Bodega -> coordinadas:", sum(bod$coordinada %||% FALSE), "| automatizadas:", sum(bod$automatizada %||% FALSE),
    "| clústeres de consigna:", nrow(clusters), "\n")
res <- lapply(res, function(r) {
  b <- bmap[[r$handle]]
  if (!is.null(b)) { r$banda <- b$banda[1]; r$bodega <- isTRUE(b$bodega[1])
    r$share_auto <- b$share_auto[1]; r$comparten <- b$comparten[1]; r$en_cohorte <- isTRUE(b$en_cohorte[1]) }
  else { r$banda <- "Sin señales de coordinación"; r$bodega <- FALSE; r$share_auto <- 0; r$comparten <- 0L; r$en_cohorte <- FALSE }
  st <- if (USAR_IA) tryCatch(clasificar_postura_llm(r$textos), error=function(e) NA_character_) else NA_character_
  r$stance <- if (is.na(st %||% NA) || !nzchar(st %||% "")) "indeterminado" else st
  r
})

df2list <- function(d) if (is.null(d) || nrow(d) == 0) list() else lapply(seq_len(nrow(d)), function(i) as.list(d[i, , drop = FALSE]))
# SOLO publicamos las cuentas de BODEGA que atacan al Pacto (NO los reales, NO los de apoyo)
res_pub <- Filter(function(r) isTRUE(r$bodega) && !identical(r$stance, "apoya_pacto"), res)
perfiles <- lapply(res_pub, function(r) list(
  handle = r$handle, banda = r$banda, stance = r$stance, bodega = r$bodega, pct = 100,
  share_auto = r$share_auto, comparten = r$comparten, en_cohorte = r$en_cohorte,
  edad_dias = round(r$detalle$edad_dias[1]), tweets_dia = r$detalle$tweets_por_dia[1],
  reply_share = if ("reply_share" %in% names(r$detalle)) r$detalle$reply_share[1] else NA,
  followers = r$detalle$followers[1],
  ataca = df2list(r$respuestas), amplifica = df2list(r$top), repetidos = df2list(r$repetidos)))

# ---- CONCLUSIONES: lo que ENCONTRAMOS (las consignas + quién las postea) ----
victimas <- consolidar_amplificadores(lapply(res_pub, function(r) list(top = r$respuestas)))
con <- tryCatch(read.csv("data/cuentas_conocidas.csv", stringsAsFactors=FALSE, encoding="UTF-8"), error=function(e) NULL)
der <- if (!is.null(con)) tolower(paste0("@", gsub("^@","",con$handle[con$bando=="derecha"]))) else character(0)
pac <- if (!is.null(con)) tolower(paste0("@", gsub("^@","",con$handle[con$bando=="pacto"]))) else character(0)
puente <- vapply(res_pub, function(r) {
  atk <- tolower(if (nrow(r$respuestas)) r$respuestas$cuenta else character(0))
  amp <- tolower(if (nrow(r$top)) r$top$cuenta else character(0))
  any(atk %in% pac) && any(amp %in% der) }, logical(1))
n_coord <- sum(vapply(res_pub, function(r) isTRUE(grepl("coordinado", r$banda)), logical(1)))
n_auto  <- sum(vapply(res_pub, function(r) isTRUE(grepl("fuente", r$banda)), logical(1)))
conclusiones <- list(
  n_total = length(res_pub), n_analizadas = length(res), n_alta = length(res_pub),
  pct_alta = if (length(res)) round(100*length(res_pub)/length(res)) else 0,
  n_coordinada = n_coord, n_automatizada = n_auto, n_clusters = nrow(clusters),
  clusters = df2list(head(clusters, 15)),                 # las CONSIGNAS + las cuentas que las postean
  top_victimas = df2list(head(victimas, 6)),
  conexion = list(n_puente = sum(puente), pct = if (length(res_pub)) round(100*sum(puente)/length(res_pub)) else 0)
)

narr <- consolidar_narrativa(res_pub)
# red del enjambre: cada bodega -> sus 2 objetivos principales (mapa limpio, no cientos de nodos)
invs_red <- lapply(res_pub, function(r) list(handle = r$handle,
  top = if (!is.null(r$respuestas) && nrow(r$respuestas)) utils::head(r$respuestas, 2) else r$top))
red <- construir_red(invs_red)
if (nrow(red$nodes) > 200) {
  keep <- head(red$nodes$id[order(-red$nodes$grado)], 200)
  red$edges <- red$edges[red$edges$from %in% keep & red$edges$to %in% keep, ]
  red$nodes <- red$nodes[red$nodes$id %in% keep, ]
}
out <- list(generado = as.character(Sys.time()), fuente = fuente, conclusiones = conclusiones,
  perfiles = unname(perfiles),
  narrativa = list(palabras = df2list(narr$palabras), hashtags = df2list(narr$hashtags)),
  red = list(nodes = df2list(red$nodes), edges = df2list(red$edges)))
dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
tab <- if (length(res_pub)) do.call(rbind, lapply(res_pub, function(r) data.frame(handle = r$handle,
  clasificacion = r$banda, comparte_con = r$comparten, share_auto = r$share_auto,
  edad_dias = round(r$detalle$edad_dias[1]), followers = r$detalle$followers[1],
  ataca_a = paste(if (nrow(r$respuestas)) r$respuestas$cuenta else character(0), collapse = "; "),
  stringsAsFactors = FALSE))) else data.frame(info = "Sin bodega detectada")
write.csv(tab, "docs/data/investigacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo:", length(res_pub), "cuentas de BODEGA publicadas |", nrow(clusters), "consignas |",
    nrow(red$nodes), "nodos\n")
