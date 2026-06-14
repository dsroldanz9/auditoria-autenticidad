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
for (f in c("features.R","score.R","coordination.R","connectors.R","llm.R","ondemand.R","colectivo.R","pipeline.R"))
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

# ---- arma la lista de cuentas a investigar ----
lineas <- if (file.exists("data/objetivos.txt")) trimws(readLines("data/objetivos.txt", warn = FALSE)) else character(0)
lineas <- lineas[nchar(lineas) > 0]
if (length(lineas) == 0) {                                  # demo: 45 cuentas-granja + 5 humanas
  lineas <- c(sprintf("@fanpatriota%04d", 1:45), sprintf("@maria_real%d", 1:5))
}
objetivos <- character(0)
for (l in lineas) {
  if (startsWith(l, ">")) { if (REAL) { cat("Expandiendo atacantes de", l, "...\n"); objetivos <- c(objetivos, atacantes_de(l)) } }
  else objetivos <- c(objetivos, l)
}
objetivos <- unique(objetivos)
if (length(objetivos) > MAX) { cat("Limitando a", MAX, "cuentas (de", length(objetivos), ")\n"); objetivos <- objetivos[1:MAX] }
cat("Investigando", length(objetivos), "cuentas | fuente:", fuente, "\n")

res <- lapply(objetivos, function(h) tryCatch(auditar_handle(h, fuente = fuente), error = function(e) NULL))
res <- res[!vapply(res, is.null, logical(1))]
names(res) <- vapply(res, function(x) x$handle, character(1))

df2list <- function(d) if (is.null(d) || nrow(d) == 0) list() else lapply(seq_len(nrow(d)), function(i) as.list(d[i, , drop = FALSE]))
perfiles <- lapply(res, function(r) list(
  handle = r$handle, pct = r$pct, banda = r$banda, n_flags = r$n_flags,
  edad_dias = round(r$detalle$edad_dias[1]), tweets_dia = r$detalle$tweets_por_dia[1],
  reply_share = if ("reply_share" %in% names(r$detalle)) r$detalle$reply_share[1] else NA,
  followers = r$detalle$followers[1], senales = as.list(r$senales),
  ataca = df2list(r$respuestas), amplifica = df2list(r$top), repetidos = df2list(r$repetidos)))

# ---- CONCLUSIONES (titulares) ----
alta <- Filter(function(r) grepl("Alta", r$banda), res)
victimas <- consolidar_amplificadores(lapply(res, function(r) list(top = r$respuestas)))  # a quién atacan
amplif   <- consolidar_amplificadores(res)                                                # a quién amplifican
reps_all <- do.call(rbind, lapply(res, function(r) r$repetidos))
msg_top  <- if (!is.null(reps_all) && nrow(reps_all)) reps_all$texto[which.max(reps_all$veces)] else NA
conclusiones <- list(
  n_total = length(res), n_alta = length(alta),
  pct_alta = if (length(res)) round(100*length(alta)/length(res)) else 0,
  top_victimas = df2list(head(victimas, 6)),
  top_amplificados = df2list(head(amplif, 6)),
  mensaje_top = msg_top
)

narr <- consolidar_narrativa(res); red <- construir_red(res)
out <- list(generado = as.character(Sys.time()), fuente = fuente, conclusiones = conclusiones,
  perfiles = unname(perfiles),
  narrativa = list(palabras = df2list(narr$palabras), hashtags = df2list(narr$hashtags)),
  red = list(nodes = df2list(red$nodes), edges = df2list(red$edges)))
dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
tab <- do.call(rbind, lapply(res, function(r) data.frame(handle = r$handle, pct = r$pct, clasificacion = r$banda,
  senales = r$n_flags, edad_dias = round(r$detalle$edad_dias[1]), reply_share = if ("reply_share" %in% names(r$detalle)) r$detalle$reply_share[1] else NA,
  followers = r$detalle$followers[1], ataca_a = paste(if (nrow(r$respuestas)) r$respuestas$cuenta else character(0), collapse = "; "),
  stringsAsFactors = FALSE)))
write.csv(tab, "docs/data/investigacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo:", length(perfiles), "perfiles |", conclusiones$n_alta, "alta señal |",
    nrow(red$nodes), "nodos en la red\n")
