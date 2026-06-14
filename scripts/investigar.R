# investigar.R — Corre la investigación sobre una lista de cuentas y exporta la data
# para el sitio estático (GitHub Pages). NO necesita servidor: corres esto local con tu
# token, haces git push, y el sitio en docs/ se actualiza solo.
#
# Uso:
#   1) Pon las cuentas a investigar en data/objetivos.txt (una por línea, con o sin @).
#   2) Para datos REALES:  setear TWITTERAPI_IO_KEY y USAR_REAL=1  (si no, usa demo/mock).
#   3) Rscript scripts/investigar.R
#   4) git add docs && git commit && git push   -> el sitio público se actualiza.

setwd_here <- function() if (basename(getwd()) != "auditoria-autenticidad" &&
  dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
setwd_here()
for (f in c("features.R","score.R","coordination.R","connectors.R","llm.R","ondemand.R","colectivo.R","pipeline.R"))
  source(file.path("R", f))
suppressPackageStartupMessages(library(jsonlite))

fuente <- if (Sys.getenv("USAR_REAL") == "1" && Sys.getenv("TWITTERAPI_IO_KEY") != "") "twitterapi_io" else "mock"
objetivos <- if (file.exists("data/objetivos.txt"))
  trimws(readLines("data/objetivos.txt", warn = FALSE)) else
  c("@vozpatriota7777","@user88231","@maria_lopez","@fanderecha9012","@cuentareal_juan")
objetivos <- objetivos[nchar(objetivos) > 0]
cat("Investigando", length(objetivos), "cuentas con fuente:", fuente, "\n")

res <- lapply(objetivos, function(h) {
  cat(" -", h, "\n")
  tryCatch(auditar_handle(h, fuente = fuente), error = function(e) NULL)
})
res <- res[!vapply(res, is.null, logical(1))]
names(res) <- vapply(res, function(x) x$handle, character(1))

df2list <- function(d) if (is.null(d) || nrow(d) == 0) list() else
  lapply(seq_len(nrow(d)), function(i) as.list(d[i, , drop = FALSE]))

perfiles <- lapply(res, function(r) list(
  handle      = r$handle,
  pct         = r$pct,
  banda       = r$banda,
  n_flags     = r$n_flags,
  edad_dias   = round(r$detalle$edad_dias[1]),
  tweets_dia  = r$detalle$tweets_por_dia[1],
  reply_share = if ("reply_share" %in% names(r$detalle)) r$detalle$reply_share[1] else NA,
  followers   = r$detalle$followers[1],
  fuente      = r$fuente,
  senales     = as.list(r$senales),
  ataca       = df2list(r$respuestas),
  amplifica   = df2list(r$top),
  repetidos   = df2list(r$repetidos)
))

narr <- consolidar_narrativa(res)
red  <- construir_red(res)
out <- list(
  generado  = as.character(Sys.time()),
  fuente    = fuente,
  n_perfiles = length(perfiles),
  perfiles  = unname(perfiles),
  narrativa = list(palabras = df2list(narr$palabras), hashtags = df2list(narr$hashtags)),
  red       = list(nodes = df2list(red$nodes), edges = df2list(red$edges))
)

dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")

# CSV plano para descargar
tab <- do.call(rbind, lapply(res, function(r) data.frame(
  handle = r$handle, pct = r$pct, clasificacion = r$banda, senales = r$n_flags,
  edad_dias = round(r$detalle$edad_dias[1]), tweets_dia = r$detalle$tweets_por_dia[1],
  reply_share = if ("reply_share" %in% names(r$detalle)) r$detalle$reply_share[1] else NA,
  followers = r$detalle$followers[1],
  ataca_a = paste(if (nrow(r$respuestas)) r$respuestas$cuenta else character(0), collapse = "; "),
  amplifica = paste(if (nrow(r$top)) r$top$cuenta else character(0), collapse = "; "),
  stringsAsFactors = FALSE)))
write.csv(tab, "docs/data/investigacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo: docs/data/investigacion.json y .csv (", length(perfiles), "perfiles )\n")
