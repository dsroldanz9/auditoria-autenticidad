# scan_spam.R [target] — Escanea cuentas que responden a un objetivo y marca las que SPAMEAN
# (repiten SU propio mensaje N+ veces, como @JavierV82485933). Rápido, lote chico, sin cuelgues.
# Uso: MAX_CUENTAS=40 MAX_PAGES=5 Rscript scripts/scan_spam.R petrogustavo
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
for (f in c("features.R","score.R","coordination.R","connectors.R","ondemand.R")) source(file.path("R", f))
suppressPackageStartupMessages(library(httr2))
target <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(target)) target <- "petrogustavo"
target <- gsub("^@","",target)
N <- suppressWarnings(as.integer(Sys.getenv("MAX_CUENTAS"))); if (is.na(N)) N <- 40
UMBRAL <- suppressWarnings(as.integer(Sys.getenv("UMBRAL_SPAM"))); if (is.na(UMBRAL)) UMBRAL <- 8

# cuentas que le responden a @target (candidatas)
buscar_to <- function(t, max_pages = 5) {
  acc <- character(0); cursor <- ""
  for (pg in seq_len(max_pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0("to:", t), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = Sys.getenv("TWITTERAPI_IO_KEY")) |> req_timeout(25) |> req_perform(),
      error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list(); if (length(arr) == 0) break
    acc <- c(acc, vapply(arr, function(x) x$author$userName %||% NA_character_, character(1)))
    if (!isTRUE(d$has_next_page) || identical(d$next_cursor, "")) break; cursor <- d$next_cursor
  }
  unique(acc[!is.na(acc) & nzchar(acc)])
}
cands <- utils::head(buscar_to(target), N)
cat("Escaneando", length(cands), "cuentas que le responden a @", target, " (umbral spam:", UMBRAL, "+ repeticiones)\n\n", sep="")

rds <- "C:/Users/LENOVO/AppData/Local/Temp/bots_spam.rds"
bots <- if (file.exists(rds)) readRDS(rds) else list()   # ACUMULA entre corridas
for (i in seq_along(cands)) {
  acc <- cands[i]
  tw <- tryCatch(ta_io_buscar_tweets(acc), error = function(e) NULL)
  if (!is.null(tw) && nrow(tw) > 0) {
    rep <- mensajes_repetidos(tw, n = 1)
    if (nrow(rep) > 0 && rep$veces[1] >= UMBRAL) {
      pr <- round(100*mean(as.logical(tw$es_respuesta), na.rm=TRUE))
      cat(sprintf("🤖 @%-18s repite %2d veces (%d%% respuestas): \"%s\"\n",
                  acc, rep$veces[1], pr, substr(gsub("\\s+"," ",rep$texto[1]), 1, 55)))
      bots[[acc]] <- list(handle = acc, veces = rep$veces[1], msg = rep$texto[1])
    }
  }
  if (i %% 10 == 0) cat("  ...", i, "/", length(cands), "revisadas\n")
}
saveRDS(bots, rds)
cat("\n=== Total ACUMULADO de cuentas-spam:", length(bots), "===\n")
