# profundizar.R — Deep-dive en las cuentas YA halladas: baja más tweets y lista TODOS los
# textos que comparten entre ellas (para distinguir consignas de ataque específicas de
# frases genéricas como "Firmes por la patria"). Solo analiza, no publica.
# Uso: MAX_PAGES=5 Rscript scripts/profundizar.R
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
for (f in c("features.R","score.R","coordination.R","connectors.R","llm.R","ondemand.R")) source(file.path("R", f))
suppressPackageStartupMessages(library(jsonlite))

d <- fromJSON("docs/data/investigacion.json", simplifyVector = FALSE)
handles <- vapply(d$perfiles, function(p) p$handle, character(1))
cat("Profundizando en", length(handles), "cuentas conocidas (MAX_PAGES=",
    Sys.getenv("MAX_PAGES"), ")...\n")

all_tw <- list()
for (h in handles) {
  tw <- tryCatch(ta_io_buscar_tweets(h), error = function(e) NULL)
  if (!is.null(tw) && nrow(tw)) all_tw[[h]] <- tw
}
tw <- do.call(rbind, all_tw)
cat("Tweets recolectados:", nrow(tw), "de", length(all_tw), "cuentas\n\n")
tw$tn <- normalizar_texto(tw$text)
val <- tw[nchar(tw$tn) >= 15, ]
comp <- tapply(val$handle, val$tn, function(x) length(unique(x)))
comp <- sort(comp[comp >= 2], decreasing = TRUE)
cat("=== TEXTOS COMPARTIDOS por >=2 cuentas (las consignas reales) ===\n")
if (!length(comp)) cat("Ninguno (no comparten texto idéntico)\n")
for (i in seq_len(min(25, length(comp)))) {
  t <- names(comp)[i]; ej <- val$text[match(t, val$tn)]
  cat(sprintf("[%2d cuentas] %s\n", comp[i], substr(gsub("\\s+"," ",ej), 1, 95)))
}
