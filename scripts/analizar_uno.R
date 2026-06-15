# analizar_uno.R <@handle> — Analiza UNA cuenta a fondo y RÁPIDO (segundos, no se cuelga).
# Muestra sus mensajes más repetidos (el copia-pega de spam en respuestas) + perfil.
# Uso: MAX_PAGES=10 Rscript scripts/analizar_uno.R @cuenta
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
for (f in c("features.R","score.R","coordination.R","connectors.R","ondemand.R")) source(file.path("R", f))
h <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(h) || h == "") stop("Pasa un handle: Rscript scripts/analizar_uno.R @cuenta")

p <- fetch_perfil(h, "twitterapi_io")
cu <- p$cuenta; tw <- p$tweets
cat("=== @", gsub("^@","",h), "===\n", sep="")
if (!is.null(cu)) {
  ed <- round(as.numeric(difftime(Sys.time(), parse_fecha(cu$created_at[1]), units="days")))
  cat("seguidores:", cu$followers[1], "| siguiendo:", cu$following[1], "| tweets totales:", cu$n_tweets[1],
      "| edad:", ed, "días | bio:", ifelse(nchar(cu$bio[1])>0,"sí","(vacía)"), "\n")
}
if (is.null(tw) || nrow(tw) == 0) { cat("Sin tweets recuperados.\n"); quit() }
cat("tweets analizados:", nrow(tw), "| % respuestas:",
    round(100*mean(as.logical(tw$es_respuesta), na.rm=TRUE)), "%\n\n")

rep <- mensajes_repetidos(tw, n = 8)
cat("=== MENSAJES QUE MÁS REPITE (copia-pega) ===\n")
if (nrow(rep) == 0) cat("No repite mensajes (no parece spam).\n") else
  for (i in seq_len(nrow(rep))) cat(sprintf("[%2d veces] %s\n", rep$veces[i], substr(gsub("\\s+"," ",rep$texto[i]), 1, 100)))
cat("\n>> Si repite un mensaje 10+ veces, ESE texto lo buscamos para hallar a quién más lo calca.\n")
