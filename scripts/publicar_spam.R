# publicar_spam.R — Toma los spam-bots hallados (RDS + confirmados) y arma el JSON del sitio.
# Modelo: spam INDIVIDUAL (cuenta que repite SU propio mensaje N veces), no bodega coordinada.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
suppressPackageStartupMessages(library(jsonlite))
source("R/llm.R")
`%||%` <- function(a,b) if (is.null(a)) b else a
rds <- "C:/Users/LENOVO/AppData/Local/Temp/bots_spam.rds"
bots <- if (file.exists(rds)) readRDS(rds) else list()
extra <- list(
  JavierV82485933 = list(handle="JavierV82485933", veces=28, msg="gas esta gente comunista falsedad, montajes, sangre y fuego malditos con maldicion, sus almas"),
  IkariDavhen     = list(handle="IkariDavhen",     veces=10, msg="gas esta gente comunista falsedad, montajes, sangre y fuego malditos con maldicion, sus almas"),
  Alejo_puentesv  = list(handle="Alejo_puentesv",  veces=27, msg="pacto hamponico"))
for (k in names(extra)) if (is.null(bots[[k]])) bots[[k]] <- extra[[k]]

# umbral mínimo de credibilidad: contenido (>=10 chars) y repetido >=8
ok <- Filter(function(b) nchar(trimws(b$msg %||% "")) >= 10 && (b$veces %||% 0) >= 8, bots)

# POSTURA: solo publicamos los que ATACAN al Pacto (no los defensores que también spamean)
if (Sys.getenv("OPENAI_API_KEY") != "") {
  for (k in names(ok)) {
    s <- tryCatch(clasificar_postura_llm(ok[[k]]$msg), error = function(e) NA_character_)
    ok[[k]]$stance <- s %||% "indeterminado"
    cat(" -", k, "->", ok[[k]]$stance, "\n")
  }
  ok <- Filter(function(b) identical(b$stance, "ataca_pacto"), ok)
  cat("Tras filtro de postura (solo ataca_pacto):", length(ok), "\n")
}
ord <- order(-vapply(ok, function(b) b$veces, numeric(1)))
perfiles <- unname(lapply(ok[ord], function(b) list(
  handle = b$handle, veces = b$veces,
  mensaje = trimws(gsub("\\s+", " ", b$msg)),
  fuerte = b$veces >= 20)))

out <- list(generado = as.character(Sys.time()), fuente = "twitterapi_io", tipo = "spam_individual",
  conclusiones = list(n_total = length(perfiles),
    n_fuertes = sum(vapply(perfiles, function(p) isTRUE(p$fuerte), logical(1))),
    max_veces = if (length(perfiles)) perfiles[[1]]$veces else 0),
  perfiles = perfiles)
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
csv <- do.call(rbind, lapply(perfiles, function(p) data.frame(cuenta = paste0("@", p$handle),
  repeticiones = p$veces, mensaje = p$mensaje, stringsAsFactors = FALSE)))
write.csv(csv, "docs/data/spam_bots.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Publicado:", length(perfiles), "spam-bots |", out$conclusiones$n_fuertes, "fuertes (>=20) | max",
    out$conclusiones$max_veces, "repeticiones\n")
