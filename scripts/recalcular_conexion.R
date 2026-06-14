# recalcular_conexion.R — re-caracteriza y recalcula la conexión sobre el JSON YA generado,
# sin volver a llamar la API (gratis). Útil cuando corregimos cuentas_conocidas.csv.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

d <- fromJSON("docs/data/investigacion.json", simplifyVector = FALSE)
con <- read.csv("data/cuentas_conocidas.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
con$key <- tolower(paste0("@", gsub("^@", "", trimws(con$handle))))
lk <- function(c){ i <- match(tolower(c), con$key)
  list(nombre = if (is.na(i)) NA else con$nombre[i], bando = if (is.na(i)) NA else con$bando[i]) }
der <- con$key[con$bando == "derecha"]; pac <- con$key[con$bando == "pacto"]
retag <- function(items){ if (is.null(items) || !length(items)) return(items)
  lapply(items, function(o){ r <- lk(o$cuenta); o$nombre <- r$nombre; o$bando <- r$bando; o }) }

d$perfiles <- lapply(d$perfiles, function(p){ p$ataca <- retag(p$ataca); p$amplifica <- retag(p$amplifica); p })
puente <- vapply(d$perfiles, function(p){
  atk <- tolower(vapply(p$ataca,     function(o) o$cuenta %||% "", character(1)))
  amp <- tolower(vapply(p$amplifica, function(o) o$cuenta %||% "", character(1)))
  any(atk %in% pac) && any(amp %in% der) }, logical(1))
d$conclusiones$conexion <- list(n_puente = sum(puente),
  pct = round(100 * sum(puente) / length(d$perfiles)))
d$red$nodes <- lapply(d$red$nodes, function(n){ r <- lk(n$id); if (!is.na(r$bando)) n$bando <- r$bando; n })
d$conclusiones$top_victimas     <- retag(d$conclusiones$top_victimas)
d$conclusiones$top_amplificados <- retag(d$conclusiones$top_amplificados)

write_json(d, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Recalculado. Conexión:", d$conclusiones$conexion$n_puente,
    "(", d$conclusiones$conexion$pct, "%) atacan al Pacto Y amplifican a la derecha\n")
