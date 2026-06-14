# reclasificar.R — mejora la clasificación SOBRE el JSON ya generado (sin gastar API):
#  (1) exención por reputación: cuentas con muchos seguidores/años NO son "bots".
#  (2) postura: ataca al Pacto / lo apoya / indeterminado, por el texto de sus mensajes.
# Separa AUTOMATIZACIÓN (¿bot?) de POSTURA (¿a quién ataca?) — eran cosas distintas.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

d <- fromJSON("docs/data/investigacion.json", simplifyVector = FALSE)

# léxico de postura (contexto Colombia 2026)
ANTI_PACTO <- c("castrochav","comunis","dictadu","narcopresident","narco president","fuera petro",
  "petro corrupto","régimen","regimen","vendepatr","mermelada","expropi","venezolaniz","chavis",
  "terrorist","narcoestado","narco estado","ladrón","castro","satán","payaso petro")
PRO_PACTO <- c("narco fico","paraco","banda narco","falsos positivos","defender narco","defiende narco",
  "ñeñe","genocida","uribe preso","8000","narcoparamilitar","narcopara","abelardo narco","fico narco")

hard <- c("cuenta muy nueva","cuenta recién creada (<30 días)","sigue a muchos / pocos seguidores",
  "avatar por defecto","perfil sin personalizar")

postura <- function(p){
  txt <- tolower(paste(c(vapply(p$repetidos %||% list(), function(o) o$texto %||% "", character(1))), collapse=" "))
  if (nchar(txt) < 5) return("indeterminado")
  a <- sum(vapply(ANTI_PACTO, function(k) grepl(k, txt, fixed=TRUE), logical(1)))
  pr <- sum(vapply(PRO_PACTO, function(k) grepl(k, txt, fixed=TRUE), logical(1)))
  if (a > pr) "ataca_pacto" else if (pr > a) "apoya_pacto" else "indeterminado"
}

n_obj <- 0
d$perfiles <- lapply(d$perfiles, function(p){
  fol <- p$followers %||% 0; ed <- p$edad_dias %||% 0
  reput <- fol >= 3000 || (fol >= 1000 && ed >= 730)
  sen <- unlist(p$senales %||% list())
  nhard <- sum(sen %in% hard); nflags <- p$n_flags %||% 0
  p$stance <- postura(p)
  if (reput) { p$banda <- "Cuenta real (muy activa)"; p$pct <- min(p$pct %||% 0, 18) }
  else if (nhard >= 2 || nflags >= 4) p$banda <- "Alta señal de automatización"
  else if (nflags >= 2) p$banda <- "Sospechosa"
  else p$banda <- "Probablemente humana"
  p
})

es_alta <- function(p) identical(p$banda, "Alta señal de automatización")
alta <- Filter(es_alta, d$perfiles)
ataca <- Filter(function(p) identical(p$stance,"ataca_pacto"), d$perfiles)
objetivo <- Filter(function(p) es_alta(p) && identical(p$stance,"ataca_pacto"), d$perfiles)
n <- length(d$perfiles)
d$conclusiones$n_alta <- length(alta)
d$conclusiones$pct_alta <- round(100*length(alta)/n)
d$conclusiones$n_ataca <- length(ataca)
d$conclusiones$n_objetivo <- length(objetivo)   # atacan al Pacto Y automatizadas = el blanco real
d$conclusiones$pct_objetivo <- round(100*length(objetivo)/n)

write_json(d, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Reclasificado:", n, "cuentas\n")
cat("  Alta automatización (no reputadas):", length(alta), "\n")
cat("  Atacan al Pacto (postura):", length(ataca), "\n")
cat("  BLANCO REAL (atacan + automatizadas):", length(objetivo), "\n")
# muestra el caso del usuario
jd <- Filter(function(p) grepl("juandie", tolower(p$handle)), d$perfiles)
if (length(jd)) cat("\n@",jd[[1]]$handle," -> banda:",jd[[1]]$banda,"| stance:",jd[[1]]$stance,"| pct:",jd[[1]]$pct,"\n")
