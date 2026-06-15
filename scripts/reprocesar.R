# reprocesar.R — Reprocesa una investigación YA generada (sin gastar API) al foco nuevo:
#   publica SOLO las cuentas de bodega (coordinadas/automatizadas) + extrae las CONSIGNAS
#   (texto compartido + cuentas que lo postean) a partir de sus 'repetidos'.
# Uso: Rscript scripts/reprocesar.R <ruta_json_origen>
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
for (f in c("features.R","score.R","coordination.R","colectivo.R")) source(file.path("R", f))
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a,b) if (is.null(a)||length(a)==0) b else a
args <- commandArgs(trailingOnly = TRUE)
origen <- if (length(args)) args[1] else "C:/Users/LENOVO/AppData/Local/Temp/real_v4.json"

d <- fromJSON(origen, simplifyVector = FALSE)
perf <- d$perfiles
es_bodega <- function(p) grepl("coordinado|fuente", p$banda %||% "")
no_apoyo  <- function(p) !identical(p$stance %||% "", "apoya_pacto")
bodega <- Filter(function(p) es_bodega(p) && no_apoyo(p), perf)
cat("Cuentas de bodega:", length(bodega), "(de", length(perf), ")\n")

# CONSIGNAS: agrupar cuentas-bodega por el texto normalizado que repiten
m <- list(); ej <- list()
for (p in bodega) for (r in (p$repetidos %||% list())) {
  t <- normalizar_texto(r$texto %||% ""); if (nchar(t) < 18) next
  m[[t]] <- unique(c(m[[t]] %||% character(0), p$handle)); if (is.null(ej[[t]])) ej[[t]] <- r$texto
}
cl <- Filter(function(t) length(m[[t]]) >= 2, names(m))
clusters <- if (length(cl)) lapply(cl[order(-vapply(cl, function(t) length(m[[t]]), 1L))], function(t)
  list(ejemplo = ej[[t]], n_cuentas = length(m[[t]]), cuentas = paste0("@", m[[t]], collapse = ", "))) else list()

# fallback: la coordinación se midió cruzando tweets (no guardados). Si no salieron clusters pero hay
# cuentas que comparten texto (comparten>=4), las agrupamos por ese conteo y tomamos una consigna de ejemplo.
if (length(clusters) == 0 && length(bodega) >= 2) {
  por_n <- split(bodega, vapply(bodega, function(p) p$comparten %||% 0L, integer(1)))
  clusters <- lapply(names(por_n), function(k) {
    g <- por_n[[k]]; if (length(g) < 2) return(NULL)
    ejx <- NA; for (p in g) { rr <- p$repetidos %||% list(); if (length(rr)) { ejx <- rr[[1]]$texto; break } }
    list(ejemplo = ejx %||% "(texto coordinado — ver cuentas)",
         n_cuentas = length(g), cuentas = paste0("@", vapply(g, function(p) p$handle, character(1)), collapse = ", "))
  })
  clusters <- Filter(Negate(is.null), clusters)
}
cat("Consignas:", length(clusters), "\n")

pub <- lapply(bodega, function(p) p)   # perfiles bodega tal cual (ya traen los campos que usa el sitio)
df2list <- function(x) x
g1 <- function(x) { x <- unlist(x); if (length(x)) x[[1]] else NA }   # desenvuelve arrays de 1

# RED solo de las cuentas BODEGA -> sus objetivos (el enjambre real, no las 161)
invs <- lapply(bodega, function(p) {
  a <- utils::head(p$ataca %||% list(), 2)        # solo los 2 objetivos principales (enjambre limpio)
  td <- if (length(a)) do.call(rbind, lapply(a, function(o) data.frame(
    cuenta = g1(o$cuenta), n = suppressWarnings(as.integer(g1(o$n))),
    bando = g1(o$bando), nombre = g1(o$nombre), stringsAsFactors = FALSE))) else
    data.frame(cuenta=character(), n=integer(), bando=character(), nombre=character())
  list(handle = p$handle, top = td)
})
red <- construir_red(invs)
cat("Red del enjambre:", nrow(red$nodes), "nodos (11 bodega + sus objetivos)\n")
# víctimas: a quién atacan las coordinadas
victimas <- consolidar_amplificadores(invs)
out <- list(generado = as.character(Sys.time()), fuente = d$fuente,
  conclusiones = list(
    n_total = length(bodega), n_analizadas = length(perf), n_alta = length(bodega),
    n_clusters = length(clusters), clusters = clusters,
    top_victimas = df2list(utils::head(victimas, 6)), conexion = d$conclusiones$conexion),
  perfiles = pub,
  narrativa = d$narrativa, red = list(nodes = red$nodes, edges = red$edges))
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Publicado:", length(bodega), "cuentas de bodega |", length(clusters), "consignas\n")
if (length(clusters)) for (i in seq_len(min(3, length(clusters))))
  cat(" -", clusters[[i]]$n_cuentas, "cuentas:", substr(clusters[[i]]$ejemplo, 1, 70), "\n")
