# finalizar.R — Construye la lista HONESTA y publicable:
#   incluir SOLO cuentas que copia-pegan su PROPIO mensaje DISTINTIVO (largo/específico) >= UMBRAL veces.
#   EXCLUIR lemas genéricos ("por la razon o por la fuerza", "pacto hamponico") y falsos positivos.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
UMBRAL <- 8

red  <- readRDS("C:/Users/LENOVO/AppData/Local/Temp/red_verif.rds")
fore <- readRDS("C:/Users/LENOVO/AppData/Local/Temp/forense.rds")

# --- 1) distribución por cuenta de los fragmentos DISTINTIVOS (no genéricos) ---
distintivos <- c("ESPECÍFICO 'lumpen comunista'", "ESPECÍFICO '18.000 niños... camarillas'",
                 "JAVIER 'sangre y fuego malditos'")
cat("=== Distribución por cuenta (solo fragmentos distintivos) ===\n")
conteo <- list(); texto <- list()
for (nm in distintivos) {
  df <- red[[nm]]; if (is.null(df) || !nrow(df)) next
  tb <- sort(table(df$handle[nzchar(df$handle)]), decreasing = TRUE)
  cat("\n", nm, ":\n", sep = "")
  for (h in names(tb)) {
    cat(sprintf("   @%-18s %d posts\n", h, tb[[h]]))
    if (is.null(conteo[[h]]) || tb[[h]] > conteo[[h]]) {
      conteo[[h]] <- as.integer(tb[[h]])
      texto[[h]]  <- df$text[df$handle == h][1]
    }
  }
}
# Javier: medición directa de su timeline (forense, 40/40), más fiable que la búsqueda puntual
if (!is.null(fore[["JavierV82485933"]])) {
  jv <- fore[["JavierV82485933"]]
  h <- "javierv82485933"
  if (is.null(conteo[[h]]) || jv$rep_n > conteo[[h]]) { conteo[[h]] <- as.integer(jv$rep_n); texto[[h]] <- jv$rep_tx }
}

# --- 2) filtrar por umbral y armar perfiles ---
cuentas <- names(conteo)[vapply(names(conteo), function(h) conteo[[h]] >= UMBRAL, logical(1))]
ord <- cuentas[order(-vapply(cuentas, function(h) conteo[[h]], numeric(1)))]
perfiles <- unname(lapply(ord, function(h) list(
  handle  = h,
  veces   = conteo[[h]],
  mensaje = trimws(gsub("\\s+", " ", texto[[h]] %||% "")),
  fuerte  = conteo[[h]] >= 20)))

cat("\n=== LISTA FINAL (>= ", UMBRAL, " repeticiones de mensaje distintivo) ===\n", sep = "")
for (p in perfiles) cat(sprintf("   @%-18s %2d×  %s\n", p$handle, p$veces, substr(p$mensaje, 1, 70)))

out <- list(generado = as.character(Sys.time()), fuente = "twitterapi_io", tipo = "spam_individual",
  metodo = "Cuentas que copia-pegan su PROPIO mensaje específico (no lemas comunes) muchas veces. Verificado con búsqueda exacta del fragmento distintivo; descartados eslóganes genéricos y falsos positivos.",
  conclusiones = list(n_total = length(perfiles),
    n_fuertes = sum(vapply(perfiles, function(p) isTRUE(p$fuerte), logical(1))),
    max_veces = if (length(perfiles)) perfiles[[1]]$veces else 0),
  perfiles = perfiles)
write_json(out, "docs/data/investigacion.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
csv <- do.call(rbind, lapply(perfiles, function(p) data.frame(cuenta = paste0("@", p$handle),
  repeticiones = p$veces, mensaje = p$mensaje, stringsAsFactors = FALSE)))
if (!is.null(csv)) write.csv(csv, "docs/data/spam_bots.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\nPublicado: %d cuentas | %d fuertes | max %d\n", length(perfiles),
    out$conclusiones$n_fuertes, out$conclusiones$max_veces))
