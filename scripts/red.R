# red.R — VERIFICA una red sin caer en "frase genérica".
# Regla: es RED solo si un fragmento LARGO y específico (no un lema común) se comparte verbatim
# por muchas cuentas distintas. Probamos fragmentos genéricos vs específicos y comparamos.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
source("R/ondemand.R")
key <- Sys.getenv("TWITTERAPI_IO_KEY"); if (key == "") stop("Falta TWITTERAPI_IO_KEY")

# Devuelve data.frame(handle, text) de TODAS las cuentas que postean la frase exacta.
autores_de <- function(frase, key, pages = 8) {
  rows <- list(); cursor <- ""
  for (pg in seq_len(pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0('"', frase, '"'), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = key) |> req_timeout(25) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    for (t in arr) rows[[length(rows) + 1]] <- data.frame(
      handle = tolower(t$author$userName %||% ""), text = t$text %||% "",
      creada = t$author$createdAt %||% NA, seg = t$author$followers %||% NA, stringsAsFactors = FALSE)
    if (!isTRUE(d$has_next_page) || is.null(d$next_cursor) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

frases <- list(
  "GENÉRICO  'por la razon o por la fuerza'"     = "por la razon o por la fuerza",
  "ESPECÍFICO 'lumpen comunista'"                = "todo ese lumpen comunista",
  "ESPECÍFICO '18.000 niños... camarillas'"      = "asesinados por los camarillas",
  "JAVIER 'sangre y fuego malditos'"             = "sangre y fuego malditos con maldicion",
  "ALEJO 'pacto hamponico'"                      = "pacto hamponico")

cat("=== VERIFICACIÓN DE RED (cuentas DISTINTAS por fragmento) ===\n\n")
guardar <- list()
for (nm in names(frases)) {
  df <- autores_de(frases[[nm]], key, pages = 8)
  n_dist <- if (nrow(df)) length(unique(df$handle[nzchar(df$handle)])) else 0
  tot    <- nrow(df)
  cat(sprintf("%-42s -> %3d cuentas distintas (%d posts)  %s\n", nm, n_dist, tot,
      if (n_dist >= 8) "<< RED COORDINADA" else if (n_dist <= 2) "<< casi nadie (no es red)" else "<< dudoso"))
  if (nrow(df)) {
    u <- unique(df$handle[nzchar(df$handle)])
    cat("   ", paste0("@", head(u, 18), collapse = "  "), if (length(u) > 18) sprintf("  …(+%d)", length(u) - 18) else "", "\n\n")
  } else cat("\n")
  guardar[[nm]] <- df
}
saveRDS(guardar, "C:/Users/LENOVO/AppData/Local/Temp/red_verif.rds")
cat("Guardado: red_verif.rds\n")
