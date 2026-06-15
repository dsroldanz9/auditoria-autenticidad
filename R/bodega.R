# bodega.R — Detección de bodega/automatización por COORDINACIÓN, no por metadata.
# Un bot no se delata por ser una cuenta individual "rara" (handle con números, sin bio,
# muy activa) — eso es un humano apasionado. Se delata por COMPORTAMIENTO COLECTIVO:
#  (1) postea TEXTO IDÉNTICO al de muchas OTRAS cuentas (consigna calcada),
#  (2) publica vía una FUENTE automatizada (no la app de Twitter),
#  (3) fue creada el mismo día que muchas otras (cohorte/granja).
# Requiere los tweets de TODAS las cuentas juntos (análisis de grupo).

suppressPackageStartupMessages({ library(dplyr) })

# clientes humanos típicos (lo demás = automatización/herramienta)
CLIENTES_HUMANOS <- c("twitter for iphone","twitter for android","twitter web app",
  "twitter for ipad","twitter for mac","x for iphone","x for android","x web app","tweetdeck")

#' @param tweets data.frame del GRUPO: handle, created_at, text, source
#' @param creacion named vector handle -> fecha de creación de la cuenta (Date/POSIXct/texto)
#' @param min_comparten nº de cuentas distintas que deben postear el mismo texto para ser "coordinado"
#' @param umbral_auto fracción de tweets vía fuente automatizada para marcar "automatizada"
#' @return data.frame por cuenta: coordinada, automatizada, en_cohorte, bodega, banda, métricas.
analizar_bodega <- function(tweets, creacion = NULL, min_comparten = 4, umbral_auto = 0.6) {
  if (is.null(tweets) || nrow(tweets) == 0) return(data.frame())
  tw <- tweets
  tw$tn <- normalizar_texto(tw$text)
  # umbral de longitud: solo texto LARGO específico cuenta como consigna (no frases genéricas
  # como "firmes por la patria"). Configurable con MIN_LEN_CONSIGNA (default 45).
  min_len <- suppressWarnings(as.integer(Sys.getenv("MIN_LEN_CONSIGNA"))); if (is.na(min_len)) min_len <- 45
  val <- tw[nchar(tw$tn) >= min_len, , drop = FALSE]

  # (1) coordinación: cuántas cuentas DISTINTAS postean cada texto
  comparten <- if (nrow(val)) tapply(val$handle, val$tn, function(h) length(unique(h))) else integer(0)
  textos_coord <- names(comparten)[comparten >= min_comparten]
  handles <- unique(tw$handle)
  co_por_cuenta <- function(h) {
    s <- val$tn[val$handle == h]; if (!length(s)) return(0L)
    cc <- comparten[s[s %in% textos_coord]]; if (!length(cc)) 0L else max(cc)
  }
  out <- data.frame(handle = handles, stringsAsFactors = FALSE)
  out$comparten <- vapply(handles, co_por_cuenta, integer(1))   # con cuántas cuentas comparte texto

  # (2) fuente automatizada: % de tweets de la cuenta vía cliente NO humano
  src <- tapply(tolower(tw$source %||% rep("", nrow(tw))), tw$handle, function(s) {
    s <- s[nzchar(s)]; if (!length(s)) return(NA_real_); mean(!(s %in% CLIENTES_HUMANOS))
  })
  out$share_auto <- round(vapply(handles, function(h) { v <- src[[h]]; if (is.null(v) || is.na(v)) 0 else v }, numeric(1)), 2)

  # (3) cohorte de creación: creada el mismo día que >=5 otras del grupo
  out$en_cohorte <- FALSE
  if (!is.null(creacion)) {
    dias <- as.Date(suppressWarnings(parse_fecha(creacion[handles])))
    tb <- table(dias); cohortes <- names(tb)[tb >= 5]
    out$en_cohorte <- !is.na(dias) & as.character(dias) %in% cohortes
  }

  out$coordinada   <- out$comparten >= min_comparten
  out$automatizada <- out$share_auto >= umbral_auto
  out$bodega       <- out$coordinada | out$automatizada      # cohorte sola NO basta (ruidosa); solo contexto
  out$banda <- ifelse(out$coordinada,   "Bodega — texto coordinado",
                ifelse(out$automatizada, "Automatizada — fuente", "Sin señales de coordinación"))

  # CLÚSTERES DE CONSIGNA: cada texto coordinado + un ejemplo original + las cuentas que lo postean
  cl <- if (length(textos_coord)) do.call(rbind, lapply(textos_coord, function(t) {
    idx <- which(val$tn == t); cuentas <- unique(val$handle[idx])
    data.frame(ejemplo = val$text[idx[1]], n_cuentas = length(cuentas),
               cuentas = paste0("@", cuentas, collapse = ", "), stringsAsFactors = FALSE)
  })) else data.frame()
  if (nrow(cl)) cl <- cl[order(-cl$n_cuentas), ]
  list(cuentas = out, clusters = cl)
}
