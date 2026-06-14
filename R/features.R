# features.R — Extracción de señales por cuenta
# Todas las señales son OBSERVABLES y TRANSPARENTES. Ninguna "prueba" que algo
# sea un bot; son indicadores que, combinados, elevan la probabilidad.

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(lubridate)
})

# Parsea fechas en ISO (8601) Y en formato Twitter clásico ("Tue Dec 10 07:00:30 +0000 2024"),
# que es el que devuelve twitterapi.io / la X API v1.1. Vital para calcular bien la edad.
# Usa lubridate (devuelve NA en vez de lanzar error con formatos no estándar).
parse_fecha <- function(x) {
  ts <- suppressWarnings(parse_date_time(as.character(x),
          orders = c("a b d H:M:S z Y", "Ymd HMS", "Ymd HMSz", "Ymd", "mdY", "dmY"),
          tz = "UTC", quiet = TRUE))
  as.POSIXct(ts)
}

# Entropía de Shannon de un string (un handle aleatorio "x8f3kq92" tiene alta entropía)
entropia_shannon <- function(s) {
  s <- tolower(gsub("[^a-z0-9]", "", s))
  if (nchar(s) == 0) return(0)
  p <- table(strsplit(s, "")[[1]]) / nchar(s)
  -sum(p * log2(p))
}

# Proporción de dígitos en el handle (los bots suelen ser "nombre12345678")
ratio_digitos <- function(s) {
  s <- gsub("@", "", s)
  if (nchar(s) == 0) return(0)
  str_count(s, "[0-9]") / nchar(s)
}

#' Extrae features a nivel cuenta a partir de una tabla de cuentas.
#'
#' @param cuentas data.frame con columnas mínimas:
#'   handle, created_at (Date/POSIXct o texto ISO), followers, following, n_tweets
#'   Opcionales: display_name, bio, default_avatar (lógico), default_profile (lógico),
#'   verified (lógico), location
#' @param ref_fecha fecha de referencia para calcular la edad (por defecto Sys.time()).
#' @return data.frame con una fila por cuenta y columnas de features.
extraer_features_cuenta <- function(cuentas, ref_fecha = Sys.time()) {
  stopifnot(all(c("handle","created_at","followers","following","n_tweets") %in% names(cuentas)))
  c0 <- cuentas
  ca <- parse_fecha(c0$created_at)
  edad_dias <- as.numeric(difftime(ref_fecha, ca, units = "days"))
  edad_dias[is.na(edad_dias) | edad_dias < 1] <- 1

  has <- function(col) col %in% names(c0)
  bio        <- if (has("bio")) c0$bio else NA_character_
  dname      <- if (has("display_name")) c0$display_name else c0$handle
  def_avatar <- if (has("default_avatar"))  as.logical(c0$default_avatar)  else NA
  def_prof   <- if (has("default_profile")) as.logical(c0$default_profile) else NA
  verif      <- if (has("verified"))        as.logical(c0$verified)        else FALSE

  out <- tibble(
    handle           = c0$handle,
    edad_dias        = round(edad_dias, 1),
    followers        = as.numeric(c0$followers),
    following        = as.numeric(c0$following),
    n_tweets         = as.numeric(c0$n_tweets),
    # tasa de actividad: tweets por día de vida de la cuenta
    tweets_por_dia   = round(as.numeric(c0$n_tweets) / edad_dias, 2),
    # cuentas que siguen a muchos y casi nadie las sigue
    ff_ratio         = round(as.numeric(c0$following) / (as.numeric(c0$followers) + 1), 2),
    # handle aleatorio
    handle_entropia  = round(vapply(c0$handle, entropia_shannon, numeric(1)), 2),
    handle_ratio_dig = round(vapply(c0$handle, ratio_digitos, numeric(1)), 2),
    bio_vacia        = is.na(bio) | nchar(trimws(ifelse(is.na(bio), "", bio))) == 0,
    avatar_default   = ifelse(is.na(def_avatar), FALSE, def_avatar),
    perfil_default   = ifelse(is.na(def_prof), FALSE, def_prof),
    verificada       = ifelse(is.na(verif), FALSE, verif)
  )
  out
}

#' Agrega features derivadas de los tweets (si se dispone de la tabla de tweets).
#' @param tweets data.frame: handle, created_at, text (opcional is_retweet, source)
#' @return data.frame: handle + features de comportamiento de publicación.
extraer_features_tweets <- function(tweets) {
  if (is.null(tweets) || nrow(tweets) == 0) return(NULL)
  stopifnot(all(c("handle","created_at","text") %in% names(tweets)))
  tw <- tweets
  tw$ts <- parse_fecha(tw$created_at)
  tw$hora <- lubridate::hour(tw$ts)
  tw$text_norm <- tolower(trimws(gsub("\\s+", " ", gsub("https?://\\S+", "", tw$text))))
  es_rt <- if ("is_retweet" %in% names(tw)) as.logical(tw$is_retweet) else grepl("^rt @", tw$text_norm)
  # respuesta: campo es_respuesta del conector, o heurística (empieza con @)
  es_resp <- if ("es_respuesta" %in% names(tw)) as.logical(tw$es_respuesta) else grepl("^@\\w", tw$text_norm)

  tw %>%
    mutate(es_rt = es_rt, es_resp = es_resp) %>%
    group_by(handle) %>%
    summarise(
      n_obs            = n(),
      share_retweets   = round(mean(es_rt, na.rm = TRUE), 3),
      # proporción de publicaciones que son RESPUESTAS a otros (cuentas torpedo comentan/atacan)
      reply_share      = round(mean(es_resp, na.rm = TRUE), 3),
      # proporción de tweets cuyo texto está duplicado dentro de la misma cuenta
      ratio_duplicados = round(1 - dplyr::n_distinct(text_norm) / n(), 3),
      # nº de horas distintas del día con actividad (un humano no postea 24/24 parejo)
      horas_activas    = dplyr::n_distinct(hora),
      .groups = "drop"
    )
}
