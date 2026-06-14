# coordination.R — Detección de comportamiento inauténtico COORDINADO (CIB).
#
# Esta es la evidencia más fuerte de "apoyo inflado / pura gente paga": cuentas
# distintas publicando TEXTO IDÉNTICO en ventanas de segundos, o creadas en
# cohortes sospechosas el mismo día. No depende de adivinar si una cuenta es bot;
# mide un PATRÓN colectivo que es muy difícil de producir orgánicamente.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(lubridate) })

normalizar_texto <- function(x) {
  x <- tolower(x)
  x <- gsub("https?://\\S+", "", x)        # quitar URLs
  x <- gsub("@\\w+", "", x)                 # quitar menciones
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Detecta clústeres de co-tweet: mismo texto, muchas cuentas, ventana corta.
#' @param tweets data.frame: handle, created_at, text
#' @param ventana_seg ventana temporal en segundos (default 120).
#' @param min_cuentas mínimo de cuentas distintas para considerar coordinación.
#' @return lista con: clusters (resumen por texto) y detalle (cuentas implicadas).
detectar_cotweet <- function(tweets, ventana_seg = 120, min_cuentas = 3) {
  stopifnot(all(c("handle","created_at","text") %in% names(tweets)))
  tw <- tweets %>%
    mutate(ts = parse_fecha(created_at),
           tn = normalizar_texto(text)) %>%
    filter(nchar(tn) >= 25) %>%               # ignorar frases cortas/triviales (evita falsa coordinación)
    arrange(tn, ts)

  if (nrow(tw) == 0) return(list(clusters = tibble(), detalle = tibble()))

  # para cada texto normalizado, buscar ráfagas de >= min_cuentas en la ventana
  res <- tw %>%
    group_by(tn) %>%
    group_modify(function(g, key) {
      g <- arrange(g, ts)
      n <- nrow(g)
      mejor <- NULL
      for (i in seq_len(n)) {
        j <- which(g$ts >= g$ts[i] & g$ts <= g$ts[i] + ventana_seg)
        cuentas <- unique(g$handle[j])
        if (length(cuentas) >= min_cuentas) {
          cand <- tibble(n_cuentas = length(cuentas),
                         n_posts = length(j),
                         t_ini = min(g$ts[j]), t_fin = max(g$ts[j]),
                         span_seg = as.numeric(difftime(max(g$ts[j]), min(g$ts[j]), units="secs")),
                         cuentas = paste(cuentas, collapse=", "))
          if (is.null(mejor) || cand$n_cuentas > mejor$n_cuentas) mejor <- cand
        }
      }
      if (is.null(mejor)) tibble() else mejor
    }) %>%
    ungroup()

  if (nrow(res) == 0) return(list(clusters = tibble(), detalle = tibble()))

  clusters <- res %>%
    transmute(texto = str_trunc(tn, 80), n_cuentas, n_posts, span_seg, t_ini) %>%
    arrange(desc(n_cuentas))
  list(clusters = clusters, detalle = res)
}

#' Detecta co-URL: el MISMO enlace compartido por varias cuentas en una ventana corta.
#' Señal clásica de bodega (difundir el mismo link de forma coordinada).
#' @param tweets data.frame: handle, created_at, text (los URLs se extraen del texto).
#' @return lista con: clusters (resumen por URL) y detalle (cuentas implicadas).
detectar_courl <- function(tweets, ventana_seg = 300, min_cuentas = 3) {
  stopifnot(all(c("handle","created_at","text") %in% names(tweets)))
  ext <- regmatches(tweets$text, gregexpr("https?://[^\\s]+", tweets$text, perl = TRUE))
  rows <- list()
  for (i in seq_len(nrow(tweets))) for (u in ext[[i]]) {
    url <- tolower(gsub("[)\\].,]+$", "", u))                 # limpiar puntuación final
    rows[[length(rows)+1]] <- data.frame(handle = tweets$handle[i],
      created_at = tweets$created_at[i], url = url, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(list(clusters = tibble(), detalle = tibble()))
  df <- bind_rows(rows) %>% mutate(ts = parse_fecha(created_at)) %>% arrange(url, ts)

  res <- df %>% group_by(url) %>% group_modify(function(g, key) {
    g <- arrange(g, ts); mejor <- NULL
    for (i in seq_len(nrow(g))) {
      j <- which(g$ts >= g$ts[i] & g$ts <= g$ts[i] + ventana_seg)
      cuentas <- unique(g$handle[j])
      if (length(cuentas) >= min_cuentas) {
        cand <- tibble(n_cuentas = length(cuentas), n_posts = length(j),
          t_ini = min(g$ts[j]), span_seg = as.numeric(difftime(max(g$ts[j]), min(g$ts[j]), units="secs")),
          cuentas = paste(cuentas, collapse=", "))
        if (is.null(mejor) || cand$n_cuentas > mejor$n_cuentas) mejor <- cand
      }
    }
    if (is.null(mejor)) tibble() else mejor
  }) %>% ungroup()

  if (nrow(res) == 0) return(list(clusters = tibble(), detalle = tibble()))
  clusters <- res %>% transmute(url = str_trunc(url, 60), n_cuentas, n_posts, span_seg, t_ini) %>%
    arrange(desc(n_cuentas))
  list(clusters = clusters, detalle = res)
}

#' Devuelve el conjunto de handles que participan en CUALQUIER clúster coordinado
#' (co-tweet y/o co-URL). Sirve para marcar el eje "coordinación/bodega" por cuenta.
handles_coordinados <- function(...) {
  detalles <- list(...)
  hs <- character(0)
  for (d in detalles) if (!is.null(d) && nrow(d) > 0 && "cuentas" %in% names(d))
    hs <- c(hs, trimws(unlist(strsplit(paste(d$cuentas, collapse = ", "), ","))))
  unique(hs[nchar(hs) > 0])
}

#' Detecta cohortes de creación: días con un número anómalo de cuentas creadas.
#' Útil para granjas de cuentas registradas en lote.
#' @param cuentas data.frame con columna created_at.
#' @param z_umbral z-score sobre el conteo diario para marcar anomalía.
detectar_cohortes_creacion <- function(cuentas, z_umbral = 3) {
  stopifnot("created_at" %in% names(cuentas))
  dias <- cuentas %>%
    mutate(dia = as.Date(parse_fecha(created_at))) %>%
    filter(!is.na(dia)) %>%
    count(dia, name = "n_cuentas")
  if (nrow(dias) < 3) return(tibble())
  mu <- mean(dias$n_cuentas); sdv <- sd(dias$n_cuentas)
  if (is.na(sdv) || sdv == 0) return(tibble())
  dias %>%
    mutate(z = round((n_cuentas - mu) / sdv, 2)) %>%
    filter(z >= z_umbral) %>%
    arrange(desc(z))
}
