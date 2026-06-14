# score.R — Combina las señales en un índice de inautenticidad (0-1) TRANSPARENTE.
#
# IMPORTANTE: esto NO es un clasificador entrenado ni una verdad absoluta. Es un
# índice heurístico con pesos explícitos y auditables. Reportar SIEMPRE como
# "señales de automatización/inautenticidad", nunca como "es un bot, seguro".

suppressPackageStartupMessages({ library(dplyr) })

# Cada regla devuelve un flag 0/1. Los umbrales están a la vista y se pueden discutir.
# Referencias: literatura de bot detection (Varol et al. 2017; Yang et al. BotometerLite).
#
# DECISIONES DEL USUARIO (2026-06):
#  - PESOS PAREJOS: todas las señales pesan 1.0; el % depende de cuántas se prenden.
#  - SENSIBLE: umbrales bajos (hiperactividad >50/día, señal fuerte = ≥3 señales).
#  - DOS EJES: estas reglas son el eje AUTOMATIZACIÓN (por cuenta). El eje
#    COORDINACIÓN/bodega se calcula aparte sobre el grupo (ver coordination.R / auditar()).
# Cada regla lleva 'eje' por si más adelante se reportan por separado.
REGLAS_DEFAULT <- list(
  cuenta_muy_nueva   = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$edad_dias < 90),
  cuenta_recien_creada = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$edad_dias < 30),  # 17 días = alarma
  casi_solo_respuestas = list(peso = 1.0, eje = "automatizacion", fn = function(d) if(!"reply_share" %in% names(d)) rep(FALSE,nrow(d)) else coalesce(d$reply_share, 0) > 0.70),  # cuenta torpedo
  hiperactividad     = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$tweets_por_dia > 50),
  actividad_extrema  = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$tweets_por_dia > 100),
  ff_desbalanceado   = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$ff_ratio > 5 & d$followers < 100),
  handle_aleatorio   = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$handle_entropia > 3.2 | d$handle_ratio_dig > 0.35),
  sin_bio            = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$bio_vacia),
  avatar_default     = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$avatar_default),
  perfil_default     = list(peso = 1.0, eje = "automatizacion", fn = function(d) d$perfil_default),
  # las siguientes solo aplican si hay datos de tweets (FALSE limpio si la columna falta):
  casi_solo_rt       = list(peso = 1.0, eje = "automatizacion", fn = function(d) if(!"share_retweets"   %in% names(d)) rep(FALSE,nrow(d)) else coalesce(d$share_retweets, 0)   > 0.95),
  contenido_repetido = list(peso = 1.0, eje = "automatizacion", fn = function(d) if(!"ratio_duplicados" %in% names(d)) rep(FALSE,nrow(d)) else coalesce(d$ratio_duplicados, 0) > 0.30),
  sin_descanso       = list(peso = 1.0, eje = "automatizacion", fn = function(d) if(!"horas_activas"    %in% names(d)) rep(FALSE,nrow(d)) else coalesce(d$horas_activas, 12)   >= 23),
  # señal opcional de contenido (LLM): solo activa si existe la columna llm_contenido
  texto_automatizado = list(peso = 1.0, eje = "automatizacion", fn = function(d) if(!"llm_contenido"    %in% names(d)) rep(FALSE,nrow(d)) else coalesce(d$llm_contenido, 0)    > 0.70)
)

#' Calcula el índice de inautenticidad por cuenta.
#' @param feats data.frame de features (salida de extraer_features_*).
#' @param reglas lista de reglas (ver REGLAS_DEFAULT).
#' @param umbral_fuerte nº de flags para marcar "señal fuerte" (default 3).
#' @return el mismo data.frame + columnas: <una por regla>, n_flags, score_inaut (0-1), banda.
calcular_score <- function(feats, reglas = REGLAS_DEFAULT, umbral_fuerte = 3) {
  # las cuentas verificadas no se eximen, pero se anota (transparencia)
  flags <- lapply(names(reglas), function(nm) {
    r <- reglas[[nm]]
    v <- tryCatch(r$fn(feats), error = function(e) rep(FALSE, nrow(feats)))
    as.integer(ifelse(is.na(v), FALSE, v))
  })
  names(flags) <- paste0("flag_", names(reglas))
  flagdf <- as.data.frame(flags)

  res <- bind_cols(feats, flagdf)
  res$n_flags     <- rowSums(flagdf)
  # Con pesos parejos, el índice = nº de señales / 5 (5+ señales = máximo). Punchy e intuitivo:
  # ya no se diluye entre muchas reglas, así una cuenta nueva con 2-3 señales se ve fuerte.
  res$score_inaut <- round(pmin(1, res$n_flags / 5), 3)
  # Banda por NÚMERO de señales (con pesos parejos es más intuitivo que el ratio diluido):
  # 0 = auténtica · 1-2 = sospechosa · ≥3 = alta señal. Así una cuenta de 17 días con
  # 2 señales ya NO sale como "humana".
  res$banda <- cut(res$n_flags,
                   breaks = c(-Inf, 0, 2, Inf),
                   labels = c("Probablemente humana", "Sospechosa", "Alta señal de automatización"))
  res$senal_fuerte <- res$n_flags >= umbral_fuerte
  res
}

#' Resumen estadístico HONESTO de un conjunto de cuentas ya puntuadas.
#' Devuelve proporción con señal fuerte + intervalo de confianza Wilson 95%.
resumen_estadistico <- function(scored, umbral_fuerte = 3) {
  n <- nrow(scored)
  k <- sum(scored$n_flags >= umbral_fuerte)
  p <- k / n
  # IC Wilson (mejor que normal para proporciones)
  z <- 1.96
  centro <- (p + z^2/(2*n)) / (1 + z^2/n)
  margen <- z * sqrt((p*(1-p) + z^2/(4*n)) / n) / (1 + z^2/n)
  list(
    n_cuentas        = n,
    n_senal_fuerte   = k,
    pct_senal_fuerte = round(100*p, 1),
    ic95_inf         = round(100*max(0, centro - margen), 1),
    ic95_sup         = round(100*min(1, centro + margen), 1),
    score_mediano    = round(median(scored$score_inaut), 3),
    por_banda        = as.data.frame(table(scored$banda))
  )
}
