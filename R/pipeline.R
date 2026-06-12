# pipeline.R — Orquesta todo: dataset -> features -> score -> coordinación -> resumen.
# Requiere que features.R, score.R y coordination.R ya estén sourceados por el caller
# (ver scripts/demo.R y app.R).

#' Ejecuta la auditoría completa.
#' @param cuentas data.frame de cuentas (esquema del toolkit).
#' @param tweets  data.frame de tweets (opcional).
#' @return lista: scored (cuentas puntuadas), resumen, cotweet, cohortes.
auditar <- function(cuentas, tweets = NULL,
                    ventana_seg = 120, min_cuentas = 3, umbral_fuerte = 3) {
  feats <- extraer_features_cuenta(cuentas)
  ftw <- extraer_features_tweets(tweets)
  if (!is.null(ftw)) feats <- dplyr::left_join(feats, ftw, by = "handle")

  scored  <- calcular_score(feats, umbral_fuerte = umbral_fuerte)
  resumen <- resumen_estadistico(scored, umbral_fuerte = umbral_fuerte)

  cotweet  <- if (!is.null(tweets)) detectar_cotweet(tweets, ventana_seg, min_cuentas) else NULL
  cohortes <- detectar_cohortes_creacion(cuentas)

  list(scored = scored, resumen = resumen, cotweet = cotweet, cohortes = cohortes)
}
