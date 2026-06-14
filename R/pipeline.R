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

  scored  <- calcular_score(feats, umbral_fuerte = umbral_fuerte)   # EJE A: automatización
  resumen <- resumen_estadistico(scored, umbral_fuerte = umbral_fuerte)

  # EJE B: coordinación / bodega (de grupo)
  cotweet  <- if (!is.null(tweets)) detectar_cotweet(tweets, ventana_seg, min_cuentas) else NULL
  courl    <- if (!is.null(tweets)) detectar_courl(tweets, max(ventana_seg, 300), min_cuentas) else NULL
  cohortes <- detectar_cohortes_creacion(cuentas)

  coord_h <- handles_coordinados(if (!is.null(cotweet)) cotweet$detalle else NULL,
                                 if (!is.null(courl))   courl$detalle   else NULL)
  scored$en_coordinacion <- scored$handle %in% coord_h
  n <- nrow(scored); k <- sum(scored$en_coordinacion)
  resumen_coord <- list(n_cuentas = n, n_coordinadas = k,
                        pct_coordinadas = round(100 * k / n, 1))

  list(scored = scored, resumen = resumen, resumen_coord = resumen_coord,
       cotweet = cotweet, courl = courl, cohortes = cohortes)
}
