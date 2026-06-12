# gen_ejemplo.R — Genera un dataset SINTÉTICO y etiquetado para demostrar el toolkit.
# NO contiene cuentas reales. Mezcla "humanos" plausibles con una granja de bots
# coordinados, para verificar que el motor distingue ambos patrones.

set.seed(2026)
suppressPackageStartupMessages({ library(dplyr) })
dir.create("data", showWarnings = FALSE)
ahora <- as.POSIXct("2026-06-11 12:00:00", tz = "UTC")

# ---- 70 cuentas "humanas" -----------------------------------------------------
n_h <- 70
humanos <- tibble(
  handle = paste0(sample(c("maria","juan","lucia","andres","cami","pipe","sofia","dani","laura","nico"), n_h, TRUE),
                  sample(c("","_","g","co","2","_real",""), n_h, TRUE),
                  sample(c("","","","","12","87","")               , n_h, TRUE)),
  created_at = ahora - runif(n_h, 400, 4000)*86400,
  followers = round(rlnorm(n_h, 5.5, 1.2)),
  following = round(rlnorm(n_h, 5.2, 1.0)),
  n_tweets  = round(rlnorm(n_h, 7, 1.1)),
  bio = sample(c("Bogotana. Café y bici.","Ing. ambiental","Hincha de Nacional","Mamá, maestra","Amante de los Andes","Periodista freelance"), n_h, TRUE),
  default_avatar = FALSE, default_profile = FALSE, verified = FALSE
)
humanos$handle <- make.unique(humanos$handle, sep = "")

# ---- 30 cuentas "granja de bots" ----------------------------------------------
n_b <- 30
bots <- tibble(
  handle = paste0(sample(c("user","fan","patriota","real","voz"), n_b, TRUE),
                  sprintf("%07d", sample(1e6:9e6, n_b))),
  created_at = ahora - sample(c(10,11,12,13,14), n_b, TRUE)*86400,  # creadas en lote
  followers = round(runif(n_b, 0, 30)),
  following = round(runif(n_b, 200, 900)),
  n_tweets  = round(runif(n_b, 4000, 20000)),                      # cuenta nueva + muchísimos tweets
  bio = "",
  default_avatar = TRUE, default_profile = TRUE, verified = FALSE
)

cuentas <- bind_rows(mutate(humanos, clase="humano"),
                     mutate(bots,    clase="bot")) %>% mutate(across(where(is.numeric), as.numeric))
cuentas$created_at <- format(cuentas$created_at, "%Y-%m-%dT%H:%M:%SZ")
write.csv(cuentas[setdiff(names(cuentas),"clase")], "data/ejemplo_cuentas.csv", row.names = FALSE)
write.csv(cuentas[c("handle","clase")], "data/ejemplo_etiquetas.csv", row.names = FALSE)

# ---- tweets: humanos variados; bots con TEXTO IDÉNTICO en ráfaga --------------
tw <- list()
for (i in seq_len(nrow(humanos))) {
  k <- sample(2:6, 1)
  tw[[length(tw)+1]] <- tibble(
    handle = humanos$handle[i],
    created_at = format(ahora - runif(k, 0, 30)*86400, "%Y-%m-%dT%H:%M:%SZ"),
    text = sample(c("Qué frío en Bogotá hoy","Vamos Colombia","Buen partido anoche",
                    "El agua es vida","Hoy madrugué a la oficina","Feliz finde a todos"), k, TRUE))
}
# campaña coordinada: misma consigna, mismas cuentas-bot, segundos de diferencia
consignas <- c("El cambio es imparable, todos con nuestro candidato!",
               "Apoyo total, esto es puro pueblo organizado vamos!")
for (cns in consignas) {
  t0 <- ahora - sample(1:5,1)*86400
  tw[[length(tw)+1]] <- tibble(
    handle = sample(bots$handle, 22),
    created_at = format(t0 + sample(0:90, 22, TRUE), "%Y-%m-%dT%H:%M:%SZ"),
    text = cns)
}
tweets <- bind_rows(tw)
write.csv(tweets, "data/ejemplo_tweets.csv", row.names = FALSE)
cat("Generado: data/ejemplo_cuentas.csv (", nrow(cuentas), "cuentas),",
    "data/ejemplo_tweets.csv (", nrow(tweets), "tweets)\n")
