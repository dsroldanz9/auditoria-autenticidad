# demo.R — Corre la auditoría end-to-end sobre el dataset sintético y valida el motor.
# Uso:  Rscript scripts/demo.R   (desde la raíz del repo)

for (f in c("features.R","score.R","coordination.R","connectors.R","pipeline.R"))
  source(file.path("R", f))

if (!file.exists("data/ejemplo_cuentas.csv")) source("scripts/gen_ejemplo.R")

dat <- cargar_csv("data/ejemplo_cuentas.csv", "data/ejemplo_tweets.csv")
res <- auditar(dat$cuentas, dat$tweets)

cat("\n========= RESUMEN =========\n")
r <- res$resumen
cat(sprintf("Cuentas analizadas:        %d\n", r$n_cuentas))
cat(sprintf("Con señal fuerte (>=3):    %d  (%.1f%%, IC95 %.1f-%.1f)\n",
            r$n_senal_fuerte, r$pct_senal_fuerte, r$ic95_inf, r$ic95_sup))
cat(sprintf("Score mediano:             %.3f\n", r$score_mediano))
cat("\nPor banda:\n"); print(r$por_banda)

# validación contra etiquetas conocidas (solo posible porque es sintético)
et <- read.csv("data/ejemplo_etiquetas.csv")
val <- merge(res$scored[c("handle","n_flags","senal_fuerte")], et, by="handle")
cat("\n========= VALIDACIÓN (sintético) =========\n")
print(table(clase = val$clase, senal_fuerte = val$senal_fuerte))
tp <- sum(val$clase=="bot"    &  val$senal_fuerte)
fn <- sum(val$clase=="bot"    & !val$senal_fuerte)
fp <- sum(val$clase=="humano" &  val$senal_fuerte)
cat(sprintf("Recall sobre bots:    %.0f%%\n", 100*tp/(tp+fn)))
cat(sprintf("Falsos positivos:     %d de %d humanos\n", fp, sum(val$clase=="humano")))

cat("\n========= COORDINACIÓN (co-tweet) =========\n")
if (!is.null(res$cotweet) && nrow(res$cotweet$clusters)>0) print(res$cotweet$clusters) else cat("Sin clústeres.\n")

cat("\n========= COHORTES DE CREACIÓN =========\n")
if (nrow(res$cohortes)>0) print(res$cohortes) else cat("Sin cohortes anómalas.\n")
