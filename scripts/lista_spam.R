# lista_spam.R — Consolida los spam-bots hallados (RDS del escáner + los confirmados a mano)
# y exporta una tabla ordenada por nº de repeticiones. Solo formatea, no llama API.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
rds <- "C:/Users/LENOVO/AppData/Local/Temp/bots_spam.rds"
bots <- if (file.exists(rds)) readRDS(rds) else list()
# añadir los confirmados a mano (hallados con analizar_uno/buscar_spam)
extra <- list(
  JavierV82485933 = list(handle="JavierV82485933", veces=28, msg="gas esta gente comunista falsedad, montajes, sangre y fuego malditos con maldicion"),
  IkariDavhen     = list(handle="IkariDavhen",     veces=10, msg="gas esta gente comunista falsedad, montajes, sangre y fuego malditos con maldicion"),
  Alejo_puentesv  = list(handle="Alejo_puentesv",  veces=27, msg="pacto hamponico"))
for (k in names(extra)) if (is.null(bots[[k]])) bots[[k]] <- extra[[k]]

df <- do.call(rbind, lapply(bots, function(b) data.frame(
  cuenta = paste0("@", b$handle), repeticiones = b$veces,
  mensaje = substr(gsub("\\s+"," ", b$msg %||% ""), 1, 90), stringsAsFactors = FALSE)))
`%||%` <- function(a,b) if (is.null(a)) b else a
df <- df[order(-df$repeticiones), ]
cat("=== LISTA CONSOLIDADA DE SPAM-BOTS (", nrow(df), ") ===\n", sep="")
print(df, row.names = FALSE)
write.csv(df, "docs/data/spam_bots.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nGuardado: docs/data/spam_bots.csv\n")
