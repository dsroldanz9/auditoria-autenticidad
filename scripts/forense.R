# forense.R — Señales REALES de automatización (no solo "repite mucho").
# Un humano convencido también copia-pega su consigna. Lo que delata a un BOT es:
#   1) VELOCIDAD: postear en ráfagas humanamente imposibles (N posts en minutos).
#   2) RED: el MISMO texto saliendo de MUCHAS cuentas distintas (coordinación).
#   3) METADATOS: cuenta nueva, sigue a miles / casi sin seguidores, avatar default, handle con dígitos al azar.
# Repetir un mensaje argumentado 50 veces a lo largo de semanas = humano terco, NO bot.
if (basename(getwd()) != "auditoria-autenticidad" && dir.exists("C:/Users/LENOVO/Documents/auditoria-autenticidad"))
  setwd("C:/Users/LENOVO/Documents/auditoria-autenticidad")
source("R/ondemand.R")
key <- Sys.getenv("TWITTERAPI_IO_KEY")
if (key == "") stop("Falta TWITTERAPI_IO_KEY")
Sys.setenv(MAX_PAGES = "2")   # rápido, para no colgarse si la máquina duerme

parse_ts <- function(x) {
  x <- as.character(x)
  out <- as.POSIXct(x, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "UTC")  # "Tue Dec 10 07:00:30 +0000 2024"
  na <- is.na(out)
  if (any(na)) out[na] <- as.POSIXct(gsub("Z$", "", x[na]), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  out
}
fnum <- function(x, d = 1) if (is.null(x) || length(x) == 0 || is.na(x)) "  —" else formatC(as.numeric(x), format = "f", digits = d, width = 5)
fint <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else as.character(as.integer(x))
handle_aleatorio <- function(h) grepl("[0-9]{4,}$", h)   # dígitos al azar al final (JavierV82485933)

handles <- c("EDWINANDRESREN1","JavierV82485933","brokass25","jarealbb","Alejo_puentesv","IkariDavhen","CECM1991")

cat("=== FORENSE: señales de AUTOMATIZACIÓN por cuenta ===\n\n")
res <- list()
for (h in handles) {
  p <- tryCatch(fetch_twitterapi_io(h, key), error = function(e) NULL)
  if (is.null(p) || is.null(p$cuenta)) { cat(sprintf("@%-16s  (no se pudo consultar)\n", h)); next }
  cu <- p$cuenta; tw <- p$tweets
  edad <- if (!is.null(cu$created_at)) as.numeric(difftime(Sys.time(), parse_ts(cu$created_at), units = "days")) else NA
  segdr <- cu$followers %||% NA; sigue <- cu$following %||% NA
  ff <- if (!is.na(sigue) && !is.na(segdr)) sigue / max(segdr, 1) else NA
  rep <- mensajes_repetidos(tw, 1)
  rep_n <- if (nrow(rep) > 0) rep$veces[1] else 0
  rep_tx <- if (nrow(rep) > 0) rep$texto[1] else ""
  n <- if (is.null(tw)) 0 else nrow(tw)
  pct_resp <- if (n > 0 && "es_respuesta" %in% names(tw)) mean(tw$es_respuesta, na.rm = TRUE) else NA
  span_h <- NA; burst <- NA; vel <- NA
  if (n > 1) {
    ts <- sort(parse_ts(tw$created_at)); ts <- ts[!is.na(ts)]
    if (length(ts) > 1) {
      span_h <- as.numeric(difftime(max(ts), min(ts), units = "hours"))
      burst  <- max(vapply(seq_along(ts), function(i) sum(ts >= ts[i] & ts < ts[i] + 600), integer(1)))  # max en 10 min
      vel    <- n / max(span_h, 1e-4)
    }
  }
  res[[h]] <- list(handle = h, edad = edad, seg = segdr, sigue = sigue, ff = ff, n = n,
    avatar_def = isTRUE(cu$default_avatar), verif = isTRUE(cu$verified), alea = handle_aleatorio(h),
    pct_resp = pct_resp, span_h = span_h, burst = burst, vel = vel, rep_n = rep_n, rep_tx = rep_tx)
  cat(sprintf("@%-16s edad=%5sd  seg=%6s sigue=%6s (sigue/seg=%s)  avatarDef=%s verif=%s handleAlea=%s\n",
      h, fint(edad), fint(segdr), fint(sigue), fnum(ff), isTRUE(cu$default_avatar), isTRUE(cu$verified), handle_aleatorio(h)))
  cat(sprintf("   tweets=%d  %%respuestas=%s  lapso=%sh  velocidad=%s/h  RAFAGA(max en 10min)=%s  repiteMsg=%dx\n",
      n, fnum(pct_resp, 2), fnum(span_h), fnum(vel), fint(burst), rep_n))
  cat(sprintf("   msg: \"%s\"\n\n", substr(rep_tx, 1, 90)))
}
saveRDS(res, "C:/Users/LENOVO/AppData/Local/Temp/forense.rds")

# === PRUEBA DECISIVA: ¿el texto sale de 1 cuenta (humano) o de MUCHAS (red)? ===
cross <- function(frase, key, pages = 3) {
  authors <- character(0); cursor <- ""
  for (pg in seq_len(pages)) {
    r <- tryCatch(request("https://api.twitterapi.io/twitter/tweet/advanced_search") |>
      req_url_query(query = paste0('"', frase, '"'), queryType = "Latest", cursor = cursor) |>
      req_headers(`X-API-Key` = key) |> req_timeout(25) |> req_perform(), error = function(e) NULL)
    if (is.null(r)) break
    d <- resp_body_json(r); arr <- d$tweets %||% list()
    if (length(arr) == 0) break
    authors <- c(authors, tolower(vapply(arr, function(t) t$author$userName %||% "", character(1))))
    if (!isTRUE(d$has_next_page) || is.null(d$next_cursor) || identical(d$next_cursor, "")) break
    cursor <- d$next_cursor
  }
  unique(authors[nzchar(authors)])
}
cat("=== ¿UNA persona o una RED? (cuentas DISTINTAS que postean el MISMO texto) ===\n")
pruebas <- list(
  "EDWIN (pro-Abelardo, eloquente)" = "abenarco no representa la inteligencia",
  "JAVIER (consigna cruda)"          = "sangre y fuego malditos",
  "JAREALBB (consigna cruda)"        = "por la razon o por la fuerza")
for (nm in names(pruebas)) {
  a <- cross(pruebas[[nm]], key, pages = 3)
  cat(sprintf("  %-34s -> %2d cuenta(s) distinta(s)  %s\n", nm, length(a),
      if (length(a) >= 5) "<< RED / coordinacion" else if (length(a) <= 1) "<< una sola cuenta (humano?)" else ""))
  if (length(a) > 0) cat("     ", paste0("@", head(a, 12), collapse = "  "), "\n")
}
cat("\nListo. RDS: forense.rds\n")
