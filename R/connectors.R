# connectors.R — Conectores OPCIONALES a fuentes de datos legítimas.
#
# El motor del toolkit funciona sobre un CSV que tú alimentes. Estos conectores
# son comodidades para quien tenga credenciales. NO hacen scraping: usan APIs
# oficiales. Las claves se leen de variables de entorno (.env), nunca se hardcodean.

suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

# --- X / Twitter API v2 (requiere Bearer token de un proyecto de pago) ----------
# Lee X_BEARER_TOKEN del entorno. Devuelve cuentas en el esquema del toolkit.
x_lookup_usuarios <- function(handles, bearer = Sys.getenv("X_BEARER_TOKEN")) {
  if (bearer == "") stop("Falta X_BEARER_TOKEN en el entorno (.env).")
  handles <- gsub("@", "", handles)
  campos <- "created_at,public_metrics,description,profile_image_url,verified,location,protected"
  out <- list()
  for (lote in split(handles, ceiling(seq_along(handles)/100))) {
    resp <- request("https://api.twitter.com/2/users/by") |>
      req_url_query(usernames = paste(lote, collapse=","), `user.fields` = campos) |>
      req_auth_bearer_token(bearer) |>
      req_perform()
    js <- resp_body_json(resp)
    for (u in js$data) {
      pm <- u$public_metrics
      out[[length(out)+1]] <- data.frame(
        handle = u$username, display_name = u$name, created_at = u$created_at,
        followers = pm$followers_count, following = pm$following_count,
        n_tweets = pm$tweet_count, bio = ifelse(is.null(u$description), "", u$description),
        verified = isTRUE(u$verified),
        avatar_default = grepl("default_profile_images", ifelse(is.null(u$profile_image_url),"",u$profile_image_url)),
        location = ifelse(is.null(u$location), NA, u$location),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# --- Botometer (vía RapidAPI) ---------------------------------------------------
# Score validado académicamente (0-5). Requiere RAPIDAPI_KEY y credenciales de X.
# Devuelve el score 'cap.universal' (probabilidad calibrada de ser bot).
botometer_score <- function(handle, rapidapi_key = Sys.getenv("RAPIDAPI_KEY")) {
  if (rapidapi_key == "") stop("Falta RAPIDAPI_KEY en el entorno (.env).")
  warning("Botometer requiere además tus credenciales de la X API en la petición; ver docs de RapidAPI.")
  # Esqueleto: el usuario debe completar el payload con sus credenciales de X.
  # Se deja como stub deliberado para no inducir un uso que viole términos.
  stop("Implementar payload Botometer con credenciales propias. Ver METODOLOGIA.md §Conectores.")
}

# --- Carga desde CSV (la ruta recomendada y siempre disponible) -----------------
cargar_csv <- function(ruta_cuentas, ruta_tweets = NULL) {
  cuentas <- read.csv(ruta_cuentas, stringsAsFactors = FALSE, encoding = "UTF-8")
  tweets <- if (!is.null(ruta_tweets) && file.exists(ruta_tweets))
    read.csv(ruta_tweets, stringsAsFactors = FALSE, encoding = "UTF-8") else NULL
  list(cuentas = cuentas, tweets = tweets)
}
