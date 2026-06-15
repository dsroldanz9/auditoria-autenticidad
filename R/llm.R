# llm.R — Señal OPCIONAL de contenido vía un LLM (p.ej. OpenAI).
#
# OJO: el LLM NO trae datos de Twitter. Solo EVALÚA el texto de tweets que ya
# obtuviste de una fuente de datos real. Devuelve 0..1 = qué tan automatizado /
# generado / copia-pega parece el contenido. Es una señal entre varias, no un veredicto.

suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

#' Clasifica la POSTURA política de una cuenta a partir de sus tweets (no trae datos; solo juzga texto).
#' Atacar a la derecha (Abelardo/Uribe/Fico) = APOYAR al Pacto. @return "ataca_pacto"|"apoya_pacto"|"neutral"|NA
clasificar_postura_llm <- function(textos, api_key = Sys.getenv("OPENAI_API_KEY"), modelo = "gpt-4o-mini") {
  if (api_key == "" || length(textos) == 0) return(NA_character_)
  muestra <- paste0("- ", utils::head(textos, 25), collapse = "\n")
  sis <- paste("Eres analista político de Colombia (elecciones 2026). Te doy tweets de UNA cuenta.",
    "Clasifica su postura: ¿ATACA al Pacto Histórico / la izquierda (Gustavo Petro, Iván Cepeda, Mafe Carrascal)",
    "o los APOYA? OJO: atacar a la derecha (Abelardo de la Espriella, Álvaro Uribe, Fico) = APOYAR al Pacto;",
    "defender a Petro/Cepeda = APOYAR al Pacto. Si no hay señal clara, neutral.",
    "Responde SOLO JSON: {\"postura\":\"ataca_pacto\"|\"apoya_pacto\"|\"neutral\"}.")
  cuerpo <- list(model = modelo, temperature = 0, response_format = list(type = "json_object"),
    messages = list(list(role = "system", content = sis),
                    list(role = "user", content = paste0("Tweets:\n", muestra))))
  resp <- tryCatch(request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(api_key) |> req_body_json(cuerpo) |> req_timeout(25) |> req_perform(), error = function(e) NULL)
  if (is.null(resp)) return(NA_character_)
  txt <- resp_body_json(resp)$choices[[1]]$message$content
  val <- tryCatch(jsonlite::fromJSON(txt)$postura, error = function(e) NA_character_)
  as.character(val)
}

#' @param textos vector de tweets (texto).
#' @param api_key OPENAI_API_KEY del entorno.
#' @param modelo modelo de chat (barato por defecto).
#' @return numérico 0..1, o NA si no hay clave / falla.
analizar_texto_llm <- function(textos, api_key = Sys.getenv("OPENAI_API_KEY"),
                               modelo = "gpt-4o-mini") {
  if (api_key == "" || length(textos) == 0) return(NA_real_)
  muestra <- paste0("- ", utils::head(textos, 20), collapse = "\n")
  sistema <- paste("Eres un analista de autenticidad en redes. Evalúa SOLO el estilo del texto.",
    "Devuelve un JSON {\"score\":0.0} donde score es la probabilidad (0 a 1) de que estos",
    "mensajes sean automatizados, generados por máquina, spam o copia-pega coordinado.",
    "No inventes datos de la cuenta; juzga únicamente el texto provisto.")
  cuerpo <- list(model = modelo, temperature = 0,
    response_format = list(type = "json_object"),
    messages = list(
      list(role="system", content=sistema),
      list(role="user", content=paste0("Mensajes:\n", muestra))))
  resp <- tryCatch(
    request("https://api.openai.com/v1/chat/completions") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(cuerpo) |> req_timeout(25) |> req_perform(),
    error = function(e) NULL)
  if (is.null(resp)) return(NA_real_)
  txt <- resp_body_json(resp)$choices[[1]]$message$content
  val <- tryCatch(jsonlite::fromJSON(txt)$score, error = function(e) NA_real_)
  as.numeric(val)
}
