# Auditoría de autenticidad en X (Twitter)

Toolkit abierto en R + app Shiny para **estimar la prevalencia de cuentas
automatizadas y de comportamiento coordinado** en una conversación de X, con
metodología **transparente y auditable**.

> **Lo que es y lo que no es.** No existe ninguna herramienta que *pruebe* que
> una cuenta es un bot —ni Botometer ni esta. Lo que se mide son **señales
> observables** que, combinadas, elevan la probabilidad. La forma honesta y
> defendible de comunicar el resultado es:
> *"El X % de las cuentas que amplifican tal etiqueta muestran ≥3 señales fuertes
> de automatización/coordinación (IC 95 % …)"*, nunca *"son bots"*.

## Por qué la coordinación es el dato más fuerte

Adivinar "bot sí / bot no" cuenta por cuenta siempre será discutible. En cambio,
el **comportamiento inauténtico coordinado** —cuentas distintas publicando el
**mismo texto en ventanas de segundos**, o creadas en **cohortes el mismo día**—
deja una huella estadística que es muy difícil de producir orgánicamente. Es lo
que de verdad desmonta un "apoyo popular" inflado. Por eso el módulo de
coordinación (`R/coordination.R`) es el corazón del toolkit.

## Qué hace

- **Score por cuenta** (`R/features.R`, `R/score.R`): índice 0–1 a partir de edad
  de la cuenta, tasa de publicación, ratio seguidos/seguidores, entropía del
  handle, avatar/perfil por defecto, % de retweets, contenido duplicado, actividad
  sin descanso. Pesos explícitos y editables.
- **Coordinación** (`R/coordination.R`): detecta co-tweet (texto idéntico, muchas
  cuentas, ventana corta) y cohortes de creación anómalas (z-score).
- **Estadística honesta** (`R/score.R`): proporción con señal fuerte + intervalo
  de confianza de Wilson 95 %.
- **App online** (`app.R`): subes un CSV, ves KPIs, distribución, tabla de cuentas,
  clústeres de coordinación y cohortes; exportas resultados.

## Instalación

```r
install.packages(c("shiny","DT","ggplot2","dplyr","stringr","lubridate","httr2","jsonlite"))
```

## Uso rápido (datos de ejemplo, sintéticos)

```bash
Rscript scripts/demo.R     # corre la auditoría sobre data/ejemplo_*.csv
```

```r
shiny::runApp(".")         # abre la app; botón "Usar datos de ejemplo"
```

El ejemplo sintético (`scripts/gen_ejemplo.R`, 70 humanos + 30 bots coordinados)
sirve para verificar el motor: en él logra 100 % de recall sobre los bots y 0
falsos positivos, y aísla los dos clústeres de consigna coordinada.

## Modo "a demanda" (buscar un perfil)

Buscás un `@handle` y obtenés el % en vivo con el desglose de señales:

```r
source_all <- function() for (f in c("features.R","score.R","coordination.R",
  "connectors.R","llm.R","ondemand.R","pipeline.R")) source(file.path("R", f))
source_all()
auditar_handle("@cuenta", fuente = "mock")        # "mock" = prueba sin gastar
auditar_handle("@cuenta", fuente = "x_api")        # X API v2 (X_BEARER_TOKEN)
auditar_handle("@cuenta", fuente = "twitterapi_io")# proveedor tercero barato
```

Cada consulta se guarda en `data/registro.csv` (la "base de datos" local). En la app
está la pestaña **🔎 Buscar un perfil** con caja de búsqueda, medidor y señales.

> **Importante: una API de ChatGPT/OpenAI NO trae datos de Twitter.** Para obtener el
> perfil real de `@cuenta` (edad, seguidores, tweets) hace falta una *fuente de datos
> de Twitter* (X API v2, o un tercero tipo twitterapi.io/Apify). El LLM (opcional,
> `R/llm.R`) solo se usa para **evaluar el estilo del texto** de tweets ya obtenidos —
> nunca para inventar métricas de la cuenta.

## De dónde salen los datos (el cuello de botella honesto)

El motor corre sobre un **CSV que tú alimentes legítimamente**. Opciones:

1. **X API v2** (proyecto de pago): `R/connectors.R::x_lookup_usuarios()` con tu
   `X_BEARER_TOKEN`. No hace scraping; usa la API oficial.
2. **Botometer** (RapidAPI, validado académicamente): stub en `R/connectors.R`
   —requiere tus propias credenciales de X; completar según sus términos.
3. **Exportaciones / datasets** que ya tengas, con el esquema de columnas de abajo.

**No** se incluye ningún scraper: scrapear X viola sus términos de servicio.

### Esquema de columnas

`cuentas.csv`: `handle, created_at, followers, following, n_tweets` (+ opcionales
`bio, default_avatar, default_profile, verified, location`).
`tweets.csv`: `handle, created_at, text` (+ opcional `is_retweet`).

## Herramientas relacionadas

- **Botometer / BotometerLite** (OSoMe, Indiana University) — estándar académico.
- **Bot Sentinel** — cuentas tóxicas/trolls (modelo propio, cerrado).
- **Hoaxy** (OSoMe) — visualización de difusión y bots.
- **tweetbotornot2** (R) — modelo entrenado, dependía de la API v1.1 (descontinuada).

## Estructura

```
R/            features.R · score.R · coordination.R · connectors.R · pipeline.R
app.R         aplicativo Shiny
scripts/      gen_ejemplo.R (datos sintéticos) · demo.R (pipeline end-to-end)
data/         CSVs de ejemplo (sintéticos)
METODOLOGIA.md  indicadores, pesos, umbrales y limitaciones
```

## Licencia

MIT. Ver `LICENSE`. Úsese con rigor: el valor de esta herramienta es su
transparencia, no exagerar las conclusiones.
