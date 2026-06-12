# Metodología

## Principio

Ningún rasgo individual prueba automatización. El enfoque es **acumulación de
señales débiles pero independientes** + **detección de patrones colectivos**
(coordinación), comunicando siempre la incertidumbre.

## Señales por cuenta y umbrales

Definidas en `R/score.R::REGLAS_DEFAULT`. Cada regla es un flag 0/1 con un peso:

| Regla | Umbral | Peso | Fundamento |
|---|---|---|---|
| cuenta_muy_nueva | edad < 90 días | 1.0 | granjas usan cuentas recientes |
| hiperactividad | > 50 tweets/día | 1.5 | volumen impropio de un humano |
| actividad_extrema | > 100 tweets/día | 1.0 | refuerza el anterior |
| ff_desbalanceado | seguidos/seguidores > 5 y <100 followers | 1.0 | sigue masivo, nadie lo sigue |
| handle_aleatorio | entropía > 3.2 o >35 % dígitos | 1.0 | `user84726152` |
| sin_bio | bio vacía | 0.5 | señal débil |
| avatar_default | huevo/silueta | 1.0 | cuenta desatendida |
| perfil_default | perfil sin personalizar | 0.5 | señal débil |
| casi_solo_rt | >95 % retweets | 1.0 | amplificador, no autor |
| contenido_repetido | >30 % textos duplicados | 1.5 | spam/copia-pega |
| sin_descanso | actividad en ≥23 horas distintas | 1.0 | no duerme |

**Índice** = (suma de pesos de flags activos) / (suma de pesos posibles) ∈ [0,1].
**Bandas**: <0.20 probablemente humana · 0.20–0.40 sospechosa · >0.40 alta señal.
**Señal fuerte** = ≥3 flags (ajustable). Los pesos y umbrales son hipótesis
explícitas: cámbialos y documenta el cambio.

## Coordinación (lo más robusto)

`R/coordination.R`:

- **Co-tweet** (`detectar_cotweet`): normaliza texto (sin URLs/menciones/puntuación),
  agrupa por texto idéntico y busca ráfagas de ≥ *N* cuentas distintas dentro de
  una ventana de *W* segundos. Salida: nº de cuentas, nº de posts, span temporal.
  Un mismo mensaje publicado por 20 cuentas en 90 segundos no ocurre por azar.
- **Cohortes de creación** (`detectar_cohortes_creacion`): conteo de cuentas por
  día de creación; marca días con z-score ≥ 3. Detecta registros en lote.

## Estadística

Proporción con señal fuerte reportada con **intervalo de Wilson 95 %**
(`resumen_estadistico`), más adecuado que el normal para proporciones y muestras
chicas. Reporta también el score mediano y la distribución por banda.

## Validación

Con datos reales no hay verdad de campo perfecta. Estrategias:
1. **Sintético etiquetado** (`scripts/gen_ejemplo.R`): mide recall/falsos positivos
   del motor (en el ejemplo: 100 % recall, 0 FP). Es prueba del *código*, no del mundo.
2. **Contraste con Botometer** sobre una submuestra (cuando haya credenciales).
3. **Revisión manual** de una muestra aleatoria de las cuentas marcadas.

## Limitaciones (decirlas siempre)

- No es un clasificador entrenado; es un índice heurístico transparente.
- Cuentas humanas muy activas (periodistas, community managers) pueden marcar flags
  → por eso se inspecciona el detalle, no solo el agregado.
- Sin acceso a la API, la **muestra** depende de cómo se recolectó: documentar el
  método de recolección y posibles sesgos.
- El resultado es una **estimación con incertidumbre**, no un censo de bots.

## Conectores

- `x_lookup_usuarios()` — X API v2, campos `created_at,public_metrics,...`. Lee
  `X_BEARER_TOKEN` del entorno.
- `botometer_score()` — stub: Botometer (RapidAPI) exige además tus credenciales de
  X en el payload; complétalo conforme a sus términos. Se deja sin implementar a
  propósito para no inducir un uso que los infrinja.
