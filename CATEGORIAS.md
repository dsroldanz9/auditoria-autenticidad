# Categorías y pesos — marco de literatura (propuesta para iterar)

> Los pesos viven en `R/score.R::REGLAS_DEFAULT`. Este documento justifica el modelo
> de **dos ejes** y deja las fuentes a mano.

## ✅ Decisiones tomadas (2026-06)
1. **Dos ejes**: Automatización (por cuenta, en la tarjeta) + Coordinación/bodega
   (de grupo: co-tweet, co-URL, cohortes; en el modo "Auditar lista").
2. **Pesos parejos**: todas las señales pesan 1.0; el % depende de cuántas se prenden.
3. **Sensible**: hiperactividad >50/día, "señal fuerte" = ≥3 señales. (Atrapa más; por eso
   el lenguaje público es "muestra señales", nunca "es un bot".)
4. **co-URL implementado** (`detectar_courl`) + co-tweet con mínimo de 25 caracteres para
   evitar falsa coordinación por frases triviales.

## El hallazgo central: son DOS fenómenos, no uno

La literatura distingue **automatización** (bots) de **coordinación inauténtica**
(operaciones de cuentas, "bodegas", trolls pagos). Un detector de bots por sí solo
**no** captura a los bodegueros, porque esas cuentas pueden ser **humanas reales**
pero actuar coordinadas. Hay que medir las dos cosas por separado.

### Eje A — Automatización (señales POR CUENTA)
Basado en Varol et al. (2017) / Botometer: ~1.000 features en 6 familias —
*perfil/metadata, amigos, red, temporal, contenido/lenguaje, sentimiento*— y las de
**metadata son las más informativas**. Nuestras señales (y su familia):

| Señal (`R/score.R`) | Familia Botometer | Peso actual |
|---|---|---|
| cuenta_muy_nueva | metadata | 1.0 |
| hiperactividad / actividad_extrema | temporal | 1.5 / 1.0 |
| ff_desbalanceado | amigos/red | 1.0 |
| handle_aleatorio | metadata | 1.0 |
| avatar_default / perfil_default | metadata | 1.0 / 0.5 |
| sin_bio | metadata | 0.5 |
| casi_solo_rt | contenido | 1.0 |
| contenido_repetido | contenido | 1.5 |
| sin_descanso | temporal | 1.0 |
| texto_automatizado (LLM) | contenido/lenguaje | 1.0 |

### Eje B — Coordinación / "bodega" (señales DE GRUPO, no por cuenta)
No se ve en un perfil aislado; se ve en la **red**. Indicadores de la literatura de
*coordinated inauthentic behavior* (CIB):

- **co-retweet / co-tweet**: mismo texto, varias cuentas, ventana de segundos. (implementado: `detectar_cotweet`)
- **co-URL**: el mismo enlace compartido por muchas cuentas casi a la vez. (por implementar)
- **co-hashtag**: misma secuencia de hashtags. (por implementar)
- **cohortes de creación**: cuentas registradas el mismo día / en lote. (implementado: `detectar_cohortes_creacion`)
- **sincronía temporal**: picos de actividad simultánea. Umbrales conservadores en la
  literatura: percentil 99,5 de similitud para aristas, percentil 95 para podar nodos.

A una cuenta se le puede asignar un **"score de coordinación"** = cuánto participa en
los clústeres coordinados detectados. Ese es el número que delata al bodeguero, aunque
su perfil individual parezca el de un humano normal.

## Propuesta concreta (a discutir antes de cambiar pesos)
1. Reportar **dos índices** separados en la app: `Automatización` y `Coordinación`.
2. Añadir **co-URL** y **co-hashtag** a `coordination.R`.
3. Calibrar pesos del Eje A contra una submuestra etiquetada con **Botometer** (cuando
   haya credenciales) en vez de fijarlos a mano.
4. Mantener el reporte honesto: porcentajes con IC 95%, nunca "es un bot".

## Fuentes
- Varol et al. (2017), *Online Human-Bot Interactions: Detection, Estimation, and Characterization*.
- Yang et al., *BotometerLite / Scalable and Generalizable Social Bot Detection*.
- Reviews de detección por ML de bots (Springer SNAM 2022) y survey de spam/cuentas falsas (arXiv 2211.05913).
- Literatura de Coordinated Inauthentic Behavior e *information spreading on Twitter* (Decision Support Systems / ScienceDirect 2022).
