# app.R — Detector de autenticidad en X · identidad de campaña (Iván Cepeda).
# UI bslib + tarjeta de resultado descargable (html2canvas) para compartir en X.

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(ggplot2); library(dplyr)
})
for (f in c("features.R","score.R","coordination.R","connectors.R","llm.R","ondemand.R","pipeline.R"))
  source(file.path("R", f))
# Carga tokens (TWITTERAPI_IO_KEY, etc.) en shinyapps.io. secret.R NO se sube a GitHub (gitignored).
if (file.exists("secret.R")) try(source("secret.R"), silent = TRUE)

AZ <- "#2839BE"; NAR <- "#FF4403"; ORO <- "#F7A40D"; VERDE <- "#16A34A"; INK <- "#1B1F3B"
REPO <- "https://github.com/dsroldanz9/auditoria-autenticidad"
URL_APP <- "geografiacritica2026.shinyapps.io/auditoria-autenticidad"

# ---------- helpers de la tarjeta ----------
fmt_edad <- function(d) {
  if (is.na(d)) return("—")
  if (d >= 365) sprintf("%.1f años", d/365)
  else if (d >= 60) sprintf("%.0f meses", d/30)
  else sprintf("%.0f días", d)
}
donut_svg <- function(pct, col) {
  r <- 70; circ <- 2*pi*r; off <- circ*(1 - pct/100)
  sprintf('<svg width="170" height="170" viewBox="0 0 180 180">
    <circle cx="90" cy="90" r="70" fill="none" stroke="#FFFFFF30" stroke-width="18"/>
    <circle cx="90" cy="90" r="70" fill="none" stroke="%s" stroke-width="18" stroke-linecap="round"
      stroke-dasharray="%.1f" stroke-dashoffset="%.1f" transform="rotate(-90 90 90)"/>
    <text x="90" y="84" text-anchor="middle" font-family="Archivo Black" font-size="44" fill="#FFFFFF">%d%%</text>
    <text x="90" y="110" text-anchor="middle" font-family="Inter" font-size="12" fill="%s">automatización</text>
  </svg>', col, circ, off, pct, ORO)
}
chip <- function(lab, val)
  sprintf('<div style="background:#FFFFFF14;border-radius:10px;padding:8px 11px">
    <div style="font-size:11px;color:#C7CCEF">%s</div>
    <div style="font-family:Archivo Black;font-size:16px;color:#fff">%s</div></div>', lab, val)

tema <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = INK,
  primary = AZ, secondary = "#6B6F7A", success = VERDE, warning = ORO, danger = NAR,
  base_font = font_collection(font_google("Inter", local = FALSE), "Segoe UI", "sans-serif")
) |>
  bs_add_rules("
    body{background:#EEF0FA}
    .navbar{background:#2839BE !important}
    .navbar .navbar-brand,.navbar .nav-link{color:#fff !important}
    .card{border:1px solid #E6E9F5;border-radius:14px}
    .hero{font-family:'Archivo Black',system-ui;letter-spacing:-.3px}
    .nota{font-size:.82rem;color:#6B6F7A}
    .btn-tw{background:#F7A40D;border:none;color:#1B1F3B;font-weight:800}
    .btn-tw:hover{background:#e2950a;color:#1B1F3B}
  ")

# ---------- pestaña 1: buscar un perfil ----------
tab_buscar <- nav_panel(
  title = tagList(tags$span("\U0001F50E"), " Buscar perfil"),
  div(class = "p-2",
    h4(class = "hero", style = paste0("color:", AZ),
       "¿ESTA CUENTA ES REAL O INFLADA?"),
    p(class = "nota", "Escribe un usuario y mira su índice de inautenticidad con los datos clave. ",
      "Es una estimación con criterios transparentes, no un veredicto de 'bot'."),
    layout_columns(
      col_widths = c(5, 3, 2, 2), gap = "10px",
      textInput("h_handle", NULL, placeholder = "@cuenta_a_revisar", width = "100%"),
      selectInput("h_fuente", NULL,
        c("Demo (gratis)" = "mock", "X API v2" = "x_api", "twitterapi.io" = "twitterapi_io")),
      div(class = "pt-1", checkboxInput("h_llm", "Señal LLM", FALSE)),
      actionButton("h_go", "Analizar", class = "btn-primary w-100")
    ),
    br(),
    uiOutput("card_share"),
    div(class = "mt-2",
      tags$button(tagList(icon("download"), " Descargar imagen para Twitter"),
                  onclick = "descargarCard()", class = "btn btn-tw")),
    br(), br(),
    card(card_header("Historial de consultas (base de datos)"), DTOutput("registro_tabla"))
  )
)

# ---------- pestaña 2: auditar una lista ----------
tab_lista <- nav_panel(
  title = tagList(tags$span("\U0001F4CB"), " Auditar lista (CSV)"),
  div(class = "p-2",
    layout_columns(
      col_widths = c(4, 4, 4),
      fileInput("f_cuentas", "CSV de cuentas", accept = ".csv"),
      fileInput("f_tweets", "CSV de tweets (opcional)", accept = ".csv"),
      div(class = "pt-4", actionButton("demo", "Usar datos de ejemplo", class = "btn-outline-primary w-100"))
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Cuentas analizadas", textOutput("k_n"), theme = "primary"),
      value_box("Eje A · Automatización (bots)", textOutput("k_pct"), theme = "danger"),
      value_box("Eje B · Coordinación (bodega)", textOutput("k_coord"), theme = "warning"),
      value_box("Clústeres coordinados", textOutput("k_clus"), theme = "secondary")
    ),
    navset_card_tab(
      nav_panel("Distribución", plotOutput("plot_dist", height = "320px"),
        div(class = "nota p-2", "Eje A. Índice 0–1 por cuenta. La cola derecha concentra cuentas con muchas señales de automatización.")),
      nav_panel("Cuentas", DTOutput("tabla")),
      nav_panel("Co-tweet (mismo texto)",
        div(class = "nota p-2", "Eje B. Mismo texto, varias cuentas, ventana de segundos = coordinación (bodega). La evidencia más fuerte."),
        DTOutput("tabla_cot")),
      nav_panel("Co-URL (mismo link)",
        div(class = "nota p-2", "Eje B. El mismo enlace difundido por varias cuentas casi a la vez. Solo aparece con tweets reales (el demo no trae links)."),
        DTOutput("tabla_courl")),
      nav_panel("Cohortes de creación",
        div(class = "nota p-2", "Eje B. Días con un nº anómalo de cuentas creadas (z ≥ 3): posibles granjas en lote."),
        DTOutput("tabla_coh"))
    ),
    div(class = "p-2", downloadButton("dl", "Descargar resultados (CSV)", class = "btn-sm btn-outline-secondary"))
  )
)

# ---------- pestaña 3: metodología ----------
tab_metodo <- nav_panel(
  title = tagList(tags$span("\U0001F4D0"), " Metodología"),
  div(class = "p-3", style = "max-width:780px", uiOutput("metodo"))
)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Archivo+Black&family=Inter:wght@400;600;800&display=swap"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      function descargarCard(){
        var el=document.getElementById('card_share_inner');
        if(!el){return;}
        html2canvas(el,{scale:2,useCORS:true,backgroundColor:null}).then(function(c){
          var a=document.createElement('a');
          a.download='autenticidad_'+Date.now()+'.png';
          a.href=c.toDataURL('image/png'); a.click();
        });
      }"))
  ),
  page_navbar(
    title = tagList(
      tags$img(src = "logo.png", height = "30",
               style = "border-radius:50%;margin-right:8px;vertical-align:middle"),
      tags$b("DETECTOR DE AUTENTICIDAD")),
    theme = tema, fillable = FALSE,
    tab_buscar, tab_lista, tab_metodo,
    nav_spacer(),
    nav_item(tags$a("Código", href = REPO, target = "_blank", class = "nav-link"))
  )
)

server <- function(input, output, session) {

  # ===== modo a demanda =====
  perfil <- eventReactive(input$h_go, {
    req(nchar(trimws(input$h_handle)) > 0)
    withProgress(message = "Consultando y puntuando...", {
      tryCatch(auditar_handle(input$h_handle, fuente = input$h_fuente, usar_llm = input$h_llm),
               error = function(e) list(error = conditionMessage(e)))
    })
  })

  output$card_share <- renderUI({
    r <- perfil()
    if (is.null(r))
      return(div(class = "nota", "Escribe un usuario y dale clic a Analizar para ver la tarjeta."))
    if (!is.null(r$error))
      return(div(class = "text-danger p-2",
        icon("triangle-exclamation"), paste(" No se pudo consultar:", r$error),
        tags$br(), tags$span(class = "nota",
          "¿Configuraste el token de esa fuente? Con 'Demo' funciona sin token.")))

    if (r$pct >= 40) { col <- NAR; verd <- "ALTA SEÑAL DE AUTOMATIZACIÓN" }
    else if (r$pct >= 20) { col <- ORO; verd <- "CUENTA SOSPECHOSA" }
    else { col <- VERDE; verd <- "PARECE UNA CUENTA REAL" }

    det <- r$detalle
    rt <- if ("share_retweets" %in% names(det) && !is.na(det$share_retweets[1]))
      paste0(round(100*det$share_retweets[1]), "%") else "—"
    chips <- paste0(
      chip("Edad de la cuenta", fmt_edad(det$edad_dias[1])),
      chip("Tweets por día", format(round(det$tweets_por_dia[1]), big.mark=".")),
      chip("Sigue ÷ seguidores", det$ff_ratio[1]),
      chip("% retweets", rt))
    senales_html <- if (length(r$senales) > 0)
      paste0("<ul style='margin:6px 0 0;padding-left:18px;color:#EEF0FA;columns:2'>",
             paste0("<li>", r$senales, "</li>", collapse=""), "</ul>")
    else "<p style='color:#BFE8CF;margin:6px 0 0'>Sin señales de riesgo detectadas.</p>"

    top_html <- if (!is.null(r$top) && nrow(r$top) > 0)
      paste0(vapply(seq_len(nrow(r$top)), function(i) sprintf(
        "<div style='display:flex;justify-content:space-between;align-items:center;padding:5px 0;border-bottom:1px solid #FFFFFF14'>
           <span style='font-size:13px;color:#EEF0FA'>%d. %s</span>
           <span style='font-family:Archivo Black;font-size:13px;color:%s'>%d</span></div>",
        i, r$top$cuenta[i], ORO, r$top$n[i]), character(1)), collapse = "")
    else "<div style='font-size:12px;color:#AEB6E8'>Sin interacciones visibles.<br>(Aparece con tweets reales vía token.)</div>"

    HTML(sprintf("
    <div id='card_share_inner' style='max-width:860px;background:%s;border-radius:18px;
         padding:22px 24px;color:#fff;font-family:Inter,Segoe UI,sans-serif'>
      <div style='display:flex;align-items:center;gap:10px;margin-bottom:12px'>
        <img src='logo.png' height='36' style='border-radius:50%%'/>
        <div style='font-family:Archivo Black;font-size:13px;color:%s'>ME LA JUEGO POR LA VIDA</div>
        <div style='margin-left:auto;font-size:12px;color:#D7DBF5'>@RoldnSantiago</div>
      </div>
      <div style='font-family:Archivo Black;font-size:30px;line-height:1'>@%s</div>
      <div style='font-size:13px;color:#D7DBF5;margin-bottom:14px'>Detector de autenticidad · X</div>
      <div style='display:flex;gap:18px;align-items:flex-start'>
        <div style='flex-shrink:0'>%s</div>
        <div style='flex:1'>
          <span style='display:inline-block;background:%s;color:#1B1F3B;font-family:Archivo Black;
                font-size:13px;padding:6px 12px;border-radius:9px'>%s</span>
          <div style='display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-top:12px'>%s</div>
        </div>
        <div style='width:240px;flex-shrink:0;background:#FFFFFF12;border-radius:12px;padding:12px 14px'>
          <div style='font-family:Archivo Black;font-size:12px;color:%s;margin-bottom:6px'>TOP CUENTAS QUE AMPLIFICA</div>
          %s
        </div>
      </div>
      <div style='margin-top:14px'><b style='color:%s'>Señales detectadas (%d):</b>%s</div>
      <div style='margin-top:14px;font-size:11px;color:#C7CCEF'>
        Mide <b style='color:#fff'>automatización</b> (perfil individual). La <b style='color:#fff'>coordinación / bodega</b> se mide sobre un grupo, en \"Auditar lista\".
      </div>
      <div style='margin-top:10px;border-top:1px solid #FFFFFF22;padding-top:10px;font-size:11px;color:#AEB6E8'>
        Estimación con criterios transparentes y abiertos — no es un veredicto de \"bot\".<br>%s
      </div>
    </div>",
    AZ, ORO, r$handle, donut_svg(r$pct, col), col, verd, chips, ORO, top_html, ORO, r$n_flags, senales_html, URL_APP))
  })

  output$registro_tabla <- renderDT({
    input$h_go
    f <- "data/registro.csv"
    if (!file.exists(f)) return(datatable(data.frame(Info = "Aún no hay consultas en esta sesión"),
                                           rownames = FALSE, options = list(dom = "t")))
    df <- read.csv(f, stringsAsFactors = FALSE)
    datatable(df[rev(seq_len(nrow(df))), ], rownames = FALSE, options = list(pageLength = 8, dom = "tip"))
  })

  # ===== auditar lista =====
  datos <- reactiveVal(NULL)
  observeEvent(input$demo, {
    if (!file.exists("data/ejemplo_cuentas.csv")) try(source("scripts/gen_ejemplo.R"), silent = TRUE)
    datos(cargar_csv("data/ejemplo_cuentas.csv", "data/ejemplo_tweets.csv"))
  })
  observeEvent(input$f_cuentas, {
    req(input$f_cuentas)
    tw <- if (!is.null(input$f_tweets)) read.csv(input$f_tweets$datapath, stringsAsFactors = FALSE) else NULL
    datos(list(cuentas = read.csv(input$f_cuentas$datapath, stringsAsFactors = FALSE), tweets = tw))
  })
  resultado <- reactive({ req(datos()); auditar(datos()$cuentas, datos()$tweets) })
  output$k_n   <- renderText({ req(resultado()); resultado()$resumen$n_cuentas })
  output$k_pct <- renderText({ req(resultado()); r <- resultado()$resumen
    paste0(r$pct_senal_fuerte, "%  (IC ", r$ic95_inf, "–", r$ic95_sup, ")") })
  output$k_coord <- renderText({ req(resultado()); paste0(resultado()$resumen_coord$pct_coordinadas, "%") })
  output$k_clus<- renderText({ req(resultado())
    ct <- resultado()$cotweet; cu <- resultado()$courl
    (if (is.null(ct)) 0 else nrow(ct$clusters)) + (if (is.null(cu)) 0 else nrow(cu$clusters)) })

  output$plot_dist <- renderPlot({
    req(resultado())
    ggplot(resultado()$scored, aes(score_inaut)) +
      geom_histogram(binwidth = 0.05, fill = AZ, color = "white") +
      geom_vline(xintercept = 0.4, linetype = "dashed", color = NAR, linewidth = 0.8) +
      labs(x = "Índice de inautenticidad", y = "Cuentas") +
      theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank())
  })
  output$tabla <- renderDT({
    req(resultado())
    resultado()$scored %>%
      select(handle, edad_dias, followers, following, tweets_por_dia, ff_ratio, n_flags, score_inaut, banda) %>%
      arrange(desc(score_inaut)) %>%
      datatable(rownames = FALSE, options = list(pageLength = 12)) %>%
      formatStyle("banda", target = "row",
        backgroundColor = styleEqual("Alta señal de automatización", "#FFEDE6"))
  })
  output$tabla_cot <- renderDT({
    req(resultado()); ct <- resultado()$cotweet
    if (is.null(ct) || nrow(ct$clusters) == 0)
      return(datatable(data.frame(Mensaje = "Sin clústeres (¿cargaste tweets?)"), rownames = FALSE, options = list(dom = "t")))
    datatable(ct$clusters, rownames = FALSE, options = list(pageLength = 8))
  })
  output$tabla_courl <- renderDT({
    req(resultado()); cu <- resultado()$courl
    if (is.null(cu) || nrow(cu$clusters) == 0)
      return(datatable(data.frame(Mensaje = "Sin co-URL (el demo no trae links; aparece con datos reales)"),
                       rownames = FALSE, options = list(dom = "t")))
    datatable(cu$clusters, rownames = FALSE, options = list(pageLength = 8))
  })
  output$tabla_coh <- renderDT({
    req(resultado()); ch <- resultado()$cohortes
    if (nrow(ch) == 0) return(datatable(data.frame(Mensaje = "Sin cohortes anómalas"), rownames = FALSE, options = list(dom = "t")))
    datatable(ch, rownames = FALSE, options = list(dom = "tp"))
  })
  output$dl <- downloadHandler(
    filename = function() paste0("auditoria_", Sys.Date(), ".csv"),
    content = function(file) write.csv(resultado()$scored, file, row.names = FALSE))

  output$metodo <- renderUI(HTML(sprintf("
    <h4 class='hero' style='color:%s'>CÓMO FUNCIONA</h4>
    <p>Son <b>dos fenómenos distintos</b>: <b>bots automatizados</b> (se ven en el perfil:
    cuenta nueva, hiperactividad, ratio seguidos/seguidores, handle aleatorio, avatar por defecto)
    y <b>bodegas / cuentas coordinadas</b> (se ven en el grupo: mismo texto publicado por muchas
    cuentas en segundos, cohortes creadas el mismo día). Un detector de bots solo no pilla a los bodegueros.</p>
    <p><b>Lectura honesta:</b> reporta <i>'X%% de las cuentas muestran ≥3 señales fuertes (IC 95%%)'</i>,
    nunca 'son bots'. La incertidumbre se comunica siempre.</p>
    <p class='nota'>Marco de literatura y pesos: <a href='%s/blob/main/CATEGORIAS.md' target='_blank'>CATEGORIAS.md</a> ·
    <a href='%s/blob/main/METODOLOGIA.md' target='_blank'>METODOLOGIA.md</a>. Código abierto en GitHub.</p>", AZ, REPO, REPO)))
}

shinyApp(ui, server)
