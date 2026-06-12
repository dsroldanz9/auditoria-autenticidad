# app.R — Aplicativo online de auditoría de autenticidad.
# Despliegue: rsconnect::deployApp()  ->  *.shinyapps.io
# Subes un CSV de cuentas (y opcional de tweets) y obtienes score + coordinación.

suppressPackageStartupMessages({
  library(shiny); library(DT); library(ggplot2); library(dplyr)
})
for (f in c("features.R","score.R","coordination.R","connectors.R","pipeline.R"))
  source(file.path("R", f))

AZ <- "#2839BE"; NAR <- "#FF5A1E"; ORO <- "#F7A40D"; INK <- "#23263A"

ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf("
    body{font-family:'Segoe UI',sans-serif;color:%s;background:#F4F6FC}
    .titulo{font-weight:800;font-size:26px;color:%s}
    .sub{color:#6B6F7A;margin-bottom:14px}
    .kpi{background:#fff;border:1px solid #E0E4F2;border-radius:12px;padding:14px 18px;margin:6px 0}
    .kpi .n{font-size:30px;font-weight:800;color:%s}
    .nota{font-size:12px;color:#6B6F7A}
  ", INK, AZ, AZ)))),
  div(class="titulo", "Auditoría de autenticidad en X"),
  div(class="sub", "Mide señales de automatización y comportamiento coordinado. ",
      tags$b("No prueba que una cuenta sea un bot"), ": estima probabilidad con criterios transparentes."),
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("f_cuentas", "CSV de cuentas", accept = ".csv"),
      helpText("Columnas: handle, created_at, followers, following, n_tweets (+ bio, default_avatar, default_profile, verified)"),
      fileInput("f_tweets", "CSV de tweets (opcional)", accept = ".csv"),
      helpText("Columnas: handle, created_at, text"),
      hr(),
      sliderInput("umbral", "Flags para 'señal fuerte'", 2, 6, 3, 1),
      sliderInput("ventana", "Ventana co-tweet (seg)", 30, 600, 120, 30),
      sliderInput("mincta", "Mín. cuentas en clúster", 2, 10, 3, 1),
      hr(),
      actionButton("demo", "Usar datos de ejemplo", class = "btn-primary"),
      br(), br(),
      downloadButton("dl", "Descargar resultados (CSV)")
    ),
    mainPanel(width = 9,
      fluidRow(
        column(3, div(class="kpi", div(class="n", textOutput("k_n", inline=TRUE)), "cuentas")),
        column(3, div(class="kpi", div(class="n", textOutput("k_pct", inline=TRUE)), "% señal fuerte")),
        column(3, div(class="kpi", div(class="n", textOutput("k_ic", inline=TRUE)), "IC 95%")),
        column(3, div(class="kpi", div(class="n", textOutput("k_clus", inline=TRUE)), "clústeres coord."))
      ),
      tabsetPanel(
        tabPanel("Distribución", br(), plotOutput("plot_dist", height="320px"),
                 div(class="nota", "Distribución del índice de inautenticidad (0-1). Las barras de la derecha concentran cuentas con muchas señales.")),
        tabPanel("Cuentas", br(), DTOutput("tabla")),
        tabPanel("Coordinación (co-tweet)", br(),
                 div(class="nota","Textos idénticos publicados por varias cuentas en una ventana corta = comportamiento coordinado. La evidencia más fuerte de apoyo inflado."),
                 br(), DTOutput("tabla_cot")),
        tabPanel("Cohortes de creación", br(),
                 div(class="nota","Días con un número anómalo de cuentas creadas (z-score ≥ 3): posibles granjas registradas en lote."),
                 br(), DTOutput("tabla_coh")),
        tabPanel("Metodología", br(), uiOutput("metodo"))
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL)

  observeEvent(input$demo, {
    if (!file.exists("data/ejemplo_cuentas.csv")) source("scripts/gen_ejemplo.R")
    datos(cargar_csv("data/ejemplo_cuentas.csv", "data/ejemplo_tweets.csv"))
  })
  observeEvent(input$f_cuentas, {
    req(input$f_cuentas)
    tw <- if (!is.null(input$f_tweets)) read.csv(input$f_tweets$datapath, stringsAsFactors=FALSE) else NULL
    datos(list(cuentas = read.csv(input$f_cuentas$datapath, stringsAsFactors=FALSE), tweets = tw))
  })

  resultado <- reactive({
    req(datos())
    auditar(datos()$cuentas, datos()$tweets,
            ventana_seg = input$ventana, min_cuentas = input$mincta, umbral_fuerte = input$umbral)
  })

  output$k_n   <- renderText({ req(resultado()); resultado()$resumen$n_cuentas })
  output$k_pct <- renderText({ req(resultado()); paste0(resultado()$resumen$pct_senal_fuerte, "%") })
  output$k_ic  <- renderText({ req(resultado()); r<-resultado()$resumen; paste0(r$ic95_inf,"-",r$ic95_sup) })
  output$k_clus<- renderText({ req(resultado()); ct<-resultado()$cotweet; if(is.null(ct)) 0 else nrow(ct$clusters) })

  output$plot_dist <- renderPlot({
    req(resultado())
    ggplot(resultado()$scored, aes(score_inaut)) +
      geom_histogram(binwidth=0.05, fill=AZ, color="white") +
      geom_vline(xintercept=0.4, linetype="dashed", color=NAR) +
      labs(x="Índice de inautenticidad", y="Cuentas") +
      theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank())
  })

  output$tabla <- renderDT({
    req(resultado())
    resultado()$scored %>%
      select(handle, edad_dias, followers, following, tweets_por_dia, ff_ratio,
             n_flags, score_inaut, banda) %>%
      arrange(desc(score_inaut)) %>%
      datatable(rownames=FALSE, options=list(pageLength=15)) %>%
      formatStyle("banda", target="row",
                  backgroundColor=styleEqual("Alta señal de automatización", "#FFEBE3"))
  })
  output$tabla_cot <- renderDT({
    req(resultado()); ct<-resultado()$cotweet
    if (is.null(ct) || nrow(ct$clusters)==0) return(datatable(data.frame(Mensaje="Sin clústeres (¿cargaste tweets?)")))
    datatable(ct$clusters, rownames=FALSE, options=list(pageLength=10))
  })
  output$tabla_coh <- renderDT({
    req(resultado()); ch<-resultado()$cohortes
    if (nrow(ch)==0) return(datatable(data.frame(Mensaje="Sin cohortes anómalas")))
    datatable(ch, rownames=FALSE)
  })

  output$dl <- downloadHandler(
    filename = function() paste0("auditoria_", Sys.Date(), ".csv"),
    content = function(file) write.csv(resultado()$scored, file, row.names=FALSE))

  output$metodo <- renderUI({
    HTML("<div style='max-width:760px'>
      <p>El índice combina señales observables con <b>pesos explícitos</b> (ver <code>R/score.R</code>):
      cuenta muy nueva, hiperactividad, ratio seguidos/seguidores, handle aleatorio, avatar por defecto,
      casi solo retweets, contenido repetido y actividad sin descanso.</p>
      <p><b>Lectura honesta:</b> reportar como <i>'X% de las cuentas muestran ≥3 señales fuertes
      (IC 95% …)'</i>, nunca como 'son bots'. La <b>coordinación</b> (mismo texto, muchas cuentas,
      segundos de diferencia) es la evidencia más robusta y la más difícil de refutar.</p>
      <p class='nota'>Limitaciones: sin verdad de campo no hay precisión garantizada; los umbrales son
      auditables y ajustables; cuentas legítimas muy activas pueden marcar flags (por eso se revisa el detalle).</p>
    </div>")
  })
}

shinyApp(ui, server)
