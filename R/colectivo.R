# colectivo.R — Inteligencia colectiva: narrativa, red de desinformación y consolidación.
# Trabaja sobre las investigaciones acumuladas en la sesión (lista de resultados de auditar_handle).

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

STOPWORDS_ES <- c("de","la","que","el","en","y","a","los","del","se","las","por","un","para",
  "con","no","una","su","al","es","lo","como","mas","más","pero","sus","le","ya","o","este",
  "si","porque","esta","entre","cuando","muy","sin","sobre","tambien","también","me","hasta",
  "hay","donde","quien","desde","todo","nos","durante","todos","uno","les","ni","contra","otros",
  "ese","eso","ante","ellos","esto","mi","antes","esa","estos","mucho","nada","esos","esas",
  "este","sera","ser","son","fue","han","ha","muy","aqui","aquí","va","van","asi","así","cada",
  "rt","http","https","co","el","ella","él","tu","te","nos","ese")

#' Top de palabras y hashtags de la narrativa (de los tweets de una cuenta).
extraer_narrativa <- function(tweets, n = 12) {
  vacio <- list(palabras = data.frame(termino=character(), n=integer()),
                hashtags = data.frame(termino=character(), n=integer()))
  if (is.null(tweets) || nrow(tweets) == 0) return(vacio)
  txt <- tolower(paste(tweets$text, collapse = " "))
  hs <- unlist(regmatches(txt, gregexpr("#[a-záéíóúñ0-9_]+", txt)))
  t2 <- gsub("https?://\\S+", " ", txt); t2 <- gsub("[@#][a-z0-9_]+", " ", t2)
  t2 <- gsub("[^a-záéíóúñü ]", " ", t2)
  w <- unlist(strsplit(t2, "\\s+")); w <- w[nchar(w) >= 4 & !(w %in% STOPWORDS_ES)]
  tab <- function(v, k) { if (!length(v)) return(data.frame(termino=character(), n=integer()))
    d <- as.data.frame(sort(table(v), decreasing = TRUE)); names(d) <- c("termino","n")
    utils::head(d, k) }
  list(palabras = tab(w, n), hashtags = tab(hs, n))
}

#' Consolida el TOP de amplificadores de TODAS las investigaciones de la sesión.
#' @param invs lista; cada elemento tiene $top (cuenta,n,bando).
#' @return data.frame cuenta, n_total, bando, veces (en cuántas cuentas aparece), ordenado.
consolidar_amplificadores <- function(invs) {
  tops <- lapply(invs, function(x) x$top)
  tops <- tops[!vapply(tops, is.null, logical(1)) & vapply(tops, function(t) !is.null(t) && nrow(t) > 0, logical(1))]
  if (length(tops) == 0) return(data.frame())
  all <- bind_rows(tops)
  all %>% group_by(cuenta) %>%
    summarise(n_total = sum(n), veces = n(),
              bando = dplyr::first(bando[!is.na(bando)]), nombre = dplyr::first(nombre[!is.na(nombre)]),
              .groups = "drop") %>%
    arrange(desc(n_total))
}

#' Consolida la narrativa (palabras+hashtags) de todas las investigaciones.
consolidar_narrativa <- function(invs, n = 15) {
  jp <- bind_rows(lapply(invs, function(x) x$narrativa$palabras))
  jh <- bind_rows(lapply(invs, function(x) x$narrativa$hashtags))
  agg <- function(d) { if (is.null(d) || nrow(d) == 0) return(data.frame(termino=character(), n=integer()))
    d %>% group_by(termino) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(desc(n)) %>% head(n) }
  list(palabras = agg(jp), hashtags = agg(jh))
}

#' Construye nodos y aristas de la red de desinformación a partir de las investigaciones.
#' Aristas: cuenta_analizada -> cuenta_que_amplifica/ataca (peso = nº interacciones).
construir_red <- function(invs) {
  edges <- bind_rows(lapply(invs, function(x) {
    if (is.null(x$top) || nrow(x$top) == 0) return(NULL)
    data.frame(from = paste0("@", x$handle), to = x$top$cuenta, weight = x$top$n,
               bando_to = x$top$bando, stringsAsFactors = FALSE)
  }))
  if (is.null(edges) || nrow(edges) == 0) return(list(nodes = data.frame(), edges = data.frame()))
  bandos <- setNames(edges$bando_to, edges$to)
  analizadas <- unique(edges$from)
  ids <- unique(c(edges$from, edges$to))
  nodes <- data.frame(id = ids, stringsAsFactors = FALSE)
  nodes$bando <- ifelse(nodes$id %in% names(bandos), bandos[nodes$id], NA)
  nodes$tipo  <- ifelse(nodes$id %in% analizadas, "analizada", "amplificada")
  deg <- table(c(edges$to, edges$from))
  nodes$grado <- as.integer(deg[nodes$id]); nodes$grado[is.na(nodes$grado)] <- 1
  list(nodes = nodes, edges = edges)
}

#' Dibuja la red con igraph (layout) + plotly (interactivo).
plot_red <- function(red) {
  if (is.null(red) || nrow(red$nodes) == 0) return(NULL)
  nodes <- red$nodes; edges <- red$edges
  g <- igraph::graph_from_data_frame(edges[, c("from","to")], vertices = nodes$id, directed = TRUE)
  set.seed(7); L <- igraph::layout_with_fr(g)
  pos <- data.frame(id = igraph::V(g)$name, x = L[,1], y = L[,2], stringsAsFactors = FALSE)
  nodes <- dplyr::left_join(nodes, pos, by = "id")
  ex <- numeric(); ey <- numeric()
  for (i in seq_len(nrow(edges))) {
    a <- nodes[nodes$id == edges$from[i], ]; b <- nodes[nodes$id == edges$to[i], ]
    ex <- c(ex, a$x, b$x, NA); ey <- c(ey, a$y, b$y, NA)
  }
  colb <- function(b, tipo) ifelse(tipo == "analizada", "#2839BE",
            ifelse(is.na(b), "#9AA0BF", ifelse(b == "derecha", "#FF4403",
              ifelse(b == "pacto", "#16A34A", "#9AA0BF"))))
  nodes$color <- colb(nodes$bando, nodes$tipo)
  nodes$size <- 12 + 4 * pmin(nodes$grado, 8)
  plotly::plot_ly() |>
    plotly::add_trace(x = ex, y = ey, type = "scatter", mode = "lines",
      line = list(color = "#C9CEEA", width = 1), hoverinfo = "none", showlegend = FALSE) |>
    plotly::add_trace(x = nodes$x, y = nodes$y, type = "scatter", mode = "markers+text",
      text = nodes$id, textposition = "top center", textfont = list(size = 10),
      marker = list(color = nodes$color, size = nodes$size, line = list(color = "white", width = 1)),
      hovertext = paste0(nodes$id, ifelse(is.na(nodes$bando), "", paste0(" · ", nodes$bando))),
      hoverinfo = "text", showlegend = FALSE) |>
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      margin = list(t = 10, b = 10, l = 10, r = 10), paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE)
}
