# =============================================================================
# SCM Course — Hilfsfunktionen für Darstellung, Formatierung und Konvertierung
# Prof. Dr. Thomas Kirschstein | HS RheinMain
# Bachelor Supply Chain Management & Distribution
# =============================================================================
# Ergänzt R/scm_functions.R (Fachmodelle und Algorithmen). Beide Dateien werden
# in der Präambel jeder Einheit geladen:
#   source("../R/scm_functions.R")
#   source("../R/scm_helpers.R")
#
# Abschnitte:
#   1. Farben und Zeichenhelfer für Blockdiagramme
#   2. Zahlenformatierung
#   3. Tabellen (kableExtra)
#   4. plotly-Layouts
#   5. Koordinaten und Beschriftungen
#   6. Grafiken und Tableaus der einzelnen Einheiten
#
# Hinweis: Einige Grafikfunktionen greifen per Voreinstellung auf Objekte zu,
# die in der Präambel der jeweiligen Einheit angelegt werden (z. B. sp_nodes,
# slots, maerkte). Diese Abhängigkeiten stehen als Argumente mit Standardwert
# in der Funktionssignatur und lassen sich bei Bedarf explizit übergeben.
# Benötigte Pakete: knitr, kableExtra, plotly, ggplot2, dplyr, scales, xml2.
# =============================================================================


# ─── 1. FARBEN UND ZEICHENHELFER ───────────────────────────────────────────────

# Farbdefinitionen

cols <- list(
  top    = "#0B6E4F",
  strat  = "#2D6A4F",
  mid    = "#52B788",
  low    = "#FFFFFF",
  atp    = "#D97B29",
  border = "#2F3E46",
  text   = "#1F2933",
  muted  = "#6B7280",
  dash   = "#7C8792",
  bg     = "#FFFFFF"
)

# Zeichenfunktionen Blockdiagramme

mid <- function(a, b) (a + b) / 2

chevron_path <- function(x0, x1, y0, y1, head = 0.28, notch = 0.24) {
  xm <- x1 - head
  paste0(
    "M ", x0, ",", y0, " ",
    "L ", xm, ",", y0, " ",
    "L ", x1, ",", mid(y0, y1), " ",
    "L ", xm, ",", y1, " ",
    "L ", x0, ",", y1, " ",
    "L ", x0 + notch, ",", mid(y0, y1), " Z"
  )
}

shape_rect <- function(x0, x1, y0, y1, fill, line = cols$border, width = 1.2) {
  list(
    type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    xref = "x", yref = "y",
    line = list(color = line, width = width),
    fillcolor = fill,
    layer = "below"
  )
}

shape_path <- function(path, fill, line = cols$border, width = 1.2) {
  list(
    type = "path", path = path, xref = "x", yref = "y",
    line = list(color = line, width = width),
    fillcolor = fill,
    layer = "below"
  )
}

shape_line <- function(x0, x1, y0, y1, color, width = 2) {
  list(
    type = "line", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    xref = "x", yref = "y",
    line = list(color = color, width = width)
  )
}

text_trace <- function(data, x, y, text, color, size = 16) {
  add_text(
    fig, data = data,
    x = x, y = y, text = text,
    textfont = list(size = size, color = color, family = "Arial"),
    hoverinfo = "skip", showlegend = FALSE
  )
}

arrow_ann <- function(x, y, ax, ay, dash = FALSE,
                      color = cols$border, width = 1.5) {
  list(
    x = x, y = y, ax = ax, ay = ay,
    xref = "x", yref = "y", axref = "x", ayref = "y",
    text = "", showarrow = TRUE,
    arrowhead = 2, arrowsize = 1,
    arrowwidth = width, arrowcolor = color,
    arrowdash = if (dash) "dash" else "solid"
  )
}
kable_zeilenweise <- function(data,
                              fragment_typ = "fade-up",
                              start_index = 1,
                              ...) {
  
  tab_html <- knitr::kable(
    data,
    format = "html",
    escape = FALSE,
    ...
  )
  
  doc <- xml2::read_html(as.character(tab_html))
  
  zeilen <- xml2::xml_find_all(
    doc,
    ".//tbody/tr"
  )
  
  for (i in seq_along(zeilen)) {
    
    xml2::xml_attr(
      zeilen[i],
      "class"
    ) <- paste(
      "fragment",
      fragment_typ
    )
    
    xml2::xml_attr(
      zeilen[i],
      "data-fragment-index"
    ) <- start_index + i - 1
  }
  
  knitr::asis_output(
    as.character(
      xml2::xml_find_first(
        doc,
        ".//table"
      )
    )
  )
}


# ─── 2. ZAHLENFORMATIERUNG ─────────────────────────────────────────────────────

#' Zahl im deutschen Format (Dezimalkomma, Tausenderpunkt)
#' @param x Numerischer Vektor.
#' @param d Anzahl Nachkommastellen (Standard 2).
#' @return Zeichenkette, z. B. fmt(1234.5) -> "1.234,50".
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d,
                                   big.mark = ".", decimal.mark = ",")

#' Betrag in Euro, deutsch formatiert
#' @param x Numerischer Vektor.
#' @param d Nachkommastellen (Standard 0).
#' @return Zeichenkette, z. B. eur(27083) -> "27.083 €".
eur <- function(x, d = 0) paste0(fmt(x, d), " €")


# ─── 3. TABELLEN ───────────────────────────────────────────────────────────────

#' Tabelle im Kurslayout (dunkle Kopfzeile, optional dunkle erste Spalte)
#' @param df Data frame.
#' @param ... Weitere Argumente an kableExtra::kbl().
#' @param font_size Schriftgröße in px.
#' @param first_col Erste Spalte hervorheben?
#' @param col1_bg Hintergrundfarbe der ersten Spalte.
#' Eine Spalte "Kueche" wird für die Anzeige in "Küche" umbenannt.
kbl_sf <- function(df, ..., font_size = 22, first_col = TRUE,
                   col1_bg = "#2F3E46") {
  names(df)[names(df) == "Kueche"] <- "K\u00fcche"
  out <- kbl(df, ...) |>
    kable_styling(bootstrap_options = c("condensed", "striped"),
                  full_width = TRUE, font_size = font_size) |>
    row_spec(0, background = "#2F3E46", color = "white", bold = TRUE)
  if (first_col) out <- out |>
    column_spec(1, bold = TRUE, background = col1_bg, color = "white")
  out
}


# ─── 4. PLOTLY-LAYOUTS ─────────────────────────────────────────────────────────

#' Einheitliches plotly-Layout (Einheiten 12–14)
#' @param p plotly-Objekt.
#' @param xt,yt Achsentitel.
#' @param h,w Höhe und Breite in px.
#' @param legend Legende anzeigen?
plt_layout <- function(p, xt = "", yt = "", h = 480, w = 760, legend = TRUE) {
  p |> layout(xaxis = list(title = xt), yaxis = list(title = yt),
              showlegend = legend,
              legend = list(orientation = "h", x = 0, y = -0.18),
              margin = list(l = 70, r = 20, t = 20, b = 60),
              plot_bgcolor = "white", paper_bgcolor = "white",
              height = h, width = w)
}

#' plotly-Layout mit Titel (Einheit Bullwhip)
#' @param p plotly-Objekt.
#' @param title,x_title,y_title Titel.
#' @param showlegend Legende anzeigen?
plotly_layout <- function(p,
                          title = NULL,
                          x_title = NULL,
                          y_title = NULL,
                          showlegend = TRUE) {
  p |>
    layout(
      title = list(text = title, x = 0),
      xaxis = list(title = x_title),
      yaxis = list(title = y_title),
      showlegend = showlegend,
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.15
      ),
      margin = list(l = 70, r = 30, b = 70, t = 70),
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    )
}


# ─── 5. KOORDINATEN UND BESCHRIFTUNGEN ─────────────────────────────────────────

#' Längen- bzw. Breitengrad in planare km-Koordinaten umrechnen
#' (Näherung für das Rhein-Main-Gebiet, Bezugsbreite ca. 50,4° N)
km_x <- function(lon) lon * 111.32 * cos(50.4 * pi / 180)
km_y <- function(lat) lat * 111.32

#' Tour als Text, z. B. "R → C4 → C2 → R" (Rückkehr zum Start wird ergänzt)
tour_str <- function(tour) paste(c(tour, tour[1]), collapse = " → ")


# ─── 6. GRAFIKEN UND TABLEAUS DER EINHEITEN ────────────────────────────────────


# Newsvendor-Modell ----------------------------------------------------------

#' Dichte der Nachfrage mit Bestellmenge und schraffierten Bereichen
#' @param mean_demand,sd_demand Parameter der Normalverteilung.
#' @param order_quantity Bestellmenge Q.
#' @param critical_ratio Kritisches Fraktil für den Untertitel (optional).
plot_newsvendor_distribution <- function(
    mean_demand,
    sd_demand,
    order_quantity,
    critical_ratio = NULL,
    title = "Newsvendor-Modell: Nachfrage und Bestellmenge"
) {
  stopifnot(
    is.finite(mean_demand),
    is.finite(sd_demand),
    sd_demand > 0,
    is.finite(order_quantity)
  )

  x_min <- min(0, mean_demand - 4 * sd_demand)
  x_max <- max(order_quantity, mean_demand + 4 * sd_demand)

  demand <- tibble(
    demand = seq(x_min, x_max, length.out = 1000),
    density = dnorm(demand, mean = mean_demand, sd = sd_demand)
  )

  left_area <- demand |> filter(demand <= order_quantity)
  right_area <- demand |> filter(demand >= order_quantity)

  ggplot(demand, aes(x = demand, y = density)) +
    geom_area(
      data = left_area,
      fill = "#00843D",
      alpha = 0.25
    ) +
    geom_area(
      data = right_area,
      fill = "#E85D04",
      alpha = 0.20
    ) +
    geom_line(
      linewidth = 1.1,
      colour = "#1F2937"
    ) +
    geom_vline(
      xintercept = mean_demand,
      linetype = "dashed",
      colour = "#6B7280"
    ) +
    geom_vline(
      xintercept = order_quantity,
      linewidth = 1.2,
      colour = "#00843D"
    ) +
    annotate(
      "text",
      x = order_quantity,
      y = max(demand$density) * 0.95,
      label = paste0(
        "Q* = ",
        format(
          round(order_quantity, 0),
          big.mark = ".",
          scientific = FALSE
        )
      ),
      hjust = -0.05,
      colour = "#00843D",
      fontface = "bold"
    ) +
    labs(
      title = title,
      subtitle = if (is.null(critical_ratio)) {
        NULL
      } else {
        paste0(
          "Kritische Fraktil: ",
          number(
            critical_ratio,
            accuracy = 0.001,
            decimal.mark = ","
          )
        )
      },
      x = "Nachfrage",
      y = "Dichte"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Transportplanung -----------------------------------------------------------

#' Transporttableau einer VAM-Iteration als HTML-Tabelle
#' @param step Ein Element von vam_trace()$steps.
#' @param cost Kostenmatrix.
vam_tableau <- function(step, cost, font_size = 20) {
  m <- nrow(cost); n <- ncol(cost)
  grey   <- function(x) paste0("<span style='color:#B8BEC6'>", x, "</span>")
  mark   <- function(x) paste0("<span style='background:#F4A259;padding:2px 6px;",
                               "border-radius:4px;font-weight:bold'>", x, "</span>")
  alloc_f <- function(c, a) paste0(sprintf("%.3f", c),
                                   " <b style='color:#0B6E4F'>[", a, "]</b>")
  body <- matrix("", m, n)
  for (i in 1:m) for (j in 1:n) {
    active <- (i %in% step$ar) && (j %in% step$ac)
    a <- step$alloc_before[i, j]
    txt <- if (a > 0) alloc_f(cost[i, j], a) else sprintf("%.3f", cost[i, j])
    if (!active && a == 0) txt <- grey(txt)
    if (i == step$i && j == step$j) txt <- mark(paste0(sprintf("%.3f", cost[i, j]),
                                                        " → ", step$q))
    body[i, j] <- txt
  }
  # Restangebot und Zeilen-Penalties
  s_col <- sapply(1:m, function(i) if (i %in% step$ar) step$sup[i] else grey(0))
  rp_col <- sapply(1:m, function(i) {
    if (!(i %in% step$ar)) return(grey("–"))
    v <- step$rp[rownames(cost)[i]]
    txt <- sprintf("%.3f", v)
    if (step$typ == "Zeile" && i == step$i) txt <- mark(txt)
    txt
  })
  d_row <- sapply(1:n, function(j) if (j %in% step$ac) step$dem[j] else grey(0))
  cp_row <- sapply(1:n, function(j) {
    if (!(j %in% step$ac)) return(grey("–"))
    v <- step$cp[colnames(cost)[j]]
    txt <- sprintf("%.3f", v)
    if (step$typ == "Spalte" && j == step$j) txt <- mark(txt)
    txt
  })
  df <- data.frame(
    rbind(cbind(rownames(cost), body, s_col, rp_col),
          c("a<sub>j</sub> (Rest)", d_row, "", ""),
          c("Δ Spalte", cp_row, "", "")),
    stringsAsFactors = FALSE
  )
  names(df) <- c("", colnames(cost), "s<sub>i</sub> (Rest)", "Δ Zeile")
  rownames(df) <- NULL
  kbl(df, escape = FALSE, align = c("l", rep("c", n), "c", "c")) |>
    kable_styling(bootstrap_options = c("condensed"), full_width = TRUE,
                  font_size = font_size) |>
    row_spec(0, background = "#2F3E46", color = "white", bold = TRUE) |>
    column_spec(1, bold = TRUE, background = "#2F3E46", color = "white") |>
    row_spec(m + 1:2, background = "#EEF2F4", color = "#1F2933")
}

#' Flussdarstellung (bipartiter Graph) einer Allokation
#' @param alloc Transportmatrix.
#' @param cost Kostenmatrix (für Beschriftungen).
tp_flow_plot <- function(alloc, cost, title = NULL, height = 470, width = 700) {
  m <- nrow(alloc); n <- ncol(alloc)
  kp <- data.frame(id = rownames(alloc), x = 0.18,
                   y = seq(0.85, 0.15, length.out = m))
  fp <- data.frame(id = colnames(alloc), x = 0.80,
                   y = seq(0.95, 0.05, length.out = n))
  fig <- plot_ly(type = "scatter", mode = "markers")
  qmax <- max(alloc)
  for (i in 1:m) for (j in 1:n) {
    if (alloc[i, j] <= 0) next
    w <- 1.5 + 9 * alloc[i, j] / qmax
    fig <- fig |> add_segments(x = kp$x[i], xend = fp$x[j],
                               y = kp$y[i], yend = fp$y[j],
                               line = list(color = "#52B788", width = w),
                               hoverinfo = "text", showlegend = FALSE,
                               text = paste0(rownames(alloc)[i], " → ",
                                             colnames(alloc)[j], ": ",
                                             alloc[i, j], " Boxen"))
    fig <- fig |> add_annotations(
      x = kp$x[i] + 0.62 * (fp$x[j] - kp$x[i]),
      y = kp$y[i] + 0.62 * (fp$y[j] - kp$y[i]),
      text = paste0("<b>", alloc[i, j], "</b>"), showarrow = FALSE,
      bgcolor = "white", font = list(size = 13, color = "#0B6E4F"))
  }
  fig |>
    add_trace(x = kp$x, y = kp$y, text = kp$id, mode = "markers+text",
              marker = list(symbol = "hexagon", size = 44, color = "#BFDBF7",
                            line = list(color = "#4B5563", width = 1.5)),
              textfont = list(size = 15, color = "#1F2933"),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_trace(x = fp$x, y = fp$y, text = fp$id, mode = "markers+text",
              marker = list(symbol = "circle", size = 40, color = "#2D9E8E",
                            line = list(color = "#2F3E46", width = 1.5)),
              textfont = list(size = 15, color = "white"),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_annotations(x = c(0.18, 0.80), y = c(1.06, 1.06),
                    text = c("<b>Küchen <i>i</i></b>", "<b>Filialen <i>j</i></b>"),
                    showarrow = FALSE, font = list(size = 14)) |>
    layout(title = title,
           xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
           yaxis = list(visible = FALSE, range = c(-0.03, 1.1), fixedrange = TRUE),
           plot_bgcolor = "transparent", paper_bgcolor = "transparent",
           margin = list(l = 10, r = 10, t = 10, b = 10),
           height = height, width = width)
}

# Kürzeste Wege --------------------------------------------------------------

#' Netzwerkgrafik des Straßennetzes, optional mit Dijkstra-Zustand
#' @param state Schritt aus dijkstra_trace()$steps oder NULL.
#' @param knoten,kanten Knoten- und Kantentabelle (Standard: sp_nodes, sp_edges).
sp_plot <- function(state = NULL, tree = FALSE, highlight_path = NULL,
                    show_km = TRUE, height = 590, width = 900,
                    removed = NULL, knoten = sp_nodes,
                    kanten = sp_edges) {
  nd <- knoten
  xy <- split(nd[, c("x", "y")], nd$ID)
  fig <- plot_ly(type = "scatter", mode = "markers")
  # Kanten
  tree_edges <- character(0)
  if (tree && !is.null(state)) {
    for (v in names(state$pred)) if (!is.na(state$pred[v]) && state$perm[v])
      tree_edges <- c(tree_edges, paste(sort(c(v, state$pred[v])), collapse = "-"))
  }
  path_edges <- character(0)
  if (!is.null(highlight_path) && length(highlight_path) > 1)
    for (k in 2:length(highlight_path))
      path_edges <- c(path_edges, paste(sort(highlight_path[(k - 1):k]),
                                         collapse = "-"))
  for (e in seq_len(nrow(kanten))) {
    a <- kanten$from[e]; b <- kanten$to[e]
    key <- paste(sort(c(a, b)), collapse = "-")
    is_removed <- !is.null(removed) && key %in% removed
    col <- if (key %in% path_edges) "#D97B29" else if (key %in% tree_edges)
      "#0B6E4F" else "#B8BEC6"
    wd  <- if (key %in% c(path_edges, tree_edges)) 5 else 2
    fig <- fig |> add_segments(
      x = xy[[a]]$x, xend = xy[[b]]$x, y = xy[[a]]$y, yend = xy[[b]]$y,
      line = list(color = if (is_removed) "#E15759" else col, width = wd,
                  dash = if (is_removed) "dot" else "solid"),
      hoverinfo = "skip", showlegend = FALSE)
    if (show_km) fig <- fig |> add_annotations(
      x = (xy[[a]]$x + xy[[b]]$x) / 2, y = (xy[[a]]$y + xy[[b]]$y) / 2,
      text = kanten$km[e], showarrow = FALSE, bgcolor = "white",
      font = list(size = 12, color = "#4B5563"))
  }
  # Knotenfarben
  col <- ifelse(nd$Typ == "Knoten", "#D0D5DD",
                ifelse(nd$Typ == "Küche", "#BFDBF7", "#2D9E8E"))
  lin <- rep("#4B5563", nrow(nd)); lw <- rep(1.5, nrow(nd))
  if (!is.null(state)) {
    perm <- state$perm[nd$ID]
    temp <- !perm & is.finite(state$d[nd$ID])
    col[perm] <- "#0B6E4F"
    col[temp] <- "#F4A259"
    lin[nd$ID %in% state$updates] <- "#C00000"; lw[nd$ID %in% state$updates] <- 4
    lin[nd$ID == state$u] <- "#1F2933"; lw[nd$ID == state$u] <- 4
  }
  txtcol <- ifelse(col %in% c("#0B6E4F", "#2D9E8E"), "white", "#1F2933")
  fig <- fig |> add_trace(
    x = nd$x, y = nd$y, text = nd$ID, mode = "markers+text",
    marker = list(symbol = ifelse(nd$Typ == "Knoten", "diamond",
                                  ifelse(nd$Typ == "Küche", "hexagon", "circle")),
                  size = ifelse(nd$Typ == "Knoten", 30, 40), color = col,
                  line = list(color = lin, width = lw)),
    textfont = list(size = 13, color = txtcol),
    hovertext = nd$Name, hoverinfo = "text", showlegend = FALSE)
  # Marken d(v) / Vorgänger
  if (!is.null(state)) {
    lab <- ifelse(is.finite(state$d[nd$ID]),
                  paste0(state$d[nd$ID], " | ", ifelse(is.na(state$pred[nd$ID]),
                                                       "–", state$pred[nd$ID])),
                  "∞")
    fig <- fig |> add_annotations(
      x = nd$x, y = nd$y + 2.6, text = paste0("<b>", lab, "</b>"),
      showarrow = FALSE, font = list(size = 13, color = "#1F2933"),
      bgcolor = "rgba(255,255,255,0.85)")
  } else {
    fig <- fig |> add_annotations(
      x = nd$x, y = nd$y - 2.6, text = nd$Name, showarrow = FALSE,
      font = list(size = 11, color = "#4B5563"))
  }
  fig |> layout(
    xaxis = list(visible = FALSE, fixedrange = TRUE,
                 range = range(nd$x) + c(-4, 4)),
    yaxis = list(visible = FALSE, fixedrange = TRUE,
                 range = range(nd$y) + c(-4, 4.5)),
    plot_bgcolor = "transparent", paper_bgcolor = "transparent",
    margin = list(l = 5, r = 5, t = 5, b = 5),
    height = height, width = width)
}

#' Dijkstra-Tableau (Zeilen = Iterationen) als HTML-Tabelle
#' @param res Ergebnis von dijkstra_trace().
sp_tableau <- function(res, font_size = 20) {
  ids <- names(res$d)
  rows <- lapply(seq_along(res$steps), function(k) {
    s <- res$steps[[k]]
    prev_perm <- if (k == 1) setNames(rep(FALSE, length(ids)), ids) else
      res$steps[[k - 1]]$perm
    cells <- sapply(ids, function(v) {
      if (prev_perm[v]) return("")
      if (v == s$u) return(paste0("<span style='background:#0B6E4F;color:white;",
                                  "padding:2px 6px;border-radius:4px'><b>",
                                  s$d[v], "</b></span>"))
      if (!is.finite(s$d[v])) return("<span style='color:#B8BEC6'>∞</span>")
      txt <- paste0(s$d[v], " | ", s$pred[v])
      if (v %in% s$updates) txt <- paste0("<span style='color:#C00000'><b>",
                                          txt, "</b></span>")
      txt
    })
    c(k, s$u, cells)
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("It.", "perm.", ids)
  rownames(df) <- NULL
  kbl(df, escape = FALSE, align = c("c", "c", rep("c", length(ids)))) |>
    kable_styling(bootstrap_options = c("condensed", "striped"),
                  full_width = TRUE, font_size = font_size) |>
    row_spec(0, background = "#2F3E46", color = "white", bold = TRUE) |>
    column_spec(1:2, bold = TRUE, background = "#2F3E46", color = "white")
}

# Tourenplanung --------------------------------------------------------------

#' Tourgrafik im Straßennetz (Linien = Fahrtabschnitte mit Netzdistanz)
#' @param tour Knotenfolge der Tour oder NULL.
#' @param star Pendelverkehr (Stern ab R) zeichnen?
#' @param highlight,new_edges Hervorzuhebende Kanten ("A-B").
#' @param knoten,kanten,orte,dmat Netzdaten (Standard: tsp_nodes, tsp_edges, stops, D).
tsp_plot <- function(tour = NULL, star = FALSE, highlight = NULL,
                     new_edges = NULL, height = 560, width = 860,
                     edge_labels = TRUE, knoten = tsp_nodes,
                     kanten = tsp_edges, orte = stops, dmat = D) {
  nd <- knoten[knoten$ID %in% orte, ]
  xy <- split(nd[, c("x", "y")], nd$ID)
  fig <- plot_ly(type = "scatter", mode = "markers")
  # Straßennetz im Hintergrund
  for (e in seq_len(nrow(kanten))) {
    a <- knoten[knoten$ID == kanten$from[e], ]
    b <- knoten[knoten$ID == kanten$to[e], ]
    fig <- fig |> add_segments(x = a$x, xend = b$x, y = a$y, yend = b$y,
                               line = list(color = "#E5E7EB", width = 2),
                               hoverinfo = "skip", showlegend = FALSE)
  }
  segs <- NULL
  if (star) segs <- data.frame(a = "R", b = orte[-1])
  if (!is.null(tour)) segs <- data.frame(a = tour, b = c(tour[-1], tour[1]))
  if (!is.null(segs)) for (k in seq_len(nrow(segs))) {
    a <- segs$a[k]; b <- segs$b[k]
    key <- paste(sort(c(a, b)), collapse = "-")
    col <- if (!is.null(highlight) && key %in% highlight) "#C00000" else
      if (!is.null(new_edges) && key %in% new_edges) "#D97B29" else "#0B6E4F"
    dash <- if (!is.null(highlight) && key %in% highlight) "dot" else "solid"
    fig <- fig |> add_segments(
      x = xy[[a]]$x, xend = xy[[b]]$x, y = xy[[a]]$y, yend = xy[[b]]$y,
      line = list(color = col, width = 4, dash = dash),
      hoverinfo = "skip", showlegend = FALSE)
    if (edge_labels) fig <- fig |> add_annotations(
      x = (xy[[a]]$x + xy[[b]]$x) / 2, y = (xy[[a]]$y + xy[[b]]$y) / 2,
      text = paste0("<b>", dmat[a, b], "</b>"), showarrow = FALSE,
      bgcolor = "white", font = list(size = 13, color = col))
  }
  fig |>
    add_trace(x = nd$x, y = nd$y, text = nd$ID, mode = "markers+text",
              marker = list(symbol = ifelse(nd$Typ == "Küche", "hexagon", "circle"),
                            size = 42,
                            color = ifelse(nd$Typ == "Küche", "#BFDBF7", "#2D9E8E"),
                            line = list(color = "#2F3E46", width = 1.5)),
              textfont = list(size = 14,
                              color = ifelse(nd$Typ == "Küche", "#1F2933", "white")),
              hovertext = nd$Name, hoverinfo = "text", showlegend = FALSE) |>
    add_annotations(x = nd$x, y = nd$y - 2.7, text = nd$Name, showarrow = FALSE,
                    font = list(size = 11, color = "#4B5563")) |>
    layout(xaxis = list(visible = FALSE, fixedrange = TRUE,
                        range = range(knoten$x) + c(-4, 4)),
           yaxis = list(visible = FALSE, fixedrange = TRUE,
                        range = range(knoten$y) + c(-4.5, 4)),
           plot_bgcolor = "transparent", paper_bgcolor = "transparent",
           margin = list(l = 5, r = 5, t = 5, b = 5),
           height = height, width = width)
}

# Lager und Kommissionierung -------------------------------------------------

#' Lagerlayout mit ABC-Belegung, Picks und Route
#' @param zuordnung Fachnummer je Artikel (Ergebnis einer Lagerplatzvergabe).
#' @param picks Pickpositionen (Zeilen von slots mit Spalten rx, y, name).
#' @param wp Wegpunkte einer Route (Matrix x, y).
#' @param faecher,kl,farben,gx,y_max Layoutdaten (Standard: slots, klasse,
#'   klassen_farbe, gang_x, Y).
lager_plot <- function(zuordnung = NULL, picks = NULL, wp = NULL,
                       route_farbe = "#C00000", height = 520, width = 760,
                       labels = TRUE, faecher = slots, kl = klasse,
                       farben = klassen_farbe, gx = gang_x, y_max = Y) {
  fach_klasse <- rep(NA, nrow(faecher))
  if (!is.null(zuordnung)) fach_klasse[zuordnung] <- kl
  shapes <- lapply(seq_len(nrow(faecher)), function(i) {
    s <- faecher[i, ]
    list(type = "rect", x0 = s$rx - 0.5, x1 = s$rx + 0.5,
         y0 = s$y - 0.7, y1 = s$y + 0.7,
         fillcolor = if (is.na(fach_klasse[i])) "#E5E7EB" else
           farben[[fach_klasse[i]]],
         line = list(color = "white", width = 1), layer = "below")
  })
  p <- plot_ly(type = "scatter", mode = "markers")
  if (!is.null(wp)) p <- p |> add_trace(
    x = wp[, 1], y = wp[, 2], mode = "lines",
    line = list(color = route_farbe, width = 3), hoverinfo = "skip",
    showlegend = FALSE)
  if (!is.null(picks)) {
    p <- p |> add_trace(
      x = picks$rx, y = picks$y, mode = "markers",
      marker = list(size = 15, color = "#2A7ACA",
                    line = list(color = "white", width = 2)),
      text = picks$name, hoverinfo = "text", showlegend = FALSE)
    if (labels) p <- p |> add_annotations(
      x = picks$rx, y = picks$y, text = seq_len(nrow(picks)),
      showarrow = FALSE, font = list(color = "white", size = 11))
  }
  p |>
    add_trace(x = 0, y = 0, mode = "markers+text", text = "Depot",
              textposition = "bottom center",
              marker = list(symbol = "square", size = 22, color = "#BFDBF7",
                            line = list(color = "#2F3E46", width = 2)),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_annotations(x = gx, y = -1.6, text = paste("Gang", seq_along(gx)),
                    showarrow = FALSE, font = list(size = 11, color = "#4B5563")) |>
    layout(shapes = shapes,
           xaxis = list(visible = FALSE, range = c(-2.5, max(gx) + 2.5),
                        fixedrange = TRUE),
           yaxis = list(visible = FALSE, range = c(-3, y_max + 1),
                        fixedrange = TRUE),
           plot_bgcolor = "transparent", paper_bgcolor = "transparent",
           margin = list(l = 5, r = 5, t = 5, b = 5),
           height = height, width = width)
}

# Distributionsstrukturen und -systeme ---------------------------------------

#' Karte (km-Koordinaten) mit Märkten, Touren und optionalem Hauptlauf
#' @param touren Liste von Indexfolgen (Knoten 1 = Depot).
#' @param ziel Koordinaten eines Hauptlaufziels.
#' @param P,maerkte_df,depot_pt Fallstudiendaten (Standard: P_rm, maerkte, depot).
rm_plot <- function(touren = NULL, ziel = NULL, ziel_name = "",
                    height = 520, width = 640, P = P_rm,
                    maerkte_df = maerkte, depot_pt = depot) {
  p <- plot_ly(type = "scatter", mode = "markers")
  farben <- c("#0B6E4F", "#2A7ACA", "#D97B29", "#9B59B6", "#C00000", "#2D9E8E",
              "#F4A259")
  if (!is.null(touren)) for (k in seq_along(touren)) {
    tt <- c(1, touren[[k]], 1)
    p <- p |> add_trace(x = km_x(P[tt, 1]), y = km_y(P[tt, 2]), mode = "lines",
                        line = list(color = farben[(k - 1) %% 7 + 1], width = 2.5),
                        name = paste("Tour", k), hoverinfo = "skip")
  }
  if (!is.null(ziel)) p <- p |>
    add_trace(x = km_x(c(depot_pt["lon"], ziel["lon"])), y = km_y(c(depot_pt["lat"], ziel["lat"])),
              mode = "lines", line = list(color = "#2F3E46", width = 6),
              name = "Hauptlauf", hoverinfo = "skip") |>
    add_trace(x = km_x(ziel["lon"]), y = km_y(ziel["lat"]), mode = "markers+text",
              text = ziel_name, textposition = "top center",
              marker = list(symbol = "square", size = 22, color = "#F4A259",
                            line = list(color = "#2F3E46", width = 2)),
              showlegend = FALSE, hoverinfo = "skip")
  p |>
    add_trace(x = km_x(maerkte_df$lon), y = km_y(maerkte_df$lat), mode = "markers",
              marker = list(size = 6 + maerkte_df$boxen / 6, color = "#2D9E8E",
                            line = list(color = "white", width = 1)),
              text = paste0(maerkte_df$id, ": ", maerkte_df$boxen, " Boxen"),
              hoverinfo = "text", name = "Markt") |>
    add_trace(x = km_x(depot_pt["lon"]), y = km_y(depot_pt["lat"]), mode = "markers+text",
              text = "Raunheim", textposition = "bottom center",
              marker = list(symbol = "hexagon", size = 24, color = "#BFDBF7",
                            line = list(color = "#2F3E46", width = 2)),
              showlegend = FALSE, hoverinfo = "skip") |>
    layout(xaxis = list(visible = FALSE, fixedrange = TRUE,
                        scaleanchor = "y", scaleratio = 1),
           yaxis = list(visible = FALSE, fixedrange = TRUE),
           legend = list(orientation = "h", x = 0, y = -0.02),
           margin = list(l = 5, r = 5, t = 5, b = 5),
           plot_bgcolor = "transparent", paper_bgcolor = "transparent",
           height = height, width = width)
}

#' Schema einer Distributionsstruktur
#' @param typ "direkt", "zentral", "zweistufig" oder "crossdock".
netz_panel <- function(typ) {
  kunden <- data.frame(x = rep(c(0.12, 0.31, 0.5, 0.69, 0.88), 2)[1:8],
                       y = 0.08)
  kunden$x <- seq(0.06, 0.94, length.out = 8)
  werk <- c(0.5, 0.92)
  p <- plot_ly(type = "scatter", mode = "markers")
  seg <- function(p, a, b, col = "#2F3E46", w = 2, dash = "solid")
    add_segments(p, x = a[1], xend = b[1], y = a[2], yend = b[2],
                 line = list(color = col, width = w, dash = dash),
                 hoverinfo = "skip", showlegend = FALSE)
  lager <- NULL
  if (typ == "direkt") {
    for (k in seq_len(nrow(kunden))) p <- seg(p, werk, unlist(kunden[k, ]), "#9AA5B1", 1.5)
  } else if (typ == "zentral") {
    lager <- data.frame(x = 0.5, y = 0.5)
    p <- seg(p, werk, c(0.5, 0.5), "#2F3E46", 5)
    for (k in seq_len(nrow(kunden))) p <- seg(p, c(0.5, 0.5), unlist(kunden[k, ]), "#9AA5B1", 1.5)
  } else if (typ == "zweistufig") {
    lager <- data.frame(x = c(0.5, 0.25, 0.75), y = c(0.62, 0.36, 0.36))
    p <- seg(p, werk, c(0.5, 0.62), "#2F3E46", 5)
    p <- seg(p, c(0.5, 0.62), c(0.25, 0.36), "#2F3E46", 3.5)
    p <- seg(p, c(0.5, 0.62), c(0.75, 0.36), "#2F3E46", 3.5)
    for (k in seq_len(nrow(kunden))) {
      l <- if (kunden$x[k] < 0.5) c(0.25, 0.36) else c(0.75, 0.36)
      p <- seg(p, l, unlist(kunden[k, ]), "#9AA5B1", 1.5)
    }
  } else if (typ == "crossdock") {
    werke <- data.frame(x = c(0.2, 0.5, 0.8), y = 0.92)
    lager <- data.frame(x = 0.5, y = 0.5)
    for (k in 1:3) p <- seg(p, unlist(werke[k, ]), c(0.5, 0.5), "#2F3E46", 4)
    for (k in seq_len(nrow(kunden))) p <- seg(p, c(0.5, 0.5), unlist(kunden[k, ]), "#D97B29", 1.5)
    p <- p |> add_trace(x = werke$x, y = werke$y, mode = "markers",
                        marker = list(symbol = "hexagon", size = 18, color = "#BFDBF7",
                                      line = list(color = "#2F3E46", width = 1.5)),
                        hoverinfo = "skip", showlegend = FALSE)
  }
  if (typ != "crossdock") p <- p |> add_trace(
    x = werk[1], y = werk[2], mode = "markers",
    marker = list(symbol = "hexagon", size = 20, color = "#BFDBF7",
                  line = list(color = "#2F3E46", width = 1.5)),
    hoverinfo = "skip", showlegend = FALSE)
  if (!is.null(lager)) p <- p |> add_trace(
    x = lager$x, y = lager$y, mode = "markers",
    marker = list(symbol = if (typ == "crossdock") "diamond" else "square",
                  size = 18, color = "#F4A259",
                  line = list(color = "#2F3E46", width = 1.5)),
    hoverinfo = "skip", showlegend = FALSE)
  p |> add_trace(x = kunden$x, y = kunden$y, mode = "markers",
                 marker = list(size = 11, color = "#2D9E8E"),
                 hoverinfo = "skip", showlegend = FALSE) |>
    layout(xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
           yaxis = list(visible = FALSE, range = c(0, 1.02), fixedrange = TRUE))
}
