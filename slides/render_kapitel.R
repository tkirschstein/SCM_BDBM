# ─────────────────────────────────────────────────────────────────────────────
# Kapitelweises Rendern der Vorlesungsfolien
#
# Die Gesamtausgabe scm-komplett.html ist für viele Mobilgeräte zu groß.
# Dieses Skript rendert jede Vorlesungseinheit als eigene reveal.js-
# Präsentation nach docs/slides/kapitel/ und erzeugt dort eine index.html
# mit der Kapitelübersicht.
#
# Vorgehen je Kapitel:
#   1. Im Ordner slides/ wird eine temporäre Wrapper-Datei <slug>.qmd angelegt,
#      die – wie scm-komplett.qmd – den Setup-Chunk enthält und die Datei aus
#      lectures/ per include einbindet. Weil der Wrapper in slides/ liegt,
#      funktionieren alle relativen Bildpfade (images/...) unverändert.
#   2. Der Wrapper wird mit dem Quarto-Profil "kapitel" gerendert
#      (output-dir: ../docs/slides/kapitel, siehe _quarto-kapitel.yml).
#   3. Der Wrapper wird wieder gelöscht.
#
# Aufruf (aus slides/ oder dem Projektverzeichnis):
#   Rscript render_kapitel.R            # alle Kapitel
#   Rscript render_kapitel.R 09 10      # nur Kapitel 9 und 10
# oder in RStudio: source("render_kapitel.R")
# ─────────────────────────────────────────────────────────────────────────────

# In das Verzeichnis slides/ wechseln --------------------------------------------
if (dir.exists("slides/lectures")) setwd("slides")
stopifnot("Bitte aus slides/ oder dem Projektverzeichnis starten" =
            dir.exists("lectures") && file.exists("_quarto-kapitel.yml"))

# Kapitel in Vorlesungsreihenfolge (wie scm-komplett.qmd) ------------------------
kapitel <- data.frame(
  nr    = sprintf("%02d", 1:14),
  slug  = c("01-einfuehrung", "02-strategic-fit", "03-ziele-und-kosten",
            "04-distributionsstrukturen", "05-standortplanung-1",
            "06-standortplanung-2", "07-transportplanung", "08-pooling",
            "09-newsvendor", "10-bullwhip", "11-lager-und-kommissionierung",
            "12-transportmittel", "13-kuerzeste-wege", "14-tourenplanung"),
  datei = c("01_einfuehrung-und-grundbegriffe-des-supply-chain-managements.qmd",
            "02_wertschoepfung-strategische-fertigung-und-strategic-fit.qmd",
            "03_ziele-zielkonflikte-und-logistikkosten.qmd",
            "14a_distributionsstrukturen.qmd",
            "04_standortplanung-i-qualitative-und-kontinuierliche-verfahren.qmd",
            "05_standortplanung-ii-diskrete-netzwerkplanungsmodelle.qmd",
            "09_transportplanung.qmd",
            "06_unsicherheit-in-supply-chains-pooling.qmd",
            "07_das-newsvendor-modell.qmd",
            "08_der-bullwhip-effekt-und-das-beergame.qmd",
            "13_lagerplanung-und-kommissionierung.qmd",
            "12_transportmittel-und-antriebe.qmd",
            "10_kuerzeste-wege.qmd",
            "11_tourenplanung-tsp.qmd"),
  titel = c("Einführung und Grundbegriffe des Supply Chain Managements",
            "Wertschöpfung, strategische Fertigung und Strategic Fit",
            "Ziele, Zielkonflikte und Logistikkosten",
            "Distributionsstrukturen und -kanäle",
            "Standortplanung I: Qualitative und kontinuierliche Verfahren",
            "Standortplanung II: Diskrete Netzwerkplanungsmodelle",
            "Transportplanung",
            "Unsicherheit in Supply Chains: Pooling",
            "Das Newsvendor-Modell",
            "Der Bullwhip-Effekt und das Beer Game",
            "Lagerplanung und Kommissionierung",
            "Transportmittel und Antriebe",
            "Kürzeste Wege",
            "Tourenplanung"),
  stringsAsFactors = FALSE
)

ausgabe_dir <- "../docs/slides/kapitel"

# Auswahl über Kommandozeilenargumente (z. B. "09" oder "9") ---------------------
args <- commandArgs(trailingOnly = TRUE)
auswahl <- if (length(args)) sprintf("%02d", as.integer(args)) else kapitel$nr
auswahl <- intersect(auswahl, kapitel$nr)

# Wrapper-Datei für ein Kapitel ------------------------------------------------
wrapper_text <- function(k) {
  c("---",
    sprintf('title: "%s"', k$titel),
    sprintf('subtitle: "Kapitel %d · Supply Chain Management & Distribution"',
            as.integer(k$nr)),
    'author: "Prof. Dr. Thomas Kirschstein"',
    "",
    "execute:",
    "  cache: false",
    "",
    "format:",
    "  revealjs:",
    "    slide-number: c/t",
    "    chalkboard: true",
    "    css: styles.css",
    "---",
    "",
    "```{r}",
    "#| echo: false",
    "#| warning: false",
    "#| message: false",
    "",
    "library(quarto)",
    "library(readr)",
    "library(tidyverse)",
    "library(plotly)",
    "library(knitr)",
    "library(kableExtra)",
    "library(dplyr)",
    "",
    'source("../R/scm_functions.R")',
    'source("../R/scm_helpers.R")',
    "```",
    "",
    sprintf("{{< include lectures/%s >}}", k$datei),
    "")
}

# Quarto rendern (Profil über Umgebungsvariable, funktioniert mit allen
# Versionen des R-Pakets quarto) -------------------------------------------------
render_kapitel <- function(k) {
  wrapper <- paste0(k$slug, ".qmd")
  writeLines(enc2utf8(wrapper_text(k)), wrapper, useBytes = TRUE)
  on.exit(unlink(wrapper), add = TRUE)

  alt <- Sys.getenv("QUARTO_PROFILE", unset = NA)
  Sys.setenv(QUARTO_PROFILE = "kapitel")
  on.exit(if (is.na(alt)) Sys.unsetenv("QUARTO_PROFILE")
          else Sys.setenv(QUARTO_PROFILE = alt), add = TRUE)

  message("\n── Kapitel ", k$nr, ": ", k$titel, " ──")
  quarto::quarto_render(wrapper, as_job = FALSE)
}

fehler <- character(0)
for (nr in auswahl) {
  k <- kapitel[kapitel$nr == nr, ]
  ok <- tryCatch({ render_kapitel(k); TRUE },
                 error = function(e) { message("FEHLER: ", conditionMessage(e)); FALSE })
  if (!ok) fehler <- c(fehler, nr)
}

# Übersichtsseite docs/slides/kapitel/index.html --------------------------------
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

eintraege <- vapply(seq_len(nrow(kapitel)), function(i) {
  k <- kapitel[i, ]
  datei <- paste0(k$slug, ".html")
  if (file.exists(file.path(ausgabe_dir, datei))) {
    sprintf('      <li><a href="%s">%s</a></li>', datei, html_escape(k$titel))
  } else {
    sprintf('      <li class="offen">%s <span>(noch nicht verfügbar)</span></li>',
            html_escape(k$titel))
  }
}, character(1))

index_html <- c(
'<!doctype html>',
'<html lang="de">',
'<head>',
'<meta charset="utf-8">',
'<meta name="viewport" content="width=device-width, initial-scale=1">',
'<title>Vorlesungsfolien – Kapitelübersicht</title>',
'<style>',
'  :root { --bg:#f7f8fa; --surface:#fff; --text:#1f2933; --muted:#5b6875; --border:#dde2e8; --accent:#1f5fa8; }',
'  @media (prefers-color-scheme: dark) {',
'    :root { --bg:#14181d; --surface:#1c2229; --text:#e6e9ed; --muted:#a0abb7; --border:#2f3842; --accent:#7fb0ea; }',
'  }',
'  * { box-sizing: border-box; }',
'  body { margin:0; background:var(--bg); color:var(--text);',
'         font:16px/1.55 system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif; }',
'  main { max-width:760px; margin:0 auto; padding:40px 16px 56px; }',
'  h1 { font-size:1.75rem; line-height:1.2; margin:0 0 8px; }',
'  header p { margin:4px 0; color:var(--muted); }',
'  a { color:var(--accent); }',
'  ol { background:var(--surface); border:1px solid var(--border); border-radius:10px;',
'       margin:24px 0; padding:16px 16px 16px 2.6em; }',
'  li { margin:10px 0; }',
'  li.offen { color:var(--muted); }',
'  li span { font-size:0.9rem; }',
'  footer { color:var(--muted); font-size:0.9rem; }',
'</style>',
'</head>',
'<body>',
'<main>',
'  <header>',
'    <h1>Vorlesungsfolien nach Kapiteln</h1>',
'    <p>Supply Chain Management &amp; Distribution · Wintersemester 2026/27</p>',
'    <p>Jedes Kapitel ist eine eigene Präsentation und lädt deutlich schneller als die Gesamtausgabe – besonders auf Smartphones und Tablets.</p>',
'  </header>',
'  <ol>',
eintraege,
'  </ol>',
'  <footer>',
'    <a href="../scm-komplett.html">Gesamtausgabe aller Folien</a> · <a href="../../index.html">Zur Kursseite</a>',
'  </footer>',
'</main>',
'</body>',
'</html>')

dir.create(ausgabe_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(enc2utf8(index_html), file.path(ausgabe_dir, "index.html"), useBytes = TRUE)

message("\nÜbersicht geschrieben: ", normalizePath(file.path(ausgabe_dir, "index.html")))
if (length(fehler)) {
  message("Nicht gerendert (Fehler): Kapitel ", paste(fehler, collapse = ", "))
} else {
  message("Alle ausgewählten Kapitel erfolgreich gerendert.")
}
