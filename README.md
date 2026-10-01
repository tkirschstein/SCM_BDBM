# Supply Chain Management & Distribution – Kursmaterialien

**Hochschule RheinMain | Bachelor Digital Business Management | 3. Fachsemester | Wintersemester 2026/27**

Prof. Dr. Thomas Kirschstein

---

## Kursüberblick

Dieses Repository enthält alle Materialien zu den Lehrveranstaltungen *Supply Chain Management* und *Distribution* im Bachelorstudiengang Digital Business Management: Vorlesungsfolien, den begleitenden Reader, Übungsaufgaben und die zugehörige R-Funktionsbibliothek.

Die Veranstaltung vermittelt die strategischen und quantitativen Grundlagen des Supply Chain Managements. Im Mittelpunkt steht die Übertragung von Methoden aus der Vorlesung auf möglichst realistische Problemstellungen. 

| | |
|---|---|
| **Studiengang** | Bachelor Digital Business Management, 3. Fachsemester |
| **Umfang** | 2+2 SWS (seminaristische Vorlesung) |
| **Termine** | montags, 19.10.2026 – 25.01.2027 (14 Termine, keine Veranstaltung am 28.12.) |
| **Sprache** | Deutsch (Fachliteratur überwiegend Englisch) |
| **Lehrformat** | Inverted Classroom: Vorbereitung mit dem Reader, in der Veranstaltung kurzer Input, Fallstudien-/Übungsbesprechung |
| **Prüfungsleistung** | Klausur |
| **Voraussetzungen** | Grundkenntnisse in Statistik; Programmierkenntnisse sind hilfreich, aber nicht erforderlich (R-Startercode wird bereitgestellt) |

**Lernziele:** Nach der Veranstaltung können die Studierenden

- Supply Chains als Netzwerke beschreiben und ihre strategische Ausrichtung (Strategic Fit) beurteilen,
- Zielkonflikte zwischen Kosten, Service und Nachhaltigkeit erkennen und bewerten,
- Standortentscheidungen mit qualitativen (Nutzwertanalyse, AHP), kontinuierlichen (Steiner-Weber) und diskreten Modellen (WLP) vorbereiten,
- Bestandsentscheidungen unter Unsicherheit (Pooling, Newsvendor) treffen,
- die Ursachen des Bullwhip-Effekts erklären und Gegenmaßnahmen bewerten sowie
- die Aussagekraft und Grenzen quantitativer Modelle für reale Managemententscheidungen kritisch reflektieren.

---

## Prüfungsleistung: Klausur


---

## Direkter Zugriff auf die Ressourcen

- Navigation: https://tkirschstein.github.io/SCM_BDBM/
- Slides: https://tkirschstein.github.io/SCM_BDBM/slides/scm-komplett.html
- Reader: https://tkirschstein.github.io/SCM_BDBM/book/index
- Case-Studies: 
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_01_strategie_frosch
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_02_standort_ahp_steiner_weber
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_03_wlp_batterielogistik
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_04_pooling_einzelhandel
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_05_newsvendor_baeckerei
  - https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_06_bullwhip

---

## Beer Game

Das **Beer Distribution Game** (Sterman 1989) simuliert den Bullwhip-Effekt in einer vierstufigen Supply Chain (Einzelhandel → Großhandel → Distributor → Hersteller). Wir spielen das Spiel mehrfach in der Veranstaltung in der Regel mit folgendem Ablauf:

1. Einführung in die Regeln/Annahmen (variieren während des Semesters),
2. Spielrunden/Simulation,
3. Auswertung der Bestell- und Bestandsverläufe und Diskussion.

Online-Plattform: [Transentis](https://beergame.transentis.com/de). 

Machen Sie sich gern vorab mit den Regeln und Ablauf vertraut.

---


## Aufbau des Repositories

```
SCM/
├── slides/                  # Vorlesungsfolien (Quarto revealjs) → docs/slides
│   ├── lectures/            # 8 Vorlesungseinheiten (01–08)
│   ├── scm-komplett.qmd     # Gesamtausgabe aller Einheiten
│   └── _quarto.yml
├── book/                    # Reader (Quarto Book) → docs/book
│   ├── chapters/            # Kapitel 1–9
│   ├── index.qmd
│   └── _quarto.yml
├── R/
│   └── scm_functions.R      # R-Funktionsbibliothek des Kurses (roxygen2-dokumentiert)
└── docs/                    # gerenderte Ausgabe (HTML)
```

### Rendern

```bash
# Reader
cd book && quarto render

# Vorlesungsfolien (einzeln oder als Gesamtausgabe, siehe slides/README.md)
cd slides && quarto render lectures/01_einfuehrung-und-grundbegriffe-des-supply-chain-managements.qmd
cd slides && quarto render --profile full

# Fallstudien
cd case_studies && quarto render
```

Die Ausgabe landet jeweils im Ordner `docs/`. Das Arbeitsverzeichnis für R-Code ist der jeweilige Projektordner, die Funktionsbibliothek wird daher mit `source("../R/scm_functions.R")` eingebunden.

---

## Software

- **R** ≥ 4.3 mit **RStudio** oder Positron
- **Quarto** ≥ 1.4 (<https://quarto.org>)

```r
install.packages(c(
  "tidyverse",        # Datenaufbereitung und Grafiken
  "lubridate",        # Datumsfunktionen (Fallstudie 5)
  "zoo",              # gleitende Durchschnitte (Fallstudie 4)
  "knitr", "kableExtra",
  "plotly",           # interaktive Grafiken im Reader
  "ompr", "ompr.roi", "ROI", "ROI.plugin.glpk",  # MILP (Fallstudie 3)
  "lpSolve",          # Transportproblem
  "MASS"
))
```

Für die Fallstudien 4 und 5 ist ein (kostenloses) **Kaggle-Konto** erforderlich, um die Datensätze herunterzuladen.

Für diejenigen, die sich mit R vertraut machen wollen, ist unter [Data Science](https://ds-pl-r-book.netlify.app/) ein Einführungs-Kurs verfügbar. 


---

## Literatur

| Titel | Autoren | Relevanz |
|---|---|---|
| *Supply Chain Management: Strategy, Planning, and Operation* | Chopra & Meindl | alle Themen |
| *Matching Supply with Demand* | Cachon & Terwiesch | Pooling, Newsvendor, Bullwhip |
| *Sustainable Logistics and Supply Chain Management* | Grant et al. | Nachhaltigkeit |

Weitere Quellen sind im Reader und in den Fallstudien angegeben (`literature/!references.bib`).

---

## Kontakt

Prof. Dr. Thomas Kirschstein – thomas.kirschstein@hs-rm.de

Fehler und Verbesserungsvorschläge bitte als GitHub-Issue melden oder an thomas.kirschstein@hs-rm.de.
