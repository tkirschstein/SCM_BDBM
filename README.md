# Supply Chain Management & Distribution – Kursmaterialien

**Hochschule RheinMain | Bachelor Digital Business Management | 3. Fachsemester | Wintersemester 2026/27**

Prof. Dr. Thomas Kirschstein

---

## Kursüberblick

Dieses Repository enthält alle Materialien zu den Lehrveranstaltungen *Supply Chain Management* und *Distribution* im Bachelorstudiengang Digital Business Management: Vorlesungsfolien, den begleitenden Reader, Fallstudien mit Übungsaufgaben und die zugehörige R-Funktionsbibliothek.

Die Veranstaltung vermittelt die strategischen und quantitativen Grundlagen des Supply Chain Managements und der Distributionsplanung. Im Mittelpunkt steht die Übertragung von Methoden aus der Vorlesung auf möglichst realistische Problemstellungen. Ein durchgängiges Fallbeispiel, die fiktive **SushiFresh GmbH**, verbindet alle Themen.

| | |
|---|---|
| **Studiengang** | Bachelor Digital Business Management, 3. Fachsemester (Pflichtveranstaltung) |
| **Umfang** | 2+2 SWS (seminaristische Vorlesung) |
| **Termine** | montags 18:00–21:00 Uhr, 19.10.2026 – 25.01.2027 (14 Termine, keine Veranstaltung am 28.12.) |
| **Sprache** | Deutsch (Fachliteratur überwiegend Englisch) |
| **Lehrformat** | Inverted Classroom: Selbststudium mit Folien und Reader vor dem Termin, in der Veranstaltung Rückfragen, Klausuraufgaben und Besprechung |
| **Prüfungsleistung** | Klausur |
| **Voraussetzungen** | Grundkenntnisse in Statistik; Programmierkenntnisse sind hilfreich, aber nicht erforderlich (R-Startercode wird bereitgestellt) |

**Lernziele:** Nach der Veranstaltung können Studierende

- Supply Chains als Netzwerke beschreiben und ihre strategische Ausrichtung (Strategic Fit) beurteilen,
- Zielkonflikte zwischen Kosten, Service und Nachhaltigkeit erkennen und bewerten,
- Distributionsstrukturen und -kanäle unterscheiden und bewerten,
- Standortentscheidungen mit qualitativen (Nutzwertanalyse, AHP), kontinuierlichen (Steiner-Weber) und diskreten Modellen (WLP) vorbereiten sowie Transportprobleme lösen,
- Bestandsentscheidungen unter Unsicherheit (Pooling, Newsvendor) treffen,
- die Ursachen des Bullwhip-Effekts erklären und Gegenmaßnahmen bewerten,
- Lager- und Kommissioniersysteme gestalten, Transportmittel und Antriebe über Kosten und Emissionen vergleichen,
- kürzeste Wege und Touren mit exakten und heuristischen Verfahren planen sowie
- die Aussagekraft und Grenzen quantitativer Modelle für reale Managemententscheidungen kritisch reflektieren.

---

## Ablauf der Veranstaltung

### Inverted Classroom

Die Veranstaltung arbeitet nach dem Prinzip *Inverted Classroom*. Einen festen Vortrag gibt es nicht. Die Inhalte eines Termins erarbeiten Sie **vorab im Selbststudium** mit den Folien und dem zugehörigen Reader-Kapitel (siehe Terminübersicht). Die R-Exkurse im Reader sind optional und nicht klausurrelevant.

In der Präsenzveranstaltung steht die Anwendung im Vordergrund. Ein Termin läuft in der Regel so ab:

| Zeit | Block | Inhalt |
|---|---|---|
| 18:00–18:45 | **Rückfragen zum Thema** | Offene Fragen aus dem Selbststudium werden gemeinsam geklärt. Bringen Sie Ihre Fragen mit – je konkreter, desto besser. |
| 18:45–19:15 | **Übungsaufgaben** | Bearbeitung von Teil 1 der zugehörigen Fallstudie sowie der Verstädnisfragen im Reader in Kleingruppen, mit Stift und Papier |
| 19:15–19:30 | Pause | |
| 19:30–20:15 | **Besprechung** | Gemeinsame Besprechung der Lösungen, Diskussion von Lösungswegen und typischen Fehlern |
| 20:15–21:00 | **Fallstudien und Vertiefung** | betreute Bearbeitung oder Präsentation / Diskussion  Gruppenfallstudien (Teil 2) |

### Fallstudien

Zu jedem Thema gibt es eine Fallstudie im Ordner `case_studies` (Links siehe unten). Jede Fallstudie besteht aus zwei Teilen:

- **Teil 1 – Klausurvorbereitung:** Übungsaufgaben im Klausurstil zu einer neuen SushiFresh-Entscheidungssituation, lösbar mit Stift und Papier in 25–30 Minuten. Die Musterlösungen sind in der Fallstudie ausklappbar hinterlegt. Diese Aufgaben werden in der Veranstaltung bearbeitet.
- **Teil 2 – Gruppenfallstudie (fakultativ):** eine vertiefende, meist datenbasierte Fallstudie für Gruppen von 3–6 Personen. Die Bearbeitung ist freiwillig und geht nicht in die Note ein. Gruppen können ihre Ergebnisse in einem der Termine nach dem jeweiligen Thema präsentieren (ca. 20 Minuten) und erhalten dazu Feedback. Bitte melden Sie Präsentationen spätestens eine Woche vorher an. Die Bearbeitung kann auch in den Präsenzterminen geschehen. 

### Terminübersicht

| Nr. | Datum | Thema | Folien / Reader-Kapitel | Übungsaufgaben (Teil 1) |
|---|---|---|---|---|
| 1 | 19.10.2026 | Kick-off und Organisation; Einführung und Grundbegriffe des SCM; Wertschöpfung, strategische Fertigung und Strategic Fit | Kapitel 1 und 2 | --- |
| 2 | 26.10.2026 | Ziele, Zielkonflikte und Logistikkosten; Nachhaltigkeit; Fallbeispiel SushiFresh |  Kapitel 3 | Fallstudie 1, Aufgaben 1-3 |
| 3 | 02.11.2026 | Distributionsstrukturen und -kanäle | Kapitel 4 | Fallstudie 1, Aufgabe 4 |
| 4 | 09.11.2026 | Standortplanung I: Nutzwertanalyse, AHP, Steiner-Weber-Modell | Kapitel 5 | Fallstudie 2 |
| 5 | 16.11.2026 | Standortplanung II: Warehouse Location Problem | Kapitel 6 | Fallstudie 3, Aufgaben 1–3 |
| 6 | 23.11.2026 | Transportplanung | Kapitel 7 | Fallstudie 3, Aufgaben 4–6 |
| 7 | 30.11.2026 | Unsicherheit in Supply Chains: Kapazitäts- und Lagerbestandspooling | Kapitel 8 | Fallstudie 4 |
| 8 | 07.12.2026 | Das Newsvendor-Modell | Kapitel 9 | Fallstudie 5 |
| 9 | 14.12.2026 | Der Bullwhip-Effekt und das Beer Game | Kapitel 10 | Fallstudie 6 |
| 10 | 21.12.2026 | Lagerplanung und Kommissionierung | Kapitel 11 | Fallstudie 7 |
| – | 28.12.2026 | *keine Veranstaltung* | --- | --- |
| 11 | 04.01.2027 | Transportmittel, Antriebe und Transportkosten | Kapitel 12 | Fallstudie 8 |
| 12 | 11.01.2027 | Kürzeste Wege | Kapitel 13 | Fallstudie 9 |
| 13 | 18.01.2027 | Tourenplanung | Kapitel 14 | Fallstudie 10 |
| 14 | 25.01.2027 | Klausurvorbereitung: Besprechung und Fragestunde | alle | --- |

**Hinweise zu einzelnen Terminen:**

- **Termin 1:** Da vor dem ersten Termin keine Vorbereitung möglich ist, beginnt die Veranstaltung mit Organisation und einem kurzen Überblick. Für die übrigen Termine gilt der oben beschriebene Ablauf.
- **Termin 9:** Ein großer Teil des Abends ist für das Beer Game reserviert (siehe unten).
- **Termin 14:** Wird zur Klausurvorbereitung genutzt.

---

## Prüfungsleistung: Klausuren

- Es wird je eine Klausur für SCM und Distribution á 45 Minuten an einem gemeinsamen Termin geshrieben.
- Beide Klausuren werden gemeinsam mit dem BBA-Kurs geschrieben und gestellt.
- **Prüfungsrelevant** sind die Inhalte der Folien. R-Code und die R-Exkurse des Readers sind nicht klausurrelevant.
- Beide Klausuren bestehen aus **Pflicht- und Wahlaufgaben**. Pflichtaufgaben behandeln Themen, die im BBA- und DBM-Kurs gleichermaßen behandelt werden. Wahlaufgaben gehen auf die spezifischen Schwerpunkte des BBA- und DBM-Kurses ein. Sie müssen **entweder** die Wahlaufgaben des BBA- **oder** des DBM-Teils wählen. Ihre Wahl ist unabhängig von ihrem Studiengang.
- Die Wahlaufgaben entsprechen in Stil und Umfang den Aufgaben in **Teil 1 der Fallstudien**: Rechenaufgaben, die mit Stift und Papier lösbar sind. Die Pflichtaufgabem sind eher Verständnis- und Transferfragen.
- Die Gruppenfallstudien (Teil 2) sind fakultativ und gehen nicht in die Note ein.

---

## Direkter Zugriff auf die Ressourcen

- Navigation: https://tkirschstein.github.io/SCM_BDBM/
- Slides: https://tkirschstein.github.io/SCM_BDBM/slides/scm-komplett.html
- Reader: https://tkirschstein.github.io/SCM_BDBM/book/index
- Fallstudien:
  - Fallstudie 1 – Grundlagen des SCM: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_01_strategie_frosch
  - Fallstudie 2 – Qualitative und kontinuierliche Standortplanung: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_02_standort_ahp_steiner_weber
  - Fallstudie 3 – Diskrete Standortplanung und Transportplanung: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_03_wlp_batterielogistik
  - Fallstudie 4 – Kapazitäts- und Lagerbestandspooling: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_04_pooling_einzelhandel
  - Fallstudie 5 – Newsvendor-Modell: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_05_newsvendor_baeckerei
  - Fallstudie 6 – Bullwhip-Effekt: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_06_bullwhip
  - Fallstudie 7 – Lagerplanung und Kommissionierung: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_07_lager_kommissionierung
  - Fallstudie 8 – Transportmittel, Antriebe und Transportkosten: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_08_transportmittel_antriebe
  - Fallstudie 9 – Kürzeste Wege: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_09_kuerzeste_wege
  - Fallstudie 10 – Tourenplanung: https://tkirschstein.github.io/SCM_BDBM/case_studies/case_study_10_tourenplanung

---

## Beer Game

Das **Beer Distribution Game** (Sterman 1989) simuliert den Bullwhip-Effekt in einer vierstufigen Supply Chain (Einzelhandel → Großhandel → Distributor → Hersteller). Wir spielen das Spiel am 14.12.2026 in der Veranstaltung, in der Regel mit folgendem Ablauf:

1. Einführung in die Regeln/Annahmen (variieren während des Semesters),
2. Spielrunden/Simulation,
3. Auswertung der Bestell- und Bestandsverläufe und Diskussion.

Online-Plattform: [Transentis](https://beergame.transentis.com/de).

Machen Sie sich gern vorab mit den Regeln und Ablauf vertraut.

---

## Aufbau des Repositories

```
SCM_BDBM/
├── slides/                  # Vorlesungsfolien (Quarto revealjs) → docs/slides
│   ├── lectures/            # 14 Vorlesungseinheiten (Dateinummern 01–14a, Reihenfolge siehe Terminübersicht)
│   ├── scm-komplett.qmd     # Gesamtausgabe aller Einheiten in Vorlesungsreihenfolge
│   └── _quarto.yml
├── book/                    # Reader (Quarto Book) → docs/book
│   ├── chapters/            # 14 Kapitel in fünf Teilen
│   ├── index.qmd
│   └── _quarto.yml
├── case_studies/            # Fallstudien 1–10 (Klausuraufgaben + Gruppenfallstudie) → docs/case_studies
├── data/                    # Datensätze für Übungen und Fallstudien
├── R/
│   ├── scm_functions.R      # R-Funktionsbibliothek des Kurses (roxygen2-dokumentiert)
│   └── scm_helpers.R        # Darstellungshilfen (Farben, Tabellen, Formatierung)
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
  "ompr", "ompr.roi", "ROI", "ROI.plugin.glpk",  # MILP (Fallstudien 3 und 10)
  "readxl",           # Excel-Import (Fallstudie 7)
  "leaflet", "osrm",  # Karten und Routing (Fallstudie 9)
  "lpSolve",          # Transportproblem
  "MASS"
))
```

R wird nur für die fakultativen Gruppenfallstudien und die Vertiefungen im Reader benötigt. Für die Fallstudien 4 und 5 ist ein (kostenloses) **Kaggle-Konto** erforderlich, um die Datensätze herunterzuladen.

Für diejenigen, die sich mit R vertraut machen wollen, ist unter [Data Science](https://ds-pl-r-book.netlify.app/) ein Einführungs-Kurs verfügbar.

---

## Literatur

| Titel | Autoren | Relevanz |
|---|---|---|
| *Supply Chain Management: Strategy, Planning, and Operation* | Chopra & Meindl | alle Themen |
| *Matching Supply with Demand* | Cachon & Terwiesch | Pooling, Newsvendor, Bullwhip |
| *Operations Management* | Thonemann | alle Themen |

Weitere Quellen sind im Reader und in den Fallstudien angegeben (`literature/!references.bib`).

---

## Kontakt

Prof. Dr. Thomas Kirschstein – thomas.kirschstein@hs-rm.de

Fehler und Verbesserungsvorschläge bitte als GitHub-Issue melden oder an thomas.kirschstein@hs-rm.de.
