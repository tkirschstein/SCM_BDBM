# Vorlesungsfolien

## Einzelne Vorlesung rendern

Aus dem Verzeichnis `slides/` kann jede Datei in `lectures/` separat gerendert werden, zum Beispiel:

```bash
quarto render lectures/06_unsicherheit-in-supply-chains-pooling.qmd
```

## Kapitelweise rendern (für Mobilgeräte)

Die Gesamtausgabe ist für viele Smartphones und Tablets zu groß. Das Skript `render_kapitel.R` rendert jede Vorlesungseinheit als eigene Präsentation nach `docs/slides/kapitel/` (Profil `kapitel`, siehe `_quarto-kapitel.yml`) und erzeugt dort eine `index.html` mit der Kapitelübersicht:

```bash
cd slides
Rscript render_kapitel.R          # alle Kapitel
Rscript render_kapitel.R 09 10    # nur ausgewählte Kapitel (Nummer = Position in der Vorlesung)
```

Je Kapitel legt das Skript in `slides/` kurzzeitig eine Wrapper-Datei an (Setup-Chunk + `include` der Datei aus `lectures/`), damit alle Bildpfade wie in der Gesamtausgabe funktionieren, und löscht sie nach dem Rendern wieder. Noch nicht gerenderte Kapitel erscheinen in der Übersicht als „noch nicht verfügbar“ – so lassen sich Kapitel auch schrittweise freischalten.

## Gesamtausgabe rendern

Die Datei `scm-komplett.qmd` bindet die acht Vorlesungseinheiten in ihrer numerischen Reihenfolge ein. Das Profil `full` beschränkt den Renderlauf auf diese Gesamtausgabe:

```bash
cd slides
quarto render --profile full
```

Alternativ kann die Gesamtausgabe direkt erzeugt werden:

```bash
quarto render scm-komplett.qmd
```

Die Bibliografie wird projektweit über `_quarto.yml` aus `../literature/references.bib` bereitgestellt.
