# Vorlesungsfolien

## Einzelne Vorlesung rendern

Aus dem Verzeichnis `slides/` kann jede Datei in `lectures/` separat gerendert werden, zum Beispiel:

```bash
quarto render lectures/06_unsicherheit-in-supply-chains-pooling.qmd
```

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
