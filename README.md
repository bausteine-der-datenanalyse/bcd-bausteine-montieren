# bcd-bausteine-montieren

`collect-content.R` setzt Unterlagen aus den Bausteine-Repos zusammen. Das Skript liest die Datei `content.yml`
im aktuellen Verzeichnis und arbeitet die darin beschriebenen Jobs ab: Dateien kopieren und umschreiben,
Aufgabenblätter erzeugen, Zips packen. Das Ergebnis rendert anschließend Quarto.

Aufruf im Ordner des Quarto-Projekts, z. B.:

```bash
cd lernpfad/skript
../../bausteine/bcd-bausteine-montieren/collect-content.R
```

Benötigte R-Pakete: `yaml`, `purrr`, `readr`, `stringr`, `rprojroot`, `zip`.

## Ablauf

1. `content.yml` im aktuellen Verzeichnis (Projektordner) einlesen.
2. Den Projekt-Root bestimmen: Das ist der nächste übergeordnete Ordner mit einer `DESCRIPTION`-Datei.
3. `deploy-folder` und `target-folder` **löschen**, dann `target-folder` neu anlegen.
4. Die Jobs unter `jobs` einmal ausführen.
5. Die Jobs unter `jobs-on-parts` für jeden Eintrag in `parts` ausführen.

## Aufbau der `content.yml`

```yaml
# Zielordner, relativ zum Projektordner. Wird bei jedem Lauf geleert.
target-folder: c

# Ordner für Dateien, die direkt veröffentlicht werden (z. B. Zips), relativ zum Projekt-Root.
# Wird ebenfalls bei jedem Lauf geleert.
deploy-folder: _output/folien-r/c

# Eigene Variablen
definitions:
- bcd-bausteine-r: ${project-folder}/bausteine/bcd-bausteine-r

# Jobs, die einmal ausgeführt werden
jobs:
- copy:
  from: quarto-template.yml
  to: ${target-folder}/_metadata.yml

# Jobs, die für jeden Teil ausgeführt werden
jobs-on-parts:
- copy:
  from: ${part-folder}/folien.qmd
  to: ${target-folder}/${idx}-${name}.qmd

# Regulärer Ausdruck, dessen erste Gruppe den Namen ${name} aus dem Ordner eines Teils liefert
part-name-pattern: "w\\-(.*)/"

# Teile, Ordner relativ zum Projekt-Root
parts:
- folder: bausteine/bcd-bausteine-r/w-erste-schritte/folien
- folder: bausteine/bcd-bausteine-r/w-ggplot2/folien
```

Zu beachten:

- Die Parameter eines Jobs stehen auf derselben Einrückungsebene wie der Jobname (`- copy:` und darunter
  `from:`, `to:`).
- Relative Pfade in Jobs sind relativ zum Projektordner. Ausnahme ist `zipfile` beim Job `zip` (siehe unten).
- Kommentare (`# ...`) sind erlaubt und eignen sich, um die Jobs zu gliedern.

## Variablen

Variablen werden in allen Pfaden und Texten als `${name}` geschrieben. Werte dürfen selbst Variablen enthalten.

| Variable            | Bedeutung                                                                 |
|---------------------|---------------------------------------------------------------------------|
| `${project-folder}` | Projekt-Root (Ordner mit `DESCRIPTION`), absolut                          |
| `${target-folder}`  | Wert von `target-folder`                                                  |
| `${deploy-folder}`  | Wert von `deploy-folder`, absolut                                         |
| `${idx}`            | laufender Index, zweistellig (`01`, `02`, …)                              |
| `${sidx}`           | laufender Index ohne führende Null (`1`, `2`, …)                          |
| `${part-folder}`    | nur in `jobs-on-parts`: absoluter Pfad des aktuellen Teils                |
| `${name}`           | nur in `jobs-on-parts`: Name des Teils laut `part-name-pattern`           |
| eigene              | unter `definitions` festgelegt                                            |

Der Index beginnt vor `jobs` und vor `parts` jeweils bei 1. In `parts` wird er nach jedem Teil erhöht. Ein
Eintrag `- folder: skip` überspringt eine Nummer, ohne Jobs auszuführen. In `jobs` erhöht ihn nur der Job
`increment-index`.

## Jobs

### `copy`

Kopiert eine Datei oder den Inhalt eines Ordners.

```yaml
- copy:
  from: ${part-folder}/bilder
  to: ${target-folder}/bilder
  optional: yes                                          # kein Fehler, wenn from fehlt
  overwrite: yes                                         # vorhandene Dateien überschreiben
  exclude-patterns: [".afdesign$", ".af$", "~lock~$"]    # reguläre Ausdrücke für auszulassende Dateien
  transform:                                             # kopierte Dateien umschreiben
    type: replace
    pattern: "^#"
    replacement: "##"
```

- Ordner werden **nicht rekursiv** kopiert, nur die Dateien direkt darin. Unterordner brauchen einen eigenen
  `copy`-Job.
- Ohne `overwrite` bricht das Skript ab, wenn eine Zieldatei schon existiert. Das betrifft etwa Bilder, die
  mehrere Teile in denselben Ordner kopieren.
- `transform` mit `type: replace` ersetzt zeilenweise per regulärem Ausdruck, aber nicht innerhalb von
  ```` ```{r} ````-Chunks. Im Beispiel werden so alle Überschriften eine Ebene tiefer gesetzt.
- `transform` mit `type: insert-after-header` fügt `text` direkt nach dem YAML-Kopf ein (Variablen wie `${idx}`
  werden ersetzt), z. B. Links auf Zip und PDF auf der zweiten Folie.

### `zip`

Packt einen Ordner (rekursiv) in ein Zip-Archiv.

```yaml
- zip:
  zipfile: ${idx}-${name}.zip
  folder: ${idx}-${name}
  root: ${target-folder}
```

`folder` und ein relativer `zipfile` sind relativ zu `root`. Das Zip im Beispiel landet also in
`${target-folder}`. Soll es direkt in den Deploy-Ordner, `zipfile: ${deploy-folder}/name.zip` verwenden.

### `delete`

Löscht einen Ordner samt Inhalt, z. B. den Ordner, der eben gezippt wurde.

```yaml
- delete:
  folder: ${target-folder}/${idx}-${name}
```

### `assignment-paper`

Erzeugt aus einzelnen Aufgabendateien ein Aufgabenblatt und ein Lösungsblatt:

- `${target-folder}/aufgabenblatt-${idx}.qmd`
- `${target-folder}/aufgabenblatt-${idx}-loesung-${sol}.qmd`

```yaml
- assignment-paper:
  subtitle: "Erste Schritte"
  sol: q7m2xk9
  files:
  - ${bcd-bausteine-r}/w-erste-schritte/aufgaben/*.qmd
  folders:
  - ${bcd-bausteine-statistik}/m-statistische-daten/aufgaben/*
```

- `subtitle` und `sol` stehen als `${subtitle}` und `${sol}` zur Verfügung. `sol` ist ein zufälliges Kürzel,
  damit sich der Name der Lösungsdatei nicht erraten lässt.
- Die Kopfzeilen der Blätter kommen aus den Variablen `${assignment-header}` und
  `${assignment-solution-header}`, die unter `definitions` stehen müssen.
- `files`: Ordner mit Muster, die Treffer werden in alphabetischer Reihenfolge aneinandergehängt. Das Muster ist
  ein regulärer Ausdruck, kein Glob. `*.qmd` funktioniert trotzdem, `10.*.qmd` wählt die Dateien, die mit `10`
  beginnen.
- Zu `aufgabe.qmd` gehört die Lösung `aufgabe.sol.qmd`. Auf dem Lösungsblatt folgt sie unter "Lösung" auf die
  Aufgabe, fehlt sie, steht dort "Keine Lösung". Auf dem Lösungsblatt werden R-Chunks nur angezeigt, nicht
  ausgeführt (`{r}` wird zu `{.r}`).
- `folders`: Unterordner, die passend zum Muster in `${target-folder}` kopiert werden (z. B. Daten, Bilder),
  ebenfalls nicht rekursiv.
- Der Job erhöht den Index nicht. Danach folgt deshalb in der Regel `- increment-index:`.
- Platzhalter `${zipfile}` in den Aufgabendateien: Ist unter `definitions` eine Variable `zipfile` festgelegt (z. B.
  `zipfile: ${idx}-${name}-aufgaben.zip`, dieselbe Variable verwenden dann auch die `jobs-on-parts`), wird
  `${zipfile}` in Aufgaben und Lösungen durch deren Wert ersetzt. `${idx}` und `${name}` sind dabei die des Teils
  aus `parts`, zu dessen Ordner die Aufgabendatei gehört, nicht die des Aufgabenblatts. Ein Blatt mit Aufgaben aus
  zwei Teilen verweist so auf die richtigen Zips. Gehört der Ordner zu keinem Teil, bleibt der Platzhalter stehen.
  Andere `${…}` in den Aufgaben werden nicht ersetzt.

### `increment-index`

Erhöht `${idx}` und `${sidx}` um eins. Mehrfach hintereinander überspringt Nummern.

```yaml
- increment-index:
```
