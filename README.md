# Arthur's Technologiewahlmodell

Diese App implementiert das Technologiewahlmodell von Brian Arthur (siehe z.B. [hier](http://www.economia.ufpr.br/Eventos/Downloads/Minicurso2b.pdf)).

Eine genaue Beschreibung des Modells und der Implementierung findet sich in
`beschreibung/Technologiewahl_Modellbeschreibung.pdf`.

Mögliche Leitfragen für die Experimente mit dem Modell:

1. Welchen Einfluss hat die Netzwerktopologie auf die Tendenz zu technologischen
Monopolisierung? Warum?

2. Was ist der Einfluss des speziellen Technologienutzens und der 
gruppenbezogenen Technologiepräferenz?

3. Was ist der Effekt der Anzahl der Technologien und Agenten auf die 
Modell-Dynamik? Ist dieser Effekt Ihrer Ansicht nach intuitiv?

4. Welchen Unterschied macht es ob für die Berechnung des Netzwerknutzens der 
Anteil der Technologien an den bisher gewählten Technologien, oder der Anteil 
der Agenten, die diese Technologie verwenden, herangezogen wird? 
Welche Annahme ist für welche Situation plausibler?

5. Unter welchen Umständen beobachten wir trotz unterschiedlicher Qualität der 
Technologien keine Monopolisierung? Warum?

## Technische Hinweise zur Verwendung der App

Um die App zu verwenden gehen Sie folgendermaßen vor:

1. Laden Sie sich die Repository herunter
2. Öffnen Sie das entsprechende R Projektfile `tech-choiceR.Rproj`
3. Öffnen Sie das file `app.R`
4. Klicken Sie oben rechts aus `Run App` (ich empfehle Ihnen dabei über das Drop-Down Menü `Run external` auszuwählen, damit die App im Browser geöffnet wird)

Alternativ können Sie auch den Link im Moodle verwenden, allerdings ist die 
Nutzungszeit hier pro Monat für den Kurs beschränkt und Sie können sich nicht 
den Code ansehen.

Eine dritte Möglichkeit besteht darin, die App lokal zu installieren und über
ihre lokale R-Version zu benutzen. 
Führen Sie dazu folgenden Code aus:

```
shiny::runGitHub(username = "graebnerc", repo = "tech-choiceR")
```

## Hinweise zu notwendigen Paketen und eventuellen Updates

Wenn Sie die App auf Ihrem Computer verwenden wollen müssen Sie bestimmte 
Pakete installiert haben. Zudem sollte Ihre R Version nicht zu alt sein.
Sie können das automatisiert mit dem Skritpt `versionstest_tech_choice.R` überprüfen. 
Hier werden eventuell fehlende Pakete automatisch installiert. 
Zudem bekommen Sie einen Hinweis wenn Sie R oder ein Paket updaten sollten.

Wenn Sie R auf Windows updaten müssen gehen Sie folgendermaßen vor:

1. Installieren Sie das Paket `installr`: `install.packages("installr")`
2. Führen Sie folgenden Code aus:

```
library(installr)
updateR()
```

3. Checken Sie ob R-Studio geupdated werden muss indem Sie unter `Hilfe` nach neuen Versionen suchen.

Wenn Sie R auf dem Mac updaten wollen, installieren Sie R einfach neu.

Für eventuelle Fragen melden Sie sich bitte bei Claudius Gräbner
