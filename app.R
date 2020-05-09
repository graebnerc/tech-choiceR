library(shiny)
library(tidyverse)
library(igraph)
library(scales)
library(Hmisc)
library(wesanderson)
library(latex2exp)
library(magrittr)
library(ggpubr)

source("helpers.R")
col_blue <- "#004c93"
col_light_blue <- "#dfe4f2"
decision_kind_dict <- list(
  "Anteile an bereits gewählten Technologien"="share",
  "Nutzungsanteile bei allen Agenten"="absolute"
)
topology_dict <- list(
  "Komplettes Netzwerk"="vollst_netw",
  "Ring"="ring_netw",
  "Barabasi-Albert"="BA_netw",
  "Small-World", "smw_netw"
)

ui <- fluidPage(
  withMathJax(),
  titlePanel("Ein Technologiewahlmodell"),
  fluidRow(
    column(3,
           h3("Grundeinstellungen"),
           sliderInput("simulation_iterations", 
                       label='Anzahl der Simulationsiterationen',
                       min = 2, max = 100, step=1, value = 10),
           sliderInput("n_agents", 
                       label='Anzahl der Agenten \\( N \\)',
                       min = 2, max = 100, step=1, value = 50),
           numericInput("n_techs", label = "Anzahl Technologien", 
                        value = 2, min = 2, max = 5, step = 1),
           h4("Fallspezifische Einstellungen"),
           selectInput("decision_kind", 
                       label = "Grundlage für Netzwerknutzen",
                       choices = c("Anteile an bereits gewählten Technologien", 
                                   "Nutzungsanteile bei allen Agenten"
                       ),
                       selected = "Anteile an bereits gewählten Technologien"),
           selectInput("network_topology", 
                       label = "Netzwerktopologie",
                       choices = c("Komplettes Netzwerk", 
                                   "Ring", 
                                   "Barabasi-Albert", 
                                   "Small-World"),
                       selected = "Komplettes Netzwerk"),
           h4("Technologieeigenschaften"),
           sliderInput("int_util", 
                       label='Gruppenbezogene Technologiepräferenz',
                       min = 0.0, max = 1.0, step=0.05, value = 0.5),
           sliderInput("int_util_1", 
                label='Spezieller intrinsischer Nutzen von \\( T_1 \\)',
                min = 0.0, max = 1.0, step=0.05, value = 0.0),
           sliderInput("int_util_2", 
                       label='Spezieller intrinsischer Nutzen von \\( T_2 \\)',
                       min = 0.0, max = 1.0, step=0.05, value = 0.0),
           conditionalPanel(
             condition = "input.n_techs>2",
             sliderInput("int_util_3", 
                         label='Spezieller intrinsischer Nutzen von \\( T_3 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.0)
             ),
           conditionalPanel(
             condition = "input.n_techs>3",
             sliderInput("int_util_4", 
                         label='Spezieller intrinsischer Nutzen von \\( T_4 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.0)
           ),
           conditionalPanel(
             condition = "input.n_techs>4",
             sliderInput("int_util_5", 
                         label='Spezieller intrinsischer Nutzen von \\( T_5 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.0)
           )
  ),
    column(3,
           h3("a) Netzwerktopologie"),
           plotOutput("network_1")
    ),
  column(3, 
         h3("b) Technologienutzung am Ende"),
         numericInput("single_adapt_case_netw", label = "Iteration", 
                      min = 1, max = 100, step=1, value = 1),
         plotOutput("network_2")
  ),
    column(3,
           h3("c) Einzelne Adaptionsdynamiken"),
           numericInput("single_adapt_case", label = "Iteration", 
                        min = 1, max = 100, step=1, value = 1),
           plotOutput("single_adaption")
    )
  ),
  fluidRow(
    column(3),
    column(3,
           h3("d) Dynamik der dominanten Technologien"),
           plotOutput("dyn_dom_tech")
           ),
    column(3,
           h3("e) Finale Anteile der einzelnen Technologien"),
           plotOutput("final_shares_normal")
    ),
    column(3,
           h3("f) Finale Anteile der nach Anteilen gerankten Technologien"),
           plotOutput("final_shares_ranked")
    )
    ),
  fluidRow(
    column(3),
    column(9,
           downloadButton("downloadPlot", 
                          "Download der Abbildungen c-f im PDF Format"),
           h3("Beschreibung des Modells"),
           p("Eine genaue Beschreibung des Modelles und der Implementierung in R finden Sie im Begleitdokument (Moodle oder auf Github im Ordner 'beschreibung')."),
           p("Agenten wählen nacheinander eine Technologie. Bei der Wahl berücksichtigen Sie ihre persönliche Präferenz und den Netzwerknutzen, also die aktuelle Verbreitung der Technologie in ihrer Nachbarschascht. Diese wird durch die Netzwerktopologie bestimmmt. Im vollständigen Netzwerk wird die Verbreitung der Technologie in der gesamten Population berücksichtigt. Dies stellt die implizite Annahme in vielen Modellen dar."),
           p("Es gibt dabei unterschiedliche aber gleich große Gruppen von Agenten, die alle eine andere Technologie persönlich bevorzugen. Die Stärke dieser Präferenz wird durch den Parameter 'Gruppenbezogene Technologiepräferenz' bestimmt."),
           p("Darüber hinaus hat jede Technologie noch einen objektiven Wert, der durch den speziellen intrinsischen Nutzen der Technolgie bestimmt wird. Während die Gruppenbezogene Präferenz also tendenziell eher subjektiv ist, also z.B. der Datenschutz eines Messengers, ist der spezielle intrinsische Nutzen objektiv und für jeden Agenten relevant. Ein Beispiel wäre die grundsätzliche Übertragungsverlässlichkeit eines Messangers"),
           p("Ein einzelner Modelldurchlauf ist abgeschlossen, wenn jede*r einzelne Agent*in ihre Technolgie gewählt hat."),
           h3("Benutzung der App"),
           p("Parameterwerte können im Panel 'Grundeinstellungen' geändert werden. Die einzelnen Parameter haben dabei folgenden Effekt:"),
           tags$ol(
             tags$li(paste(
               "Die Anzahl der Simulationsiterationen bestimmt wie viele",
               "Simulationsdurchläufe den Abbildungen zugrunde liegen sollen.",
               "Beachten Sie, dass größere Werte längere Rechenzeit implizieren", 
               "und dass sie eventuell überraschende Ergebnisse bekommen wenn",
               "bei wenigen Iterationen die Anzahl nicht ohne Rest durch die", 
               "Anzahl der Technologien teilbar ist (warum?)!")), 
             tags$li("Die Anzahl der Agenten und der Technologien ist selbsterklärend."), 
             tags$li("Bei der 'Grundlage des Netzwerknutzens' können Sie auswählen ob bei der Berechnung der relativen Nutzer*innenanteile, die dem Netzwerknutzen zugrunde liegen, nur die Agenten berücksichtigt werden sollen, die schon eine Technologie gewählt haben, oder alle Agenten."),
             tags$li("Über die Netzwerktopologie können Sie bestimmen an welchen anderen Agenten sich ein Agent orientiert wenn sie den Netzwerknutzen der Technologien berechnet. Das gewählte Netzwerk wird in Abbildungen a) und b) angezeigt."), 
             tags$li("Die gruppenbezogene Technologiepräferenz bestimmt wie stark die jeweiligen Gruppen die von ihnen präferierte Technologie den anderen Technologien vorziehen."), 
             tags$li("Der spezielle intrinsische Nutzen einer Technologie ist ein Bonus, der dieser Technologie von jedem Agenten (unabhängig der Gruppenzugehörigkeit) zugeschrieben wird.") 
           ),
           p("Auf Basis der so gewählten Parameterkonstellation werden dann die folgenden Abbildungen erstellt:"),
           tags$ol(
             tags$li("a) Hier wird das gewählte Netzwerk zu Beginn der Simulation dargestellt. Agenten beziehen bei der Berechnung des Netzwerknutzens einer Technologie nur die Entscheidungen der Agenten mit ein, mit denen sie direkt verbunden sind."),
             tags$li("b) Hier wird die Situation nach einem einzelnen Simulationsdurchlauf dargestellt. Agenten mit gleicher Farbe haben sich für die gleiche Technologie entschieden. Mit dem Widget 'Iteration' kann der darzustellende Simulationsdurchlauf ausgewählt werden."), 
             tags$li("c) Hier werden für einen einzelnen Simulationsdurchlauf die Nutzer*innenanteile der jeweiligen Technologien über die Zeit dargestellt. Der konkrete Simulationsdurchlauf kann wieder mit dem Widget 'Iteration' ausgewählt werden."),
             tags$li("d) Diesmal wird für alle Simulationsdurchläufe die Verbreitung der Technologie angezeigt, die am Ende den größten Nutzer*innenanteil erziehlt hat. Jede Linie ist also nun das Ergebnis eines einzelnen Simulationsdurchlaufs und kann sich auf unterschiedliche Technologien beziehen."), 
             tags$li("e) Die Grafik fasst die Ergebnisse aller Simulationsdurchläufe zusammen. Die Boxplots stellen dabei die Verbreitung der einzelnen Technologien zum Ende der Simulationen dar. Der große Punkt repräsentiert zusätzlich noch die durchschnittliche Verbreitung."), 
             tags$li("f) Ähnlich wie bei Grafik (e) werden die Ergebnisse aller Simulationsdurchläufe zusammengefasst. Diesmal werden allerdings nicht die einzelnen Technologien als solche betrachtet, sondern die am weitesten, zweitweitesten etc. Technologien unterschieden.") 
           ),
           p("Der Download Button unter der Abbildung erlaubt Ihnen die aktuelle Versionen der Abbildungen c-f als PDF herunterzuladen."),
           h3("Leitfragen:"),
           p("1. Welchen Einfluss hat die Netzwerktopologie auf die Tendenz zu technologischen Monopolisierung? Warum?"),
           p("2. Was ist der Einfluss des speziellen Technologienutzens und der gruppenbezogenen Technologiepräferenz?"),
           p("3. Was ist der Effekt der Anzahl der Technologien und Agenten auf die Modell-Dynamik? Ist dieser Effekt Ihrer Ansicht nach intuitiv?"),
           p("4. Welchen Unterschied macht es ob für die Berechnung des Netzwerknutzens der Anteil der Technologien an den bisher gewählten Technologien, oder der Anteil der Agenten, die diese Technologie verwenden, herangezogen wird? Welche Annahme ist für welche Situation plausibler?"),
           p("5. Unter welchen Umständen beobachten wir trotz unterschiedlicher Qualität der Technologien keine Monopolisierung? Warum?")
    )
  )
)



server <- function(input, output) {
  # Network plot
  output$network_1 <- renderPlot({
    network_used <- simul_results()[["networks"]][[input$single_adapt_case_netw]]
    if (input$network_topology=="Komplettes Netzwerk"){
      e_col <- adjustcolor("grey", alpha.f = .25)
      plot_layout <- layout.kamada.kawai(network_used)
    } else if (input$network_topology=="Ring"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout_in_circle(network_used)
    } else if (input$network_topology=="Barabasi-Albert"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout.graphopt(network_used)
    } else if(input$network_topology=="Small-World"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout_in_circle(network_used) 
    }
    plot(network_used, 
         edge.color=e_col,
         edge.arrow.size=1.2, 
         edge.curved=.0, 
         vertex.label=NA,
         vertex.size=8,
         vertex.color=col_blue,
         layout = plot_layout, xaxs="i", yaxs="i")
    
  })
  
  output$network_2 <- renderPlot({
    network_used <- simul_results()[["networks"]][[input$single_adapt_case_netw]]
    if (input$network_topology=="Komplettes Netzwerk"){
      e_col <- adjustcolor("grey", alpha.f = .25)
      plot_layout <- layout.kamada.kawai(network_used)
    } else if (input$network_topology=="Ring"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout_in_circle(network_used)
    } else if (input$network_topology=="Barabasi-Albert"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout.graphopt(network_used)
    } else if(input$network_topology=="Small-World"){
      e_col <- adjustcolor("grey", alpha.f = .75)
      plot_layout <- layout_in_circle(network_used) 
    }
    plot(network_used, 
         edge.color=e_col,
         edge.arrow.size=1.2, 
         edge.curved=.0, 
         vertex.label=NA,
         vertex.size=8,
         vertex.color=V(network_used)$technology,
         layout = plot_layout, xaxs="i", yaxs="i")
    
  })
  
  simul_results <- reactive({
    if (input$n_techs == 2){
      intrinsic_utilities <- c(input$int_util_1, input$int_util_2)
    } else if (input$n_techs == 3){
      intrinsic_utilities <- c(input$int_util_1, input$int_util_2, 
                               input$int_util_3)
    }else if (input$n_techs == 4){
      intrinsic_utilities <- c(input$int_util_1, input$int_util_2, 
                               input$int_util_3, input$int_util_4)
    } else {
      intrinsic_utilities <- c(input$int_util_1, input$int_util_2, 
                               input$int_util_3, input$int_util_4, 
                               input$int_util_5)
    }

    run_n_simulations(
      n_iterations=input$simulation_iterations, 
      n_agents=input$n_agents, 
      n_techs=input$n_techs, 
      network_topology=input$network_topology, 
      choose_mode=decision_kind_dict[[input$decision_kind]], 
      intrinsic_preference=input$int_util, 
      intrinsic_utilities=intrinsic_utilities)
  })
  
  output$single_adaption <- renderPlot({
    make_single_simul_dynamics(as.double(input$single_adapt_case), 
                               simul_results()[["data"]])
  })
  
  output$dyn_dom_tech <- renderPlot({
    plot_dynamics_dom_tech(simul_results()[["data"]])
  })
  
  output$final_shares_normal <- renderPlot({
    make_final_shares(simul_results()[["data"]], kind = "normal")
  })
  
  output$final_shares_ranked <- renderPlot({
    make_final_shares(simul_results()[["data"]], kind = "ranked")
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("tech_choice_", topology_dict[[input$network_topology]],
            ".pdf", sep = "")
    },
    content = function(file) {
      full_plot <- ggarrange(
        make_single_simul_dynamics(as.double(input$single_adapt_case),
                                   simul_results()[["data"]]),
        plot_dynamics_dom_tech(simul_results()[["data"]]),
        make_final_shares(simul_results()[["data"]], kind = "normal"),
        make_final_shares(simul_results()[["data"]], kind = "ranked"),
        ncol = 2, nrow = 2, font.label = list(face="bold"))
      plot_width <- 9
      ggsave(file, plot = full_plot, width = plot_width, height = 8)
    }
  )
  
}


shinyApp(ui = ui, server = server)
