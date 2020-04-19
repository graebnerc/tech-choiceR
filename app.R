library(shiny)
library(tidyverse)
library(igraph)

library(scales)
library(Hmisc)
library(wesanderson)
library(latex2exp)


source("helpers.R")
col_blue <- "#004c93"
col_light_blue <- "#dfe4f2"
decision_kind_dict <- list(
  "Anteile"="share",
  "Absolute Nutzer*innenzahl"="absolute"
)
# library(magrittr)
# library(ggpubr)
# source("helpers.R")
# p_range <- seq(0, 10, 0.1)
# t_input <- seq(0, 40)

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
           h3("Fallspezifische Einstellungen"),
           selectInput("decision_kind", 
                       label = "Art der Technologiewahl",
                       choices = c("Anteile"#, "Absolute Nutzer*innenzahl"
                                   ),
                       selected = "Anteile"),
           selectInput("network_topology", 
                       label = "Netzwerktopologie Fall 1",
                       choices = c("Komplettes Netzwerk", 
                                   "Ring"), # Barabasi, Small World; dann mit optionalem panel
                       selected = "Anteile")
    ),
    column(3,
           h3("a) Netzwerktopologie"),
           plotOutput("network_1")
    ),
    column(3,
           h3("b) Einzelne Adaptionsdynamiken"),
           numericInput("single_adapt_case", label = "Iteration", 
                        min = 2, max = 100, step=1, value = 1),
           plotOutput("single_adaption")
    )
  ),
  fluidRow(
    column(3),
    column(3,
           h3("c) Dynamik der dominanten Technologien"),
           plotOutput("dyn_dom_tech")
           ),
    column(3,
           h3("d) Finale Anteile der einzelnen Technologien"),
           plotOutput("final_shares_normal")
    ),
    column(3,
           h3("e) Finale Anteile der nach Anteilen gerankten Technologien"),
           plotOutput("final_shares_ranked")
    )
    ),
  fluidRow(
    column(3),
    column(9,
           #downloadButton("downloadPlot", "Download der Abbildungen im PDF Format"),
           h3("Beschreibung des Modells"),
           #p("Eine genaue Beschreibung des Modelles und der Implementierung in R finden Sie im Begleitdokument (Moodle oder auf Github im Ordner 'beschreibung')."),
           h3("Benutzung der App"),
           #p("Parameterwerte können auf der linken Seite geändert werden. Der Download Button unter der Abbildung erlaubt Ihnen die aktuelle Version der Abbildungen als PDF herunterzuladen."),
           h3("Leitfragen:")#,
           #p("1. Welchen Einfluss haben die Achsenabschnitte auf die Preisdynamiken?"),
           #p("2. Was sind die Implikationen unterschiedlicher Ausgangspreise?"),
           #p("3. Wie verändern sich die Preisanpassungsdynamiken bei unterschiedlichen Angebots- und Nachfrageelastizitäten?"),
           #p("4. Was sind die wichtigsten Determinanten der Konvergenz- bzw. Divergenzgeschwindigkeit? Was ist hier die Intuition?")
    )
  )
)


server <- function(input, output) {
  # Network plot
  output$network_1 <- renderPlot({
    network_used <- create_network(input$n_agents, 
                                   input$n_techs, 
                                   input$network_topology)
    if (input$network_topology=="Komplettes Netzwerk"){
      e_col <- adjustcolor("grey", alpha.f = .25)
      plot_layout <- layout.kamada.kawai(network_used)
    } else if (input$network_topology=="Ring"){
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
                               simul_results())
  })
  
  output$dyn_dom_tech <- renderPlot({
    plot_dynamics_dom_tech(simul_results())
  })
  
  output$final_shares_normal <- renderPlot({
    make_final_shares(simul_results(), kind = "normal")
  })
  
  output$final_shares_ranked <- renderPlot({
    make_final_shares(simul_results(), kind = "ranked")
  })
  
}


shinyApp(ui = ui, server = server)
