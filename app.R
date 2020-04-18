library(shiny)
library(ggplot2)
library(igraph)
col_blue <- "#004c93"
col_light_blue <- "#dfe4f2"
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
           sliderInput("n_agents", 
                       label='Anzahl der Agenten \\( N \\)',
                       min = 2, max = 100, step=1, value = 50),
           numericInput("n_techs", label = h3("Anzahl Technologien"), 
                        value = 1, min = 2, max = 5, step = 1),
           h4("Technologieeigenschaften"),
           sliderInput("int_util_1", 
                       label='Intrinsischer Nutzen von \\( T_1 \\)',
                       min = 0.0, max = 1.0, step=0.05, value = 0.5),
           sliderInput("int_util_1", 
                label='Intrinsischer Nutzen von \\( T_1 \\)',
                min = 0.0, max = 1.0, step=0.05, value = 0.5),
           sliderInput("int_util_2", 
                       label='Intrinsischer Nutzen von \\( T_2 \\)',
                       min = 0.0, max = 1.0, step=0.05, value = 0.5),
           conditionalPanel(
             condition = "input.n_techs>2",
             sliderInput("int_util_3", 
                         label='Intrinsischer Nutzen von \\( T_3 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.5)
             ),
           conditionalPanel(
             condition = "input.n_techs>3",
             sliderInput("int_util_4", 
                         label='Intrinsischer Nutzen von \\( T_4 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.5)
           ),
           conditionalPanel(
             condition = "input.n_techs>4",
             sliderInput("int_util_5", 
                         label='Intrinsischer Nutzen von \\( T_5 \\)',
                         min = 0.0, max = 1.0, step=0.05, value = 0.5)
           )
  ),
    column(3, 
           h3("Einstellungen Fall 1"),
           selectInput("decision_kind_1", 
                       label = "Art der Technologiewahl",
                       choices = c("Anteile", 
                                   "Absolute Nutzer*innenzahl"),
                       selected = "Anteile"),
           selectInput("network_topology_1", 
                       label = "Netzwerktopologie Fall 1",
                       choices = c("Komplettes Netzwerk", 
                                   "Ring"), # Barabasi, Small World; dann mit optionalem panel
                       selected = "Anteile")
    ),
    column(3,
           h3("Netzwerkstruktur Fall 1"),
           plotOutput("network_1")
    ),
    column(3,
           h3("Adaptionsdynamiken Fall 1")#, plotOutput("adaption_1")
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
  # Create the network

  
  output$network_1 <- renderPlot({
    network_topology_1 <- input$network_topology_1
    n_agents <- input$n_agents
    
    if (network_topology_1=="Komplettes Netzwerk"){
      network_used <- make_full_graph(
        n_agents, directed = FALSE, loops = FALSE)
      plot_layout <- layout.kamada.kawai(network_used)
      e_col <- adjustcolor("grey", alpha.f = .25)
    } else if (network_topology_1=="Ring"){
      network_used <- make_ring(
        n_agents, directed = FALSE, mutual = FALSE, circular = TRUE)
      plot_layout <- layout_in_circle(network_used)
      e_col <- adjustcolor("grey", alpha.f = .95)
    } else {
      stop("No correct network topology given!")
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
}


shinyApp(ui = ui, server = server)
