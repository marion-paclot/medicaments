
# Define UI 
shinyUI(fluidPage(

  
  # Application title
  titlePanel("Consommation mensuelle de médicaments"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  fluidRow(
    column(4,
           selectizeInput("medicament", "Sélection d'un médicament",
                          choices = unique(bdd$denomination[!is.na(bdd$denomination)]), 
                          selected = "", multiple = FALSE,
                          options = NULL, width = "100%")
    ),
    column(8, h2(verbatimTextOutput("famille")))
    ),

    mainPanel(width = 12,
              
              
      tabsetPanel(type = "tabs",
                  tabPanel("Nombre de boites", 
                           br(),
                           sidebarLayout(
                             sidebarPanel( width = 2,
                               radioButtons("boite_plotType", "Affichage",
                                            c("Par produit"= "s", "Cumulé"= "c")
                               )
                             ),
                             mainPanel(width = 10, 
                                       plotlyOutput("nb_boite"))
                             )
                           ),
                  tabPanel("Nombre d'unités",
                           br(),
                           sidebarLayout(
                             sidebarPanel( width = 2,
                                           radioButtons("unite_plotType", "Affichage",
                                                        c("Par produit"= "s", "Cumulé"= "c"),
                                           ),
                                           checkboxInput('unite_prop', 'En part de marché', 
                                                         value = FALSE,)
                             ),
                             mainPanel(width = 10, 
                                       plotlyOutput("nb_unite"))
                           )
                           ),
                  tabPanel("Dépense",
                           br(),
                           sidebarLayout(
                             sidebarPanel( width = 2,
                                           radioButtons("depense_plotType", "Affichage",
                                                        c("Par produit"= "s", "Cumulé"= "c")
                                           )
                             ),
                             mainPanel(width = 10,
                                       plotlyOutput("depense"))
                           )
                  ),
                  tabPanel("Prix",
                           br(),
                           dataTableOutput("prix")
                           
                  ),
                  tabPanel("Brevets",
                           br()
                      
                  )
      )
  )
))



#   
#   # Show the caption and plot of the requested variable against mpg
#   # mainPanel(width = 12,
#   #   tabsetPanel(type = "tabs",
#   #               tabPanel("Parts de marché",
#   #                        plotlyOutput("partMarche")),
#   #               tabPanel("Dépense par laboratoire",
#   #                       plotlyOutput("depense_mensuelle")),
#   #               tabPanel("Dépense cumulée",
#   #                        plotlyOutput("depense_mensuelle_cumul")),
#   #               tabPanel("Prix",
#   #                        tableOutput("prix"))
#   #   ))
# ))