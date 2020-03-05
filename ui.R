
library(shinydashboard)
ui <- dashboardPage(
   title="Médicaments", # Titre de l'onglet
   
    # Ligne d'entête
      dashboardHeader(
         titleWidth = "100%",
         title = HTML(
            "<div>
            <p align='left'> Dashboard médicament
            <a href='https://github.com/marion-paclot/medicaments' target='_blank'>
            <img src = 'GitHub-Mark/PNG/GitHub-Mark-64px.png' height = '30px', align= 'right', vspace='10'>
            </a></p>
            </div>")
         ),
      
  dashboardSidebar(
    collapsed = FALSE,
    
    # Menu de gauche
    sidebarMenu( 
      width = 4,
      id = "tabs",
      selectizeInput("specialite", "Spécialité",
                     choices = tousCIP, 
                     selected = tousCIP[1], 
                     multiple = FALSE,  width = "100%"),
      br(),
      menuItem("Consommation", tabName = "consommation", icon = icon("user-circle")),
      menuItem("Dépense", tabName = "depense", icon = icon("user-circle")),
      menuItem("Brevets et CCP", tabName = "brevet", icon = icon("user-circle"))
      
      )
    ),
  
  dashboardBody(
    fluidRow(
      column(width = 12),
      tags$head(
        # Lien vers le fichier CSS de l'application
        tags$link(rel = "stylesheet", type = "text/css", href = "/style.css")),
      tags$style(type='text/css', "#contenuFamille {white-space: pre-wrap;
                 font-size: 14px; 
                      font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;}"),
      

      
      mainPanel(width = 12,
         verbatimTextOutput('contenuFamille')
         ),
    
      tabItems(
        
        # Consommation par boite ============================
        tabItem(
          tabName = "consommation",
          mainPanel(
            width = 12,
            fluidRow(column(3, radioButtons("individuel_ou_cumul", "Consommation",
                                   c("Par produit"= "p", "En cumulé"= "c"),
                                   inline = TRUE)),
            column(3, radioButtons("boite_ou_unite", "Décompte",
                         c("# boites"= "b", "# unités"= "u"),
                         inline = TRUE)),
            column(3, radioButtons("volume_ou_pdm", "Affichage",
                                   c("Volume"= "volume", "Part de marché"= "pdm"),
                                   inline = TRUE)),
           column(3, radioButtons("ville_ou_hopital", "Lieu de délivrance",
                 c("Hôpital"= "h", "Ville"= "v", "Tous" = "hv"),selected = 'hv',
                 inline = TRUE))),
           plotlyOutput("consommation")
          )
        ),
        
        # Prix  ============================
        tabItem(
          tabName = "depense",
          mainPanel(width = 12
          )
        ),
        
        # Brevets  ============================
        tabItem(
           tabName = "brevet",
           mainPanel(width = 12,
                     htmlOutput("notice", )
           )
        )
      )
    )
  )
)

  


