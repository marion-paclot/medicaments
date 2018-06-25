library(shiny)
library(datasets)
library(grid)
library(gridExtra)
library(plyr)


space <- function(x, ...) { 
  format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}

ajouter_titre <- function(titre, graphique){
  graphique %>% 
    add_annotations(
      yref="paper", xref="paper", y=1.1, x=0, 
      text=titre, 
      showarrow=F ) %>% 
    layout(title=FALSE)
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
    ### Extraction du numéro de la famille de générique
    selection = reactive({
      cat('**************', input$medicament, '****************\n')
      
      # Numéro de famille associé à ces produits
      num = unique(subset(bdd, denomination == input$medicament)$numFamille)
      num = num[!is.na(num)]
      print(num)
      # Base des produits appartenant à la famille
      if (length(num)>0){
        bdd_produits = subset(bdd, numFamille == num)
      }else{
        num_cis = subset(bdd, denomination == input$medicament)$CIS
        bdd_produits = subset(bdd, CIS %in% num_cis)
      }
      
      # Existence de doublons, on suffixe par le numéro de tri
      doublons = unique(bdd_produits[, c('nomCIS', 'tri')])
      doublons = doublons$nomCIS[duplicated(doublons$nomCIS)]
      
      bdd_produits$NOM_CIP13 = ifelse(bdd_produits$NOM_CIP13 %in% doublons, 
                                   paste0(bdd_produits$NOM_CIP13, '_', bdd_produits$tri),
                                   bdd_produits$NOM_CIP13)
      bdd_produits$nomCIS = ifelse(bdd_produits$nomCIS %in% doublons, 
                                   paste0(bdd_produits$nomCIS, '_', bdd_produits$tri),
                                   bdd_produits$nomCIS)

      # # Base de la consommation des produits appartenant à la famille
      bdd_conso = subset(conso, CIP13 %in% bdd_produits$CIP13)
      
      # Jointure produit conso
      conso_cip13 = merge(bdd_conso, bdd_produits, by = "CIP13", all= FALSE)
      conso_cip13 = conso_cip13[with(conso_cip13, order(type, tri, mois)),]
      conso_cip13$NOM_CIP13 = factor(conso_cip13$NOM_CIP13, levels = unique(conso_cip13$NOM_CIP13))
      
      conso_cip13 = data.table(conso_cip13)
      conso_cip13 = conso_cip13[, .(Nb= sum(Nb, na.rm = TRUE),  
                                    Mt = sum(Mt, na.rm = TRUE), 
                                    Base = sum(Base, na.rm = TRUE)), 
                                by=list(NOM_CIP13, type, tri, mois, CIS, nomCIS, unite_final)] 
      # S'il y a des noms doublés, on modifie le nom des variables NOM_CIP13 et nomCIS

      # Conso unite
      conso_cis <- conso_cip13 # Copie de la table
      conso_cis = conso_cis[, .(Nb_unite = sum(Nb*unite_final, na.rm = TRUE), 
                                  Mt = sum(Mt, na.rm = TRUE), 
                                  Base = sum(Base, na.rm = TRUE)), 
                              by=list(CIS, nomCIS, type, tri, mois)] 
      conso_cis = conso_cis[with(conso_cip13, order(type, tri, mois)),]
      conso_cis = subset(conso_cis, !is.na(nomCIS))
      conso_cis$nomCIS = factor(conso_cis$nomCIS, levels = unique(conso_cis$nomCIS))
      conso_cis = data.frame(conso_cis)
      
      # Couleur CIP
      couleurs_cip = unique(conso_cip13[, c('NOM_CIP13', 'type', 'tri')])
      couleurs_cip$col = NA
      couleurs_cip$col[couleurs_cip$type == 0] = colorRampPalette(c("pink", "red"))(sum(couleurs_cip$type == 0))
      couleurs_cip$col[couleurs_cip$type != 0] = colorRampPalette(c("blue", "green"))(sum(couleurs_cip$type != 0))
      conso_cip13 = merge(conso_cip13, couleurs_cip)
      conso_cip13 = conso_cip13[order(conso_cip13$tri),]
      
      # Couleur CIS
      couleurs_cis = unique(conso_cis[, c('nomCIS', 'type', 'tri')])
      couleurs_cis$col = NA
      couleurs_cis$col[couleurs_cis$type == 0] = colorRampPalette(c("pink", "red"))(sum(couleurs_cis$type == 0))
      couleurs_cis$col[couleurs_cis$type != 0] = colorRampPalette(c("blue", "green"))(sum(couleurs_cis$type != 0))
      conso_cis = merge(conso_cis, couleurs_cis)
      conso_cis = conso_cis[order(conso_cis$tri),]
      
      # Sortie
      donnees  = list(num = num, 
                      bdd_produits = bdd_produits, 
                      bdd_conso = bdd_conso, 
                      conso_cip13 = conso_cip13,
                      conso_cis = conso_cis, 
                      couleurs_cip = couleurs_cip, 
                      couleurs_cis = couleurs_cis)
      return(donnees)
    })
    
    
    ############################################################################
    ### Encart avec le nom de la famille de génériques
    output$famille <- renderText({
      bdd_produits = selection()$bdd_produits
      num =  selection()$num
      
      print(num)
      affichage_famille = paste0("Le médicament n'appartient pas à une famille de générique",
                                 "\n", bdd_produits$NOM_CIP13[1])
      if (length(num) == 1){
        affichage_famille = nom_famille$nomFamille[nom_famille$numFamille %in% num]
      }
      
      affichage_famille
    })
    
    ############################################################################
    ### Nb de boites vendues
    output$nb_boite <- renderPlotly({
      conso_cip13 = selection()$conso_cip13
      couleurs = selection()$couleurs_cip$col
      conso_cip13$tooltip = paste0(conso_cip13$NOM_CIP13,
                                  '<br>', format(as.Date(conso_cip13$mois), format = "%B %Y"),
                                  '<br>', conso_cip13$Nb)
      
      # Graphiques
      p_prov = ggplot(data = conso_cip13, alpha = 0.9,
                 aes(y = Nb, x = mois, group = NOM_CIP13, 
                     color = NOM_CIP13, fill = NOM_CIP13, text = tooltip)) +   
        scale_color_manual(values = couleurs) +
        labs(y=" ", x= " ", title = "Prov") + 
        theme(legend.title=element_blank()) +
        scale_y_continuous(labels = space)
      
      # Une ligne par produit
      if (input$boite_plotType == 's') { p = p_prov + geom_line()}
      # En cumulé
      if (input$boite_plotType == 'c') {
        p = p_prov + geom_area(stat = "identity") + 
          scale_fill_manual(values = couleurs)
        }
      
      p = ggplotly(p, tooltip = c("text")) 
      p = ajouter_titre("Nombre de boites remboursées - en ville", p)
      
      # p = ggplotly(ggplot(data = diamonds[1:100,], aes(x = depth, y = price)) + geom_point())
    })
    

    
    ############################################################################
    ### Nb d'unites vendues
    output$nb_unite <- renderPlotly({
      conso_cis = selection()$conso_cis
      couleurs = selection()$couleurs_cis$col
      conso_cis$tooltip = paste0(conso_cis$nomCIS,
                                 '<br>', format(as.Date(conso_cis$mois), format = "%B %Y"),
                                 '<br>', conso_cis$Nb_unite)
      
      # Graphiques
      p_prov = ggplot(data = conso_cis, alpha = 0.9,
                      aes(y = Nb_unite, x = mois, group = nomCIS, 
                          color = nomCIS, fill = nomCIS, text = tooltip)) +   
        scale_color_manual(values = couleurs) +
        labs(y=" ", x= " ", title = "Prov") + 
        theme(legend.title=element_blank()) +
        scale_y_continuous(labels = space)
      
      # Une ligne par produit
      if (input$unite_plotType == 's') {p = p_prov + geom_line()}
      # En cumulé
      if (input$unite_plotType == 'c') {
        p = p_prov + geom_area(stat = "identity") + 
          scale_fill_manual(values = couleurs)
      }
      
      p = ggplotly(p, tooltip = c("text")) 
      p = ajouter_titre("Nombre d'unités remboursées - en ville", p)
      # p = ggplotly(ggplot(data = diamonds[1:100,], aes(x = depth, y = price)) + geom_point())
      
    })
    
    
    ############################################################################
    ### Depense
    output$depense <- renderPlotly({
      conso_cis = selection()$conso_cis
      couleurs = selection()$couleurs_cis$col
      conso_cis$tooltip = paste0(conso_cis$nomCIS,
                                   '<br>', format(as.Date(conso_cis$mois), format = "%B %Y"),
                                   '<br>', round(conso_cis$Mt))
      # Graphique
      p_prov = ggplot(data = conso_cis, alpha = 0.9,
                 aes(y = Mt, x = mois, group = nomCIS, 
                     color = nomCIS, fill = nomCIS, text = tooltip)) +
        scale_color_manual(values = couleurs) +
        labs(y=" ", x= " ", title = "Prov") + 
        theme(legend.title=element_blank()) +
        scale_y_continuous(labels = space) 
      
      # Une ligne par produit
      if (input$depense_plotType == 's') { p = p_prov + geom_line()}
      # En cumulé
      if (input$depense_plotType == 'c') {
        p = p_prov + geom_area(stat = "identity") + 
          scale_fill_manual(values = couleurs)
        }

      p = ggplotly(p, tooltip = c("text"))
      p = ajouter_titre("Montant remboursé - en ville", p)
      # p = ggplotly(ggplot(data = diamonds[1:100,], aes(x = depth, y = price)) + geom_point())
      
    })
    
    # Prix
    output$prix <- renderDataTable({
      bdd_produits = selection()$bdd_produits
      col_a_garder = c('CIS', 'CIP13', 'NOM_CIP13', 'statut',
                       'agrement', 'taux', 'prixHorsHono', 'prixTotal', 'honoraires',
                       'unite_final', 'contenance')
      bdd_produits = bdd_produits[, col_a_garder]
      print(head(bdd_produits))
      bdd_produits
    })

    
    
})



#   # Part de marché
#   output$partMarche <- renderPlotly({
#     pdm_famille = subset(pdm, N == famille_reactive())
#     # pdm_famille = subset(pdm, N == '1104')
#     pdm_famille = pdm_famille[order(pdm_famille$type),]
#     pdm_famille = pdm_famille[order(pdm_famille$tri),]
#     pdm_famille = pdm_famille[order(pdm_famille$mois),]
#     
#     pdm_famille$PRODUIT = factor(pdm_famille$PRODUIT, levels = unique(pdm_famille$PRODUIT))
#     pdm_famille = pdm_famille[order(pdm_famille$mois),]
#     pdm_famille$tooltip = paste0(pdm_famille$NOM_CIS, '\n', 
#                                  substr(as.character(as.Date(pdm_famille$mois, format = "%Y-%m")),1,7),
#                                  ' : ', round(100*pdm_famille$part_de_marche), '%')
#     print(head(pdm_famille))
#     couleurs = unique(pdm_famille[, c('type', 'tri')])
#     n_princeps = sum(couleurs$type == 0)
#     n_gen = sum(couleurs$type != 0)
# 
#     couleurs$col = NA
#     couleurs$col[couleurs$type == 0] = colorRampPalette(c("red", "pink"))(n_princeps)
#     couleurs$col[couleurs$type != 0] = colorRampPalette(c("blue", "green"))(n_gen)
# 
#     p = ggplot(data = pdm_famille,
#                aes(y = part_de_marche, x = mois, fill = PRODUIT), 
#                text = tooltip, alpha = 0.9) +
#       geom_area(stat = "identity") +
#       scale_y_continuous(labels = percent) +
#       scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m",
#                    date_minor_breaks = "1 month") +

#     
#     p = ggplotly(p)
#     p <- ggplotly(p, tooltip = c("label"))%>%
#       add_annotations(
#         yref="paper", xref="paper", 
#         y=1.1, x=0, text="Parts de marché cumulées", showarrow=F) %>% 
#       layout(title=FALSE)
#   })
#   

