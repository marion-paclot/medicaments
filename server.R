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

definir_couleurs <- function (base, col_code, col_nom){
   couleurs = unique(base[, c(col_code, col_nom, 'typemed', 'tri')])
   
   couleurs = couleurs[order(couleurs$tri),]
   colnames(couleurs) = c('code', 'nom', 'typemed', 'tri')
   couleurs = unique(couleurs[, c('code', 'nom', 'typemed')])
   
   couleurs$col = NA
   couleurs$col[couleurs$typemed == 0] = colorRampPalette(c("pink", "red"))(sum(couleurs$typemed == 0))
   couleurs$col[couleurs$typemed != 0] = colorRampPalette(c("blue", "green"))(sum(couleurs$typemed != 0))
   
   return(couleurs)
}

extraireNotice = function(num){
   url_ccp = sprintf("https://bases-brevets.inpi.fr/fr/document/FR%s.html", num)
   contenu = htmlTreeParse(getURL(url_ccp), useInternalNodes = T)
   divNotice = xpathSApply(contenu, "//div[@class='notice']")[[1]]
   divNotice = as(divNotice, "character")
   divNotice = gsub('<ul.*/ul>', '', divNotice)
   return(divNotice)
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
   
   # Menu déroulant fournissant la liste des produits disponibles
   # updateSelectizeInput(session, "specialite", 'Spécialité',
   #                      choices = tousCIP, server = TRUE)
   
   selection = reactive({
      
      if (input$specialite %in% unique(tousCIP)){
         denominationProduit <<- input$specialite
      }
      cat('**************', denominationProduit, '****************\n')
      
      #########################################################################
      ### Requêtes vers la base de données
      #########################################################################
      
      # Infos sur le médicament
      reqReferentielProduit = glue_sql("SELECT DISTINCT *
                                       FROM referentiel 
                                       WHERE denomination IN ({denomination*});",
                                       denomination = denominationProduit,
                                       .con = con)
      
      referentielProduit <- set_utf8(dbGetQuery(con, reqReferentielProduit))
      
      # Infos sur la famille
      reqReferentielFamille = glue_sql("SELECT DISTINCT *
                                       FROM referentiel 
                                       WHERE numfamille IN ({numfamille*});",
                                       numfamille = unique(referentielProduit$numfamille),
                                       .con = con)
      referentielFamille <- set_utf8(dbGetQuery(con, reqReferentielFamille))
      
      # Consommaion de la famille
      reqConsommationFamille = glue_sql("SELECT DISTINCT cip13, lieu, mois, 
                                        coalesce(nb, 0) AS nb, coalesce(mt, 0) AS mt, coalesce(base, 0) AS base
                                        FROM consommation 
                                        WHERE cip13 IN ({cip13*})
                                        ORDER BY mois;",
                                        cip13 = referentielFamille$cip13, 
                                        .con = con)
      consommationFamille <- dbGetQuery(con, reqConsommationFamille)
      
      # Ajout des dates manquantes (ie conso à 0 ou négatives)
      req_allDates = glue_sql("SELECT DISTINCT cip13, lieu
                                       FROM consommation 
                                        WHERE cip13 IN ({cip13*});",
                                        cip13 = referentielFamille$cip13, 
                                        .con = con)
      allDates <- dbGetQuery(con, req_allDates)
      allDates = merge(allDates, listeMois)
      colnames(allDates) = c('cip13', 'lieu', 'mois')
      consommationFamille = merge(consommationFamille, allDates, all.y = TRUE)
      consommationFamille[is.na(consommationFamille)] = 0
      
      #########################################################################
      ###### Retraitement des données 
      #########################################################################
      
      consommationFamille$lieu = factor(consommationFamille$lieu, 
                                        levels = c("Hopital", "Ville"))
      consommationFamille$mois = factor(consommationFamille$mois)


      # Identification des produits qui ont des doublons (deux formes identiques sauf le cip13)
      # exemple RAMIPRIL ARROW LAB 5 MG CPR SEC 90
      # Pour les CIP
      doublons = referentielFamille[ ave(1:nrow(referentielFamille), referentielFamille$nomcip, FUN=length) > 1, ]
      if (nrow(doublons)>0){
         for (nom in unique(doublons[, 'nomcip'])){
            cip_doublons = doublons$cip13[doublons$nomcip == nom]
            dernier_cip = max(cip_doublons)
            for (cip in setdiff(cip_doublons, dernier_cip)){
               consommationFamille$cip13 = gsub(cip, dernier_cip, consommationFamille$cip13)
            }
         }
         # On efface la ligne qui a été fusionnée
         referentielFamille = subset(referentielFamille, cip13 %in% unique(consommationFamille$cip13))
      }
      
      # Pour les CIS
      referentielFamille_light = unique(referentielFamille[, c('nomcis', 'cis')])
      doublons = referentielFamille_light[ ave(1:nrow(referentielFamille_light), 
                                               referentielFamille_light$nomcis, FUN=length) > 1, ]
      if (nrow(doublons)>0){
         for (nom in unique(doublons[, 'nomcis'])){
            cis_doublons = doublons$cis[doublons$nomcis == nom]
            dernier_cis = max(cis_doublons)
            for (cis in setdiff(cis_doublons, dernier_cis)){
               referentielFamille$cis = gsub(cis, dernier_cis, referentielFamille$cis)
            }
         }
      }
      
      
      # Jointure referentielFamille et consommationFamille par le cip13
      consoEnrichie = merge(consommationFamille, referentielFamille, by = "cip13", all= FALSE)
      consoEnrichie = consoEnrichie[with(consoEnrichie, order(typemed, tri, mois)),]
      
      consoEnrichie = data.table(consoEnrichie)
      consoEnrichie = consoEnrichie[, .(nb= sum(nb, na.rm = TRUE),
                                        mt = sum(mt, na.rm = TRUE),
                                        base = sum(base, na.rm = TRUE)),
                                    by=list(cip13, nomcip, typemed, tri, mois, lieu, cis, nomcis, nbunite)]
      
      # Conso unite
      consoCisEnrichie <- consoEnrichie # Copie de la table
      consoCisEnrichie = consoCisEnrichie[, .(nb = sum(nb*nbunite, na.rm = TRUE),
                                              mt = sum(mt, na.rm = TRUE),
                                              base = sum(base, na.rm = TRUE)),
                                          by=list(cis, nomcis, typemed, tri, mois, lieu)]
      consoCisEnrichie = consoCisEnrichie[with(consoEnrichie, order(typemed, tri, mois)),]
      consoCisEnrichie = subset(consoCisEnrichie, !is.na(nomcis))
      
      consoCisEnrichie = data.frame(consoCisEnrichie)
      consoEnrichie = data.frame(consoEnrichie)
      
      ## Gestion des couleurs
      couleursCIP = definir_couleurs(consoEnrichie, "cip13", "nomcip")
      couleursCIS = definir_couleurs(consoCisEnrichie, "cis", "nomcis")
      
      ## Doc number pour les CCP
      CIS = referentielFamille$cis
      doc_number = ccp$doc_number[ccp$CIS %in% CIS]
      test <<- consoEnrichie                           
      return(list(
         referentielProduit = referentielProduit,
         referentielFamille = referentielFamille,
         consommationFamille = consommationFamille,
         consoEnrichie = consoEnrichie,
         consoCisEnrichie = consoCisEnrichie,
         couleursCIP = couleursCIP,
         couleursCIS = couleursCIS,
         doc_number = doc_number
      )
      )
      
   })
   
   #############################################################################
   # CCP et brevets
   output$notice <- renderText({
      doc_number = unique(selection()$doc_number)
      
      if (length(doc_number) == 0){
         notice = "<div>Pas de CCP associé</div>"
      }
      if (length(doc_number) == 1){
         notice = extraireNotice(doc_number)
      }
      if (length(doc_number) > 1){
         notice = ""
         for (doc in doc_number){
            notice = paste0(notice, extraireNotice(doc))
            notice = paste0(notice, '<hr style="color: black;">')
         }
      }
      notice = gsub('\n', '', notice)
      return(notice)

   })
   
   # ############################################################################
   # ### Encart avec le nom de la famille de génériques
   output$contenuFamille <- renderText({
      numFamilleProduit = unique(selection()$referentielProduit$numfamille)
      nomFamilleProduit = unique(selection()$referentielProduit$nomfamille)
      contenu = nomFamilleProduit
      print(contenu)
      if (grepl('^HF_', numFamilleProduit)){
         contenu = paste0("Le médicament n'appartient pas à une famille de générique",
                          "\r\n", contenu)
      }
      
      return(contenu)
   })
   
   ############################################################################
   ### Nb de boites vendues
   output$consommation <- renderPlotly({
      
      #### Boite ou unite
      if (input$boite_ou_unite == 'b') { 
         conso = selection()$consoEnrichie
         couleurs = selection()$couleursCIP
         colnames(conso) = gsub('nomcip', 'nom', colnames(conso))
         colnames(conso) = gsub('cip13', 'code', colnames(conso))
      }
      if (input$boite_ou_unite == 'u') { 
         conso = selection()$consoCisEnrichie
         couleurs = selection()$couleursCIS
         colnames(conso) = gsub('nomcis', 'nom', colnames(conso))
         colnames(conso) = gsub('cis', 'code', colnames(conso))
      }
      
      # Pour s'assurer que l'ordre est le bon
      couleurs$nom = factor(couleurs$nom, levels = couleurs$nom)
      conso$nom = factor(conso$nom, levels = couleurs$nom)
      
      
      #### Hopital, ville, les deux
      if (input$ville_ou_hopital == "h") {
         conso = subset(conso, lieu == "Hopital")
      }
      if (input$ville_ou_hopital == "v") {
         conso = subset(conso, lieu == "Ville")
      }
      # Si input$lieu == 'hv', pas de filtrage, mais on somme les valeurs
      conso = data.table(conso)
      conso = conso[, .(nb= sum(as.numeric(nb), na.rm = TRUE),
                        mt = sum(as.numeric(mt), na.rm = TRUE),
                        base = sum(as.numeric(base), na.rm = TRUE)),
                    keyby=c('nom', 'code', 'mois')]
      
      
      #### Volume ou part de marché
      if (input$volume_ou_pdm == "pdm"){
         conso = conso[, nb := nb/sum(nb, na.rm = TRUE), by = list(mois)]
      }
      
      ## Graphiques
      conso = data.frame(conso)
      conso$tooltip = paste0(conso$nom,
                             '<br>', format(as.Date(conso$mois), format = "%B %Y"),
                             '<br>', conso$nb)
      conso$mois = as.Date(conso$mois)
      
      
      p_prov = ggplot(data = conso, alpha = 0.9,
                      aes(y = nb, x = mois, group = nom,
                          color = nom, fill = nom, text = tooltip)) +
         labs(y=" ", x= " ", title = " ") + 
         theme(legend.title=element_blank(), legend.text=element_text(size=7)) +
         scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", date_minor_breaks = "1 month") +
         scale_color_manual(breaks = levels(couleurs$nom), values= couleurs$col)
      
      # Affichage des labels en % si part de marché
      if (input$volume_ou_pdm == "pdm"){
         p_prov = p_prov + 
            scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1))
      }
      if (input$volume_ou_pdm == "volume"){
         p_prov = p_prov + scale_y_continuous(labels = space) + expand_limits(y=0)
      }
      
      # Une ligne par produit
      if (input$individuel_ou_cumul == 'p') { p_prov = p_prov + geom_line()}
      # En cumulé
      if (input$individuel_ou_cumul == 'c') {
         p_prov = p_prov + geom_area(stat = 'identity', position = "stack") +
            scale_fill_manual(breaks = levels(couleurs$nom), values= couleurs$col)
      }
      
      p = ggplotly(p_prov, tooltip = c("text"))
   })
   
   output$mitm <- renderUI({
      mitm = selection()$referentielProduit$mitm
      image = "non_mitm.png"
      if(any(mitm)){
         image = "mitm.png"
      }
      tags$img(src= image, width = '90px')
      })
   
   output$vente_libre <- renderUI({
      classement = selection()$referentielProduit$condition
      print(classement)
      image = "ordo.png"
      if(any(classement == "vente libre")){
         image = "non_ordo.png"
      }
      tags$img(src= image, width = '90px')
   })
   
   output$acces_direct <- renderUI({
      acces = selection()$referentielProduit$acces_direct
      image = "non_acces_direct.png"
      if(any(acces)){
         image = "acces_direct"
      }
      tags$img(src= image, width = '90px')
   })
   

   output$downloadData <- downloadHandler(
      filename = 'Donnees_medicaments.xlsx',
      content = function(file) {
         conso = selection()$consommationFamille
         produit = selection()$referentielProduit
         donnees_conso = merge(produit, 
                               conso, by = "cip13", all = TRUE)
         for (col in c('numfamille', 'nomfamille', 'typemed', 'nomcip',	'numfamilledci')){
            donnees_conso[, col] = NULL
         }
         write.xlsx(donnees_conso, file, sheetName="consommation",
                     col.names=TRUE, row.names=FALSE, append=FALSE)
      }
   )
   
   # ############################################################################
   # ### Depense
   # output$depense <- renderPlotly({
   #   consoCisEnrichie = selection()$consoCisEnrichie
   #   couleurs = selection()$couleursCIS$col
   #   consoCisEnrichie$tooltip = paste0(consoCisEnrichie$nomcis,
   #                                '<br>', format(as.Date(consoCisEnrichie$mois), format = "%B %Y"),
   #                                '<br>', round(consoCisEnrichie$Mt))
   #   # Graphique
   #   p_prov = ggplot(data = consoCisEnrichie, alpha = 0.9,
   #              aes(y = Mt, x = mois, group = nomcis,
   #                  color = nomcis, fill = nomcis, text = tooltip)) +
   #     scale_color_manual(values = couleurs) +
   #     labs(y=" ", x= " ", title = "Prov") +
   #     theme(legend.title=element_blank()) +
   #     scale_y_continuous(labels = space) +
   #     scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", date_minor_breaks = "1 month")
   # 
   # 
   #   # Une ligne par produit
   #   if (input$affichage == 's') { p = p_prov + geom_line()}
   #   # En cumulé
   #   if (input$affichage == 'c') {
   #     p = p_prov + geom_area(stat = "identity") +
   #       scale_fill_manual(values = couleurs)
   #     }
   # 
   #   p = ggplotly(p, tooltip = c("text"))
   #   p = ajouter_titre("Montant remboursé - en ville", p)
   # })
   # 
   
   
   
   # # Prix
   # output$prix <- renderDataTable({
   #   referentielFamille = selection()$referentielFamille
   #   col_a_garder = c('cis', 'cip13', 'nomcip', 'statut', 'typemed', 'tri',
   #                    'agrement', 'taux', 'prixTotal', 'honoraires',
   #                    'unite_final', 'contenance')
   #   referentielFamille = referentielFamille[, col_a_garder]
   #   colnames(referentielFamille) = gsub('unite_final', 'unités', colnames(referentielFamille))
   #   colnames(referentielFamille) = gsub('nomcip', 'nom', colnames(referentielFamille))
   #   
   #   referentielFamille = referentielFamille[order(referentielFamille$tri),]
   #   referentielFamille$tri = NULL
   #   print(head(referentielFamille))
   #   referentielFamille
   # })
   
})

