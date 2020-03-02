options(java.parameters = "-Xmx4g")

library(XLConnect)
library(ggplot2)
library(plyr)
library(tidyr)
library(reshape)
library(data.table)
library(foreign)
library(dplyr)


options(shiny.usecairo=T)
options(digits = 4, scipen=999)


# # Pour la rétrocession : 
# # 
# retrocession = read.dbf("./data/classification/ucd_total_00479_20191024.dbf", 
#                         as.is = TRUE)
# 
# # Fichiers concernés
# fichiersConsommationRetro = list.files("./data/retrocedAM/", full.names = TRUE,pattern = )
# consommationRetro = list()
# nomsRetro = list()
# for (fichier in fichiersConsommationRetro) {
#   print(fichier)
#   classeur = loadWorkbook(fichier, create = F)
#   donnees = readWorksheet(classeur, sheet = 2)
#   donnees$lieu = "Rétrocession"
#   
#   colnames(donnees) = gsub("^Nom$", "NOM_UCD", colnames(donnees))
#   colnames(donnees) = gsub("Code.UCD", "CODE_UCD", colnames(donnees))
#   
#   donnees$NOM_UCD = toupper(donnees$NOM_UCD)
#   
#   colnames(donnees) = gsub("\\.", "-", colnames(donnees))
#   colnames(donnees) = gsub("Nombre.*-([0-9]*)", "nb_\\1", colnames(donnees))
#   colnames(donnees) = gsub("Montant.*-([0-9]*)", "mt_\\1", colnames(donnees))
#   colnames(donnees) = gsub("Base.*-([0-9]*)", "base_\\1", colnames(donnees))
#   colnames(donnees) = gsub("dont-Marge.*-([0-9]*)", "marge_\\1", colnames(donnees))
#   
#   colonnes_an = colnames(donnees)[grepl("nb_|mt_|base_|marge_", colnames(donnees))]
#   consommationRetro[[length(consommationRetro)+1]] = donnees[, c("CODE_UCD", "NOM_UCD", 
#                                                                  "lieu", colonnes_an)]
#   nomsRetro[[length(nomsRetro)+1]] = unique(donnees[, c('CODE_UCD', 'NOM_UCD')])
#   donnees = NULL
# }

################################################################################
############## consommation
# Boucle sur les MedicAM mensuels
fichiersConsommation = list.files("./data/medicAM/", full.names = TRUE, 
                                  pattern = 'mensuel')
fichiersConsommation = fichiersConsommation[order(fichiersConsommation)]

consommation = list()
noms_atc = list()

for (fichier in fichiersConsommation) {
  print(fichier)
  classeur = loadWorkbook(fichier, create = F)

  feuilles = getSheets(classeur)
  feuilleHopital = readWorksheet(classeur, sheet = which(grepl('hopital', feuilles)))
  feuilleHopital$lieu = "Hopital"
  feuilleVille = readWorksheet(classeur, sheet = which(grepl('ville', feuilles)))
  feuilleVille$lieu = "Ville"
  
  donnees = rbind.data.frame(feuilleHopital, feuilleVille)
  
  colnames(donnees) = gsub("NOM.COURT", "NOM_CIP13", colnames(donnees))
  if (!'Code.ATC.2' %in% colnames(donnees)){
     colnames(donnees) = gsub('Code.1', 'ATC', colnames(donnees))
  }
  colnames(donnees) = gsub("Code.ATC.2", "ATC", colnames(donnees))
  
  colnames(donnees) = gsub("\\.", "-", colnames(donnees))
  colnames(donnees) = gsub("(Nombre.*--)(.*)", "nb_\\2-01", colnames(donnees))
  colnames(donnees) = gsub("(Montant-remboursé--)(.*)", "mt_\\2-01", colnames(donnees))
  colnames(donnees) = gsub("(Base-de-remboursement-)(.*)", "base_\\2-01", colnames(donnees))
  donnees$NOM_CIP13 = toupper(donnees$NOM_CIP13)
  donnees = subset(donnees, CIP13 != '3,40095E+12')
  
  colonnes_mens = colnames(donnees)[grepl("nb_|mt_|base_", colnames(donnees))]
  
  consommation[[length(consommation)+1]] = donnees[, c("CIP13", "lieu", colonnes_mens)]
  noms_atc[[length(noms_atc)+1]] = unique(donnees[, c('CIP13', 'NOM_CIP13', 'ATC')])
  
  donnees = NULL
}

# Consommation par CIP13
consommation = Reduce(function(x,y) {merge(x,y, all = TRUE)}, consommation)
consommation = subset(consommation, !is.na(as.numeric(CIP13))) # Ne garder que les lignes de médicaments

col_mt = colnames(consommation)[grepl('mt', colnames(consommation))]
col_nb = colnames(consommation)[grepl('nb', colnames(consommation))]
col_base = colnames(consommation)[grepl('base', colnames(consommation))]
mois = unique(gsub('.*_(.*)', '\\1', colnames(consommation)))
mois = mois[! mois %in% c('CIP13', 'lieu')]

consommation = reshape(consommation, idvar = c('CIP13', 'lieu'), 
                        v.names = c('nb', 'mt', 'base'),
                       varying = list(col_nb, col_mt, col_base),
                       timevar = 'mois', times = mois,
                       new.row.names = NULL , direction = "long")
row.names(consommation) = NULL


# Nom des produits et code ATC. On garde le dernier libellé -------------
noms_atc = Reduce(rbind.data.frame, noms_atc)
noms_atc = data.table(noms_atc)
noms_atc = noms_atc[,.(NOM_CIP13 = NOM_CIP13[.N], ATC = ATC[.N]), by = CIP13]
noms_atc = data.frame(noms_atc)
noms_atc = noms_atc[!is.na(as.numeric(noms_atc$CIP13)),]
colnames(noms_atc) = c('CIP13', 'nomCIP', 'ATC')


################################################################################
############ EQUIVALENCES
colEquivalence = c("CIS", "CIP7", "libelle", "statut", "typeAction", 
                   "dateAction", "CIP13", "agrement", "taux", 
                   "prixHorsHono", "prixTotal", "honoraires", "infos")
equivalence = read.table("./data/classification/CIS_CIP_bdpm.txt", header = F,
                         sep = "\t", quote = "", fill = F, col.names = colEquivalence)

# Nettoyage des colonnes chiffres
for (col in c("prixHorsHono", "prixTotal", "honoraires")){
  equivalence[, col] = gsub("(.*),(.*),(.*)", "\\1\\2,\\3", equivalence[, col])
  equivalence[, col] = as.numeric(gsub('\\,', '\\.', equivalence[, col]))
}
equivalence$dateAction = as.Date(equivalence$dateAction, format = "%d/%m/%Y")
equivalence$taux = as.numeric(sub("%", "", equivalence$taux))
head(equivalence, 3)


################################################################################
############ FAMILLES DE GENERIQUE
generiques = read.table("./data/classification/CIS_GENER_bdpm.txt", 
                      header = F, sep = "\t", quote = "", fill = TRUE,
                      col.names = c("numFamille", "nomFamille", "CIS", "typeMed", "tri", "v"),
                      stringsAsFactors = F, colClasses = "character")
generiques$v = NULL 
generiques$nomFamille = gsub('équivalant à', '-', generiques$nomFamille)
generiques$nomFamille = gsub('\\s+', ' ', generiques$nomFamille)
generiques$nomFamille = gsub('([A-z]),([A-z ])', '\\1\n\\2', generiques$nomFamille)
generiques$nomFamille = gsub('\n ', '\n', generiques$nomFamille)
generiques$nomFamille = gsub('\\.$', '', generiques$nomFamille)


################################################################################
############ CIS
cis_bdmp = read.table("./data/classification/CIS_bdpm.txt", header = F, sep = "\t",
                      fill = T, quote = "",
                      col.names = c("CIS", "denomination", "forme", 
                                    "voieAdmin", "statutAMM", "typeProcAMM",
                                    "EtatCommercialisation", "dateAMM",
                                    "statutBdm", "numAMMCE", "titulaire", 
                                    "surveillance"),
                      stringsAsFactors = F, colClasses = "character")
cis_bdmp$dateAMM = as.character(as.Date(cis_bdmp$dateAMM, format = "%d/%m/%Y"))
cis_bdmp$nomCIS = gsub("(.*),(.*)", "\\1", cis_bdmp$denomination)


### Jointures
referentiel = merge(equivalence, cis_bdmp, by = "CIS", all.x = TRUE)
referentiel = merge(referentiel, generiques, by = "CIS", all.x = TRUE)
referentiel = merge(referentiel, noms_atc, by = "CIP13", all = FALSE)
referentiel$CIS = as.character(referentiel$CIS)
referentiel$CIP13 = as.character(referentiel$CIP13)

# Ajout des unites
referentiel$nbUnite = NA

extraireUnite = function(str, pos){
  ifelse(grepl(str, referentiel$nomCIP) & is.na(referentiel$nbUnite), 
         gsub(str, pos, referentiel$nomCIP), referentiel$nbUnite)
}
nbUniteLibelle = function(str, pos){
  ifelse(grepl(str, referentiel$libelle) & is.na(referentiel$nbUnite), 
         gsub(str, pos, referentiel$libelle), referentiel$nbUnite)
}

calculerUnite = function(str){
  c1 = ifelse(grepl(str, referentiel$libelle) & is.na(referentiel$nbUnite), 
              as.numeric(gsub(str, '\\1', referentiel$libelle)),
              1)
  c2 = ifelse(grepl(str, referentiel$libelle)& is.na(referentiel$nbUnite),
              as.numeric(gsub(str, '\\2', referentiel$libelle)),
              referentiel$nbUnite)
  c3 = ifelse(grepl(str, referentiel$libelle)& is.na(referentiel$nbUnite),
              c1*c2,
              referentiel$nbUnite)
  return(c3)
}

referentiel$nbUnite = extraireUnite('.* ([0-9]+)$', '\\1')
referentiel$nbUnite = extraireUnite('.* ([0-9]+/[0-9,]+)( |)(ML|G|MG|L|DOSES)$', '\\1')
referentiel$nbUnite = extraireUnite('.*BOITE DE ([0-9]+).*', '\\1')
referentiel$nbUnite = extraireUnite('.* ([0-9]+) (GELULE|DOSE|CPR|SACHET|COMPRIME|SERINGUE|CAPSULE|UNIDOSE|FLACON|GOMMES SS SUCRE|TUBE|STYLOS PREREMPLIS|CARTOUCHE|G¿)(S|)$', 
                                  '\\1')
referentiel$nbUnite = nbUniteLibelle('^([^0-9]+|1) .* ([0-9]+) (dose|comprimé|gélule|implant).*$', '\\2')
referentiel$nbUnite = calculerUnite('^([0-9]+) .* de ([0-9]+) (gélule|comprimé).*$')
referentiel$nbUnite = nbUniteLibelle('^([0-9]+) .*$', '\\1')
referentiel$nbUnite[is.na(referentiel$nbUnite)] = 1
referentiel$nbUnite = gsub('([0-9]+)/.*', '\\1', referentiel$nbUnite)


# Ajout d'un numero de "famille", type et tri pour les médicaments hors famille
referentiel$numFamille = ifelse(is.na(referentiel$numFamille), paste0('HF_', referentiel$CIS),
                              referentiel$numFamille)
referentiel$nomFamille = ifelse(is.na(referentiel$nomFamille), referentiel$denomination,
                              referentiel$nomFamille)
referentiel$typeMed = ifelse(is.na(referentiel$typeMed), 0, referentiel$typeMed)
referentiel = data.table(referentiel)
referentiel = referentiel[, tri_new := seq_len(.N), by = CIS]
referentiel$tri = ifelse(is.na(referentiel$tri), referentiel$tri_new, referentiel$tri)
referentiel$tri_new = NULL
referentiel = data.frame(referentiel)

# referentiel = merge(referentiel, cis_cpd, all.x = TRUE)
referentiel[,] <- lapply(referentiel, function(x) type.convert(as.character(x), as.is = TRUE))



################################################################################
############ COMPOSITION -- chargement a posteriori

# Travail sur les familles de substituts (DCI + forme commune)
referentiel$numfamilledci = NA


composition = read.table("./data/classification/CIS_COMPO_bdpm.txt", header = F, sep = "\t",
                         fill = T, quote = "",
                         col.names = c("CIS", "elemPharma", "codeSubstance",
                                       "denominationSubstance", "dosage", "refDosage",
                                       "natureComposant", "numLien", "v"),
                         stringsAsFactors = F, colClasses = "character")
composition$v = NULL
composition = subset(composition, CIS %in% referentiel$CIS)

# Reformatage de composition
composition = composition[order(composition$codeSubstance),]
composition$dosage = gsub('million(s|).*(UI|unités internat).*', 'MUI', composition$dosage, ignore.case = T)
composition$dosage = gsub('unités$|U.l.|U\\.I\\.|Ul$', 'UI', composition$dosage, ignore.case = T)
composition$dosage = gsub('anti-Xa', 'AXa', composition$dosage, ignore.case = T)

composition$dosage = gsub('([0-9]+) U(\\.|).* D(\\(|)$', '\\1 UD', composition$dosage, ignore.case = T)
composition$dosage = gsub('([0-9]+) (UD|U\\.D\\.) .*$', '\\1 UD', composition$dosage, ignore.case = T)
composition$dosage = gsub('([0-9]+)\\.([0-9]+)', "\\1\\2", composition$dosage, ignore.case = T)
composition$dosage = gsub('micr.*grammes|µg', "microgrammes", composition$dosage, ignore.case = T)
composition$dosage = gsub('([0-9]+)(mg|g|microgramme)', "\\1 \\2", composition$dosage, ignore.case = T)

composition$dosage = gsub('(.*) \\(.*$', "\\1", composition$dosage, ignore.case = T)

composition$dosage = gsub('^\\+|\\s+$', '', composition$dosage)
composition$dosage = gsub('\\s+', ' ', composition$dosage)

composition$dosage = gsub('([0-9]+) ([0-9]+)', '\\1\\2', composition$dosage )

convertir = function(chaine, operation, facteur){
   valeur = gsub('([0-9, ]+) (.*)', '\\1', chaine)
   valeur = gsub(',', '\\.', valeur)
   if (operation == "diviser"){
      valeur = as.numeric(valeur)/facteur
   }
   if (operation == "multiplier"){
      valeur = as.numeric(valeur)*facteur
   }
   valeur = gsub('\\.', ',', valeur)
   return(valeur)
}

composition$dosage2 = composition$dosage
composition$dosage2 = ifelse(grepl('([0-9,]+) microgramme(s|| )$', composition$dosage),
                             paste(convertir(composition$dosage, 'diviser', 1000), 'mg'),
                             composition$dosage2)
composition$dosage2 = ifelse(grepl('([0-9,]+) g$', composition$dosage),
                             paste(convertir(composition$dosage, 'multiplier', 1000), 'mg'),
                             composition$dosage2)
composition$dosage2 = ifelse(grepl('([0-9,]+) MUI$', composition$dosage),
                             paste(convertir(composition$dosage, 'multiplier', 1000000), 'mg'),
                             composition$dosage2)

composition$dosage2 = gsub('\\s+', ' ', composition$dosage2)
composition$dosage2 = gsub(',[0]+ ', ' ', composition$dosage2)
composition$dosage2 = gsub(',([1-9]+)[0]+ ', ',\\1 ', composition$dosage2)
length(unique(composition$dosage2))


cis_hf = unique(referentiel$CIS[grepl('HF', referentiel$numFamille)])
composition = subset(composition, CIS %in% cis_hf)

composition_agg = composition %>% 
   group_by(CIS) %>% 
   mutate(codeSubstance = paste0(codeSubstance, collapse = "|"),
          dosage = paste0(dosage2, collapse = '|')) 

composition_agg = composition_agg[, c('CIS', 'elemPharma', 'codeSubstance', 'dosage')]
composition_agg = unique(composition_agg)

# Boucle sur les produits restants
combinaisons = data.frame(table(composition_agg$codeSubstance))
combinaisons = combinaisons[combinaisons$Freq>1,]
combinaisons = data.frame(combinaisons)

familles_galeniques = list()
for (combinaison in combinaisons$Var1){
   composition_combinaison = subset(composition_agg, codeSubstance == combinaison)
   cis_combinaison = composition_combinaison$CIS
   
   type_dosage = unique(gsub('.* (.*)$', '\\1', composition_combinaison$dosage))
   if (length(type_dosage)>1){
      print(composition_combinaison)
   }
   # for (d in composition_combinaison$dosage){
   #    candidats = subset(composition_combinaison, dosage == d)
   #    if nrow(candidats)
   #    print(candidats)
   # }
   # 
   #subset(referentiel, CIS %in% cis_combinaison)
}






################################################################################
#Sauvegarde des bases
################################################################################
dir.create('data/importbdd/')

# Point décimal et séparateur virgule
write.csv(referentiel, './data/importbdd/medicaments.csv', 
         fileEncoding = 'UTF-8', row.names = F, quote = TRUE)
write.csv(consommation, './data/importbdd/consommation.csv', 
          fileEncoding = 'UTF-8', row.names = F, quote = TRUE)

rm(list = ls()[! grepl('consommation|referentiel', ls())]) # A retirer plus tard





# cis = '66819566'
# numFamille = unique(referentiel$numFamille[referentiel$CIS == CIS])
# testCIS = unique(referentiel$CIS[referentiel$numFamille == numFamille])
# testCIS
# 
# 
# testCompo = subset(composition, CIS %in% testCIS)
# 
# referentiel$numfamilledci = NA
# CIS_potentiels = unique(referentiel$CIS[grepl('HF_', referentiel$numFamille)])
# 
# cis = '66745607'
# compoCIS = subset(composition, CIS %in% CIS_potentiels)
# forme = unique(compoCIS$elemPharma)
# codes_substances = unique(compoCIS$codeSubstance)
# # Candidats = 
# candidats = unique(referentiel$CIS[grepl('HF_', referentiel$numFamille)])
# produits_restants = composition
# for (code in codes_substances){
#    cis_restants = subset()
# }
# 
# 
# 
# for (i in 1:nrow(referentiel)){
#    CIS = 
#    if (grepl('HF_', referentiel$numFamille[i])){
#       referentiel$numfamilledci[i] = 1
#       print(i)
#    }
#        
# }
