options(java.parameters = "-Xmx4g")

library(XLConnect)
library(ggplot2)
library(plyr)
library(tidyr)
library(reshape)
library(data.table)
library(foreign)

options(shiny.usecairo=T)
options(digits = 4, scipen=999)

# Chargement depuis http://base-donnees-publique.medicaments.gouv.fr/telechargement.php
# consommation de médicaments : https://www.ameli.fr/l-assurance-maladie/statistiques-et-publications/donnees-statistiques/medicament/index.php
# Documentation http://base-donnees-publique.medicaments.gouv.fr/docs/Contenu_et_format_des_fichiers_telechargeables_dans_la_BDM_v1.pdf

# Pour la rétrocession : 
# 
retrocession = read.dbf("./data/classification/ucd_total_00479_20191024.dbf", 
                        as.is = TRUE)

# Fichiers concernés
fichiersConsommationRetro = list.files("./data/retrocedAM/", full.names = TRUE,pattern = )
consommationRetro = list()
nomsRetro = list()
for (fichier in fichiersConsommationRetro) {
  print(fichier)
  classeur = loadWorkbook(fichier, create = F)
  donnees = readWorksheet(classeur, sheet = 2)
  donnees$lieu = "Rétrocession"
  
  colnames(donnees) = gsub("^Nom$", "NOM_UCD", colnames(donnees))
  colnames(donnees) = gsub("Code.UCD", "CODE_UCD", colnames(donnees))
  
  donnees$NOM_UCD = toupper(donnees$NOM_UCD)
  
  colnames(donnees) = gsub("\\.", "-", colnames(donnees))
  colnames(donnees) = gsub("Nombre.*-([0-9]*)", "nb_\\1", colnames(donnees))
  colnames(donnees) = gsub("Montant.*-([0-9]*)", "mt_\\1", colnames(donnees))
  colnames(donnees) = gsub("Base.*-([0-9]*)", "base_\\1", colnames(donnees))
  colnames(donnees) = gsub("dont-Marge.*-([0-9]*)", "marge_\\1", colnames(donnees))
  
  colonnes_an = colnames(donnees)[grepl("nb_|mt_|base_|marge_", colnames(donnees))]
  consommationRetro[[length(consommationRetro)+1]] = donnees[, c("CODE_UCD", "NOM_UCD", 
                                                                 "lieu", colonnes_an)]
  nomsRetro[[length(nomsRetro)+1]] = unique(donnees[, c('CODE_UCD', 'NOM_UCD')])
  donnees = NULL
}

################################################################################
############## consommation
# Boucle sur les MedicAM mensuels

# Fichiers concernés
fichiersConsommation = list.files("./data/medicAM/", full.names = TRUE,pattern = )
fichiersConsommation = fichiersConsommation[grepl("mensuel", fichiersConsommation)]
fichiersConsommation = fichiersConsommation[order(fichiersConsommation)]

consommation = list()
noms = list()

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
  colnames(donnees) = gsub("\\.", "-", colnames(donnees))
  colnames(donnees) = gsub("(Nombre.*--)(.*)", "nb_\\2-01", colnames(donnees))
  colnames(donnees) = gsub("(Montant-remboursé--)(.*)", "mt_\\2-01", colnames(donnees))
  colnames(donnees) = gsub("(Base-de-remboursement-)(.*)", "base_\\2-01", colnames(donnees))
  donnees$NOM_CIP13 = toupper(donnees$NOM_CIP13)
  donnees = subset(donnees, CIP13 != '3,40095E+12')
  
  colonnes_mens = colnames(donnees)[grepl("nb_|mt_|base_", colnames(donnees))]
  
  consommation[[length(consommation)+1]] = donnees[, c("CIP13", "lieu", colonnes_mens)]
  noms[[length(noms)+1]] = unique(donnees[, c('CIP13', 'NOM_CIP13')])
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

# Nom des produits. On garde le dernier libellé -------------
noms = Reduce(rbind.data.frame, noms)
noms = data.table(noms)
noms = noms[,.(NOM_CIP13 = NOM_CIP13[.N]), by = CIP13]
noms = data.frame(noms)
noms = noms[!is.na(as.numeric(noms$CIP13)),]
colnames(noms) = c('CIP13', 'nomCIP')


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
############ COMPOSITION
composition = read.table("./data/classification/CIS_COMPO_bdpm.txt", header = F, sep = "\t",
                   fill = T, quote = "",
                   col.names = c("CIS", "elemPharma", "codeSubstance",
                                 "denominationSubstance", "dosage", "refDosage",
                                 "natureComposant", "numLien", "v"),
                   stringsAsFactors = F, colClasses = "character")
composition$v = NULL
composition = subset(composition, !grepl('HOMÉOPATHIQUE', denominationSubstance))
composition = subset(composition, CIS %in% baseMedic$CIS)
# composition = unique(composition[, c('CIS', 'elemPharma', 'refDosage')])

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

######################
# Ajout des unités. Attention, la base contenant les unités n'est pas mise à jour
# de manière régulière pour le moment.

unites = read.csv2('data/IR_PHA_R.csv', stringsAsFactors = FALSE)
unites = unites[, c('PHA_CIP_C13', 'PHA_UPC_NBR')]
colnames(unites) = c('CIP13', 'nbUnite')



### Jointures
baseMedic = merge(equivalence, cis_bdmp, by = "CIS", all.x = TRUE)
baseMedic = merge(baseMedic, generiques, by = "CIS", all.x = TRUE)
baseMedic = merge(baseMedic, noms, by = "CIP13", all = FALSE)
baseMedic$CIS = as.character(baseMedic$CIS)
baseMedic$CIP13 = as.character(baseMedic$CIP13)

# Ajout des unites
baseMedic$nbUnite = NA

extraireUnite = function(str, pos){
  ifelse(grepl(str, baseMedic$nomCIP) & is.na(baseMedic$nbUnite), 
         gsub(str, pos, baseMedic$nomCIP), baseMedic$nbUnite)
}
nbUniteLibelle = function(str, pos){
  ifelse(grepl(str, baseMedic$libelle) & is.na(baseMedic$nbUnite), 
         gsub(str, pos, baseMedic$libelle), baseMedic$nbUnite)
}

calculerUnite = function(str){
  c1 = ifelse(grepl(str, baseMedic$libelle) & is.na(baseMedic$nbUnite), 
              as.numeric(gsub(str, '\\1', baseMedic$libelle)),
              1)
  c2 = ifelse(grepl(str, baseMedic$libelle)& is.na(baseMedic$nbUnite),
              as.numeric(gsub(str, '\\2', baseMedic$libelle)),
              baseMedic$nbUnite)
  c3 = ifelse(grepl(str, baseMedic$libelle)& is.na(baseMedic$nbUnite),
              c1*c2,
              baseMedic$nbUnite)
  return(c3)
}

baseMedic$nbUnite = extraireUnite('.* ([0-9]+)$', '\\1')
baseMedic$nbUnite = extraireUnite('.* ([0-9]+/[0-9,]+)( |)(ML|G|MG|L|DOSES)$', '\\1')
baseMedic$nbUnite = extraireUnite('.*BOITE DE ([0-9]+).*', '\\1')
baseMedic$nbUnite = extraireUnite('.* ([0-9]+) (GELULE|DOSE|CPR|SACHET|COMPRIME|SERINGUE|CAPSULE|UNIDOSE|FLACON|GOMMES SS SUCRE|TUBE|STYLOS PREREMPLIS|CARTOUCHE|G¿)(S|)$', 
                                  '\\1')
baseMedic$nbUnite = nbUniteLibelle('^([^0-9]+|1) .* ([0-9]+) (dose|comprimé|gélule|implant).*$', '\\2')
baseMedic$nbUnite = calculerUnite('^([0-9]+) .* de ([0-9]+) (gélule|comprimé).*$')
baseMedic$nbUnite = nbUniteLibelle('^([0-9]+) .*$', '\\1')
baseMedic$nbUnite[is.na(baseMedic$nbUnite)] = 1
baseMedic$nbUnite = gsub('([0-9]+)/.*', '\\1', baseMedic$nbUnite)


# Ajout d'un numero de "famille", type et tri pour les médicaments hors famille
baseMedic$numFamille = ifelse(is.na(baseMedic$numFamille), paste0('HF_', baseMedic$CIS),
                              baseMedic$numFamille)
baseMedic$nomFamille = ifelse(is.na(baseMedic$nomFamille), baseMedic$denomination,
                              baseMedic$nomFamille)
baseMedic$typeMed = ifelse(is.na(baseMedic$typeMed), 0, baseMedic$typeMed)
baseMedic = data.table(baseMedic)
baseMedic = baseMedic[, tri_new := seq_len(.N), by = CIS]
baseMedic$tri = ifelse(is.na(baseMedic$tri), baseMedic$tri_new, baseMedic$tri)
baseMedic$tri_new = NULL
baseMedic = data.frame(baseMedic)

# baseMedic = merge(baseMedic, cis_cpd, all.x = TRUE)
baseMedic[,] <- lapply(baseMedic, function(x) type.convert(as.character(x), as.is = TRUE))


# Travail sur les familles de substituts (DCI + forme commune)
baseMedic$numfamilledci = NA


#Sauvegarde des bases
# Point décimal et séparateur virgule
write.csv(baseMedic, './data/apNettoyage/medicaments.csv', 
         fileEncoding = 'UTF-8', row.names = F, quote = TRUE)
write.csv(consommation, './data/apNettoyage/consommation.csv', 
          fileEncoding = 'UTF-8', row.names = F, quote = TRUE)
#rm(list = ls())





# Reformatage de composition
compo_sauv = composition
composition = composition[order(composition$codeSubstance),]
composition$dosage = gsub('^\\+|\\s+$', '', composition$dosage)
composition$dosage = gsub('\\s+', ' ', composition$dosage)

composition$dosage = gsub('([0-9]+) ([0-9]+)', '\\1\\2', composition$dosage )
facteurMille = function(chaine, operation){
   valeur = gsub('([0-9, ]+) (.*)', '\\1', chaine)
   valeur = gsub(',', '\\.', valeur)
   if (operation == "diviser"){
      valeur = as.numeric(valeur)/1000
   }
   if (operation == "multiplier"){
      valeur = as.numeric(valeur)*1000
   }
   valeur = gsub('\\.', ',', valeur)
   return(valeur)
}
grepl('([0-9,]+) microgramme(s|| )$', '84 microgrammes')

composition$dosage2 = composition$dosage
composition$dosage2 = ifelse(grepl('([0-9,]+) microgramme(s|| )$', composition$dosage),
                            paste(facteurMille(composition$dosage, 'diviser'), 'mg'),
                            composition$dosage2)
composition$dosage2 = ifelse(grepl('([0-9,]+) g$', composition$dosage),
                             paste(facteurMille(composition$dosage, 'multiplier'), 'mg'),
                             composition$dosage2)
composition$dosage2 = gsub('\\s+', ' ', composition$dosage2)
composition$dosage2 = gsub(',[0]+ ', ' ', composition$dosage2)
composition$dosage2 = gsub(',([1-9]+)[0]+ ', ',\\1 ', composition$dosage2)
unique(composition$dosage2)

library(dplyr)
composition = composition %>% 
   group_by(CIS) %>% 
   mutate(codeSubstance_agg = paste0(codeSubstance, collapse = "|"),
          dosage_agg = paste0(dosage, collapse = '|')) 

composition = unique(composition[, c('CIS', 'elemPharma', 'codeSubstance_agg', 'dosage_agg')])

test = merge(subset(composition, CIS == cis ), subset(composition, CIS != cis ),
             by = c('elemPharma', 'codeSubstance_agg'), all = FALSE )
test$dosage_agg.x = gsub('([0-9]+) microgrammes', '0,\\1 mg', test$dosage_agg.x)
test$dosage_agg.y = gsub('([0-9]+) microgrammes', '0,\\1 mg', test$dosage_agg.y)

cis = '66819566'
numFamille = unique(baseMedic$numFamille[baseMedic$CIS == CIS])
testCIS = unique(baseMedic$CIS[baseMedic$numFamille == numFamille])
testCIS


testCompo = subset(composition, CIS %in% testCIS)

baseMedic$numfamilledci = NA
CIS_potentiels = unique(baseMedic$CIS[grepl('HF_', baseMedic$numFamille)])

cis = '66745607'
compoCIS = subset(composition, CIS %in% CIS_potentiels)
forme = unique(compoCIS$elemPharma)
codes_substances = unique(compoCIS$codeSubstance)
# Candidats = 
candidats = unique(baseMedic$CIS[grepl('HF_', baseMedic$numFamille)])
produits_restants = composition
for (code in codes_substances){
   cis_restants = subset()
}



for (i in 1:nrow(baseMedic)){
   CIS = 
   if (grepl('HF_', baseMedic$numFamille[i])){
      baseMedic$numfamilledci[i] = 1
      print(i)
   }
       
}
