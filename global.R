# Application de visualisation de la consommation de médicaments

library(shiny)
library(grid)
library(gridExtra)
library(plyr)
library(Cairo)
library(plotly)
library(ggplot2)
library(scales)
library(data.table)

library(pool)
library(RPostgreSQL)
library(glue)
library(askpass)

options(digits = 4, scipen=999)
options(encoding = 'utf-8')

### Fonction
set_utf8 <- function(x) {
   chr = sapply(x, is.character)
   x[, chr] = lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
   Encoding(names(x)) = "UTF-8"
   x
}


#### Connexion à postgres
configuration = readLines("config.txt")
con <- dbPool(
   drv = dbDriver("PostgreSQL", max.con = 100),
   dbname = "medicaments",
   host = "localhost",
   user = configuration[1],
   password = configuration[2],
   idleTimeout = 900 # 15 minutes
)
rm(configuration)
#lapply(dbListConnections(PostgreSQL()), dbDisconnect)


# Choix du menu déroulant de la spécialité
tousCIP = dbGetQuery(con, 
                     "SELECT DISTINCT denomination from referentiel
                     ORDER BY denomination ;")
tousCIP = set_utf8(tousCIP)
tousCIP = tousCIP$denomination
tousCIP = tousCIP[!is.na(tousCIP)]

denominationProduit <<- tousCIP[1]


# # 
# # ## Chargement des données de brevets
# amm = read.csv2('./data/brevet/AMM_INPI.csv', stringsAsFactors = F)
# brevets = read.csv2('./data/brevet/BREVETS_INPI.csv', stringsAsFactors = F)
# ccp = read.csv2('./data/brevet/CCP_INPI.csv', stringsAsFactors = F)
# ccp = ccp[ccp$publi_nature == "CCP Médicament",]
# 
# 
# datesEvenement = read.csv2('./data/brevet/DATES_INPI.csv', stringsAsFactors = F)
# matching = read.csv2('./data/brevet/MATCHING.csv', stringsAsFactors = F)
# matching$Code.CIS = as.character(matching$Code.CIS)
# 
# compo = read.csv2('./data/classification/CIS_COMPO_bdpm.txt', 
#                   sep = "\t", header = FALSE, stringsAsFactors = F)
# compo$V1 = as.character(compo$V1)



# 
# ### 
# Base médicament  : Gilenya - CIS : 60728597
# compo : FINGOLIMOD ou CHLORHYDRATE DE FINGOLIMOD
# ccp : 
# 
#    
# # 
# # matchingCIS = subset(matching, Code.CIS == "67520378")
# # autorisation = unique(matchingCIS$Numéro.Autorisation)
# # fichiersCCP = unique(matchingCIS$fichier)
# # datesCIS = subset(datesEvenement, fichier %in% fichiersCCP)
# # ccpCIS = subset(ccp, fichier %in% fichiersCCP)
