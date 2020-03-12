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

library(XML)
library(RCurl)

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

ccp = read.csv('./data/importbdd/ccp.csv', stringsAsFactors = F)
 

# Texte de la faq
faq = readLines('faq.html')
