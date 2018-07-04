# Chargement des données

library(shiny)
library(grid)
library(gridExtra)
library(plyr)
library(Cairo)
library(plotly)
library(ggplot2)
library(scales)
library(data.table)


options(digits = 4, scipen=999)

bdd = read.csv2('bdd.csv', stringsAsFactors = F, encoding = 'utf-8')
conso = read.csv2('conso.csv', stringsAsFactors = F, encoding = 'utf-8')
nom_famille = read.csv2('nom_famille.csv', stringsAsFactors = F, encoding = 'utf-8')
cout_princeps = read.csv2('cout_princeps.csv', stringsAsFactors = F, encoding = 'utf-8')

# Ne proposer que les médicaments consommés au moins une fois sur la période
bdd = bdd[bdd$CIP13 %in% conso$CIP13,]
