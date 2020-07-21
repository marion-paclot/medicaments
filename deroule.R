#

# 1. Chargement des données consommation + annuaire du médicament
source('chargement_donnees.R')

# 2. Traitement des données
# Deux bases créées : référentiel et consommation
source('traitement_donnees.R')

# 3. Chargement et traitement des données CCP INPI
setwd('../Brevets/')
source('main.R')
