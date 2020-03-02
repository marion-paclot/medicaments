library(RCurl)
library(XML)

# Script permettant de télécharger et placer les fichiers de données dans 
# les bons dossiers.
# A faire tourner avant le script "traitement_donnees.R"

################################################################################
# Création des dossiers emboités, si on part de 0
################################################################################
dir.create('data')
dir.create('data/classification')
dir.create('data/medicAM')

fichiersMensuels = list.files('data/medicAM/') # Fichiers conso déjà présents

################################################################################
# Chargement des données issues de la base publique du médicament
# Mise à jour régulière de ces fichiers
################################################################################

url_referentiel = "http://base-donnees-publique.medicaments.gouv.fr/telechargement.php"
download.file(paste0(url_referentiel, '?fichier=CIS_bdpm.txt'), 'data/classification/CIS_bdpm.txt')
download.file(paste0(url_referentiel, '?fichier=CIS_CIP_bdpm.txt'),'data/classification/CIS_CIP_bdpm.txt')
download.file(paste0(url_referentiel, '?fichier=CIS_GENER_bdpm.txt'),'data/classification/CIS_GENER_bdpm.txt')
download.file(paste0(url_referentiel, '?fichier=CIS_COMPO_bdpm.txt'),'data/classification/CIS_COMPO_bdpm.txt')

################################################################################
# Chargement des données de l'assurance maladie (consommations mensuelles)
################################################################################

url_ameli = "https://www.ameli.fr"
url_conso = "/l-assurance-maladie/statistiques-et-publications/donnees-statistiques/medicament/medicaments-pharmacies-de-ville-par-classe-atc/"

url2019 = getURL(paste0(url_ameli, url_conso, 'medic-am-labellise-2019.php'))
page2019 = htmlParse(url2019)
liens  = unlist(xpathSApply(page2019, path = "//a",xmlGetAttr,"href"))
urls = paste0(url_ameli, liens[grepl(url_conso, liens)])

for (url in urls){
   pageAnnee = htmlParse(getURL(url))
   liens  = unlist(xpathSApply(pageAnnee, path = "//a",xmlGetAttr,"href"))
   fichiers = liens[grepl('Medic_AM_mensuel', liens)]
   for (fichier in fichiers){
      nomFichierZip = gsub('.*\\/(.*)', '\\1', fichier)
      nomFichier = gsub('\\.zip', '\\.xls', nomFichierZip)
      
      # Si le fichier est déjà présent dans le dossier, on passe au lien suivant
      if (nomFichier %in% fichiersMensuels){
         next()
      }
      
      # Chargement
      download.file(paste0(url_ameli, fichier), paste0('data/medicAM/', nomFichierZip))
      
      # Dézip
      unzip(paste0('data/medicAM/', nomFichierZip), overwrite = TRUE,
            junkpaths = FALSE, exdir = 'data/medicAM', unzip = "internal",
            setTimes = FALSE)
      file.remove(paste0('data/medicAM/', nomFichierZip))
   }
}

################################################################################
# Chargement de la liste des codes ATC contenant les MITM (médicaments 
# d'intérêt thérapeuthique majeur), issue d'un arrêté du JO
# Au 1er mars 2020, il s'agit de la version de 2017
################################################################################

urlJo = "https://www.legifrance.gouv.fr/affichTexte.do?cidTexte=JORFTEXT000032958454&categorieLien=id"
pageJo = htmlParse(getURL(urlJo), useInternal = TRUE)

atc = xpathApply(pageJo, "//div[@class='article']/p", function(x)
   xpathSApply(x,".//text()", xmlValue)
)

atc = unlist(atc)
atc = data.frame(nom = atc[grepl('[A-Z]{1}[0-9]*[A_Z]* - (.*)', atc)])
atc$code = gsub('([A-Z]{1}[0-9]*[A_Z]*) - (.*)', '\\1', atc$nom)
atc$nom = gsub('([A-Z]{1}[0-9]*[A_Z]*) - (.*)', '\\2', atc$nom)

write.csv2(atc, 'data/classification/liste_mitm.csv', row.names = FALSE)
