# Se mettre dans le dossier medicament
cd /

#########################################################
# Création de la base de données -- à faire une fois
sudo -u postgres psql -c "DROP DATABASE IF EXISTS medicaments;"
sudo -u postgres psql -c "CREATE DATABASE medicaments WITH ENCODING = 'UTF8';"
sudo -u postgres psql -c "ALTER DATABASE medicaments SET datestyle TO ""ISO, DMY"";"
sudo -u postgres psql -d medicaments -f "db/create_table.sql"

# Chargement des données 
sudo -u postgres psql medicaments -c "COPY consommation FROM '/srv/shiny-server/medicaments/data/importbdd/consommation.csv' 
			WITH (FORMAT CSV, DELIMITER ',', NULL 'NA', HEADER, encoding 'UTF8', ESCAPE '''') ;"
sudo -u postgres psql medicaments -c "COPY referentiel FROM '/srv/shiny-server/medicaments/data/importbdd/referentiel.csv' 
			WITH (FORMAT CSV, DELIMITER ',', NULL 'NA', HEADER, encoding 'UTF8', ESCAPE '''') ;"