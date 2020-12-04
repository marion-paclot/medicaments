-- Database: Medicaments
-- Création des tables
DROP TABLE IF EXISTS consommation ;
DROP TABLE IF EXISTS referentiel;

CREATE TABLE public.referentiel
(	cip13 CHARACTER(13),
	cis CHARACTER(8), 
	cip7 CHARACTER(7), 
	libelle CHARACTER VARYING,
	statut CHARACTER VARYING, 
	typeaction CHARACTER VARYING, 
	dateaction DATE, 
	agrement CHARACTER VARYING,
	taux INTEGER,
	prixhorshono DECIMAL(10,2),
	prixtotal DECIMAL(10,2),
	honoraires DECIMAL(10,2),
	infos CHARACTER VARYING,
	denomination CHARACTER VARYING,
	forme CHARACTER VARYING,
	voieadmin CHARACTER VARYING,
	statutamm CHARACTER VARYING,
	typeprocamm CHARACTER VARYING,
	etatcommercialisation CHARACTER VARYING,
	dateamm DATE,
	statutbdm CHARACTER VARYING,
	numammce CHARACTER VARYING,
	titulaire CHARACTER VARYING,
	surveillance CHARACTER VARYING,
	nomcis CHARACTER VARYING, 
	numfamille CHARACTER VARYING,
	nomfamille CHARACTER VARYING,
	typemed INTEGER, 
	tri INTEGER,
	condition CHARACTER VARYING,
	nomcip CHARACTER VARYING,
	atc CHARACTER VARYING,
	nbunite INTEGER,
	numfamilledci CHARACTER VARYING,
	acces_direct BOOLEAN ,
	mitm BOOLEAN 
	)
WITH (
    OIDS = FALSE
) ;

CREATE TABLE public.consommation
(	cip13 CHARACTER(13),
 	lieu CHARACTER VARYING,
 	mois DATE,
 	nb DECIMAL(10,2),
 	mt DECIMAL(10,2),
 	base DECIMAL(10,2)
)
WITH (
    OIDS = FALSE
) ;


-- "C:\\Program Files (x86)\\pgAdmin 4\\v4\\runtime\\psql.exe" --command " "\\copy public.consommation (cip13, lieu, mois, nb, mt, base) FROM 'C:/Users/Nous/Desktop/Marion/Etalab/Sante/app/data/IMPORT~1/CONSOM~1.CSV' DELIMITER ',' CSV HEADER QUOTE '\"' NULL 'NA' ESCAPE '''';""

-- "C:\\Program Files (x86)\\pgAdmin 4\\v4\\runtime\\psql.exe" --command " "\\copy public.referentiel (cip13, cis, cip7, libelle, statut, typeaction, dateaction, agrement, taux, prixhorshono, prixtotal, honoraires, infos, denomination, forme, voieadmin, statutamm, typeprocamm, etatcommercialisation, dateamm, statutbdm, numammce, titulaire, surveillance, nomcis, numfamille, nomfamille, typemed, tri, condition, nomcip, atc, nbunite, numfamilledci, acces_direct, mitm) FROM 'C:/Users/Nous/Desktop/Marion/Etalab/Sante/app/data/IMPORT~1/REFERE~1.CSV' DELIMITER ',' CSV HEADER QUOTE '\"' NULL 'NA' ESCAPE '''';""
