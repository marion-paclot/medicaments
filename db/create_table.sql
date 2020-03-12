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