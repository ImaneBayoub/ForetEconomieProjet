# Forêt, lisière et productivité agricole

Ce dépôt contient le code associé à un projet de recherche en économie, sociologie et sciences des données portant sur le lien entre l’occupation forestière des sols et la productivité agricole communale en France.

L’objectif principal est d’étudier si l’évolution de la part de forêt et de la lisière agriculture-forêt est associée à l’évolution de la productivité agricole, mesurée à partir des données Agreste.

## Question de recherche

Le projet cherche à répondre à la question suivante :

Les communes dont la part de forêt ou de lisière agriculture-forêt évolue connaissent-elles une trajectoire différente de productivité agricole ?

Nous distinguons deux variables principales de traitement :

- la part de forêt dans la surface communale ;
- la part de lisière agriculture-forêt, définie comme la part de pixels agricoles adjacents à des pixels forestiers ou semi-naturels.

L’analyse repose sur un panel communal à trois périodes :

| Période | Données agricoles | Données d’occupation des sols |
|---|---:|---:|
| 1 | Agreste 1988 | CLC 1990 |
| 2 | Agreste 2000 | CLC 2000 |
| 3 | Agreste 2010 | CLC 2012 |

## Données

Les données brutes ne sont pas versionnées dans le dépôt GitHub en raison de leur taille.

Les principales sources utilisées sont :

- données agricoles Agreste ;
- données Corine Land Cover issues de Copernicus ;
- contours communaux IGN.

Les fichiers lourds doivent être placés dans data/raw/ ou téléchargés à l’aide du script :

source("1_data_preparation/00_data_telechargement.R")

Les scripts de préparation construisent ensuite les bases intermédiaires et finales.

## Bases attendues pour lancer le pipeline court

Le fichier main.R fourni dans ce dépôt ne relance pas les scripts longs de préparation des données, ni les scripts de LCA, car ils peuvent être coûteux en temps de calcul.

Il suppose donc que les bases suivantes existent déjà :

- data/processed/twfe_data.parquet
- data/processed/twfe_data_enrichie.parquet

La base twfe_data.parquet contient la base de panel principale.

La base twfe_data_enrichie.parquet contient la même base, enrichie avec la typologie agricole issue de la LCA.

## Reproduire les résultats à partir des bases déjà construites

Pour reproduire les tableaux et estimations à partir des bases finales déjà construites :

source("main.R")

Le script main.R lance uniquement :

- les figures descriptives ;
- la table descriptive du rapport ;
- les estimations TWFE ;
- les estimateurs AS pour la forêt ;
- les estimateurs AS pour la lisière ;
- les analyses d’hétérogénéité selon la typologie agricole ;
- les vérifications de robustesse au seuil de définition des switchers.

Il ne lance pas :

- les scripts de préparation des données ;
- les scripts de construction de la typologie LCA.

## Reproduire tout le pipeline depuis les données brutes

Pour reconstruire toutes les bases depuis les données brutes, il faut lancer manuellement les scripts suivants, dans cet ordre :

- source("1_data_preparation/00_data_telechargement.R")
- source("1_data_preparation/01_agri_productivite.R")
- source("1_data_preparation/02_indicateurs_foret.R")
- source("1_data_preparation/03_base_twfe.R")
- source("1_data_preparation/04_superficies_cultures.R")

- source("2_statistiques_descriptives/01_typologie_lca_cultures.R")
- source("2_statistiques_descriptives/02_ajouter_typologie_agricole.R")

Ces scripts peuvent être longs à exécuter, en particulier :

- l’extraction des indicateurs CLC par commune ;
- la construction de la typologie agricole par LCA.

## Sorties produites

Les sorties principales sont stockées dans :

- output/tables/
- output/figures/

Les principaux fichiers produits sont notamment :

- output/tables/resultats_twfe.txt
- output/tables/coefficients_twfe.csv
- output/tables/as_foret_resultats.csv
- output/tables/as_lisiere_resultats.csv
- output/tables/as_par_typologie_agricole.csv
- output/tables/as_sensibilite_seuil.csv
- output/tables/table_descriptive_rapport.csv

et plusieurs figures descriptives ou de robustesse dans :

- output/figures/

## Environnement R

Les packages nécessaires sont listés et chargés dans :

R/packages.R

Si certains packages sont absents, ils sont installés automatiquement depuis CRAN.