# Impact de la forêt sur la productivité agricole

Ce dépôt contient le code associé à un projet de recherche en économie, sociologie et sciences des données portant sur le lien entre l’occupation forestière des sols et la productivité agricole communale en France.

L’objectif est d’étudier si les communes dont la part de forêt ou de lisière agriculture-forêt évolue connaissent des trajectoires différentes de productivité agricole.

Le projet combine des données agricoles issues d’Agreste, des données d’occupation des sols issues de Corine Land Cover, ainsi que des contours communaux permettant de construire un panel communal à trois périodes.

## Question de recherche

Le projet cherche à répondre à la question suivante :

> Les communes dont la part de forêt ou de lisière agriculture-forêt évolue connaissent-elles une trajectoire différente de productivité agricole ?

Deux variables principales d’intérêt sont étudiées :

- la part de forêt dans la surface communale ;
- la part de lisière agriculture-forêt, définie comme la part de pixels agricoles adjacents à des pixels forestiers ou semi-naturels.

La productivité agricole est mesurée à partir des données Agreste, en rapportant une mesure de production agricole à la surface agricole.

## Périodes étudiées

L’analyse repose sur un panel communal à trois périodes :

| Période | Données agricoles | Données d’occupation des sols |
|---|---|---|
| 1 | Agreste 1988 | Corine Land Cover 1990 |
| 2 | Agreste 2000 | Corine Land Cover 2000 |
| 3 | Agreste 2010 | Corine Land Cover 2012 |

Ces appariements temporels permettent de rapprocher les recensements agricoles des millésimes disponibles de Corine Land Cover.

## Données

Les données brutes ne sont pas versionnées dans le dépôt GitHub en raison de leur taille.

Les principales sources mobilisées sont :

- les données agricoles Agreste ;
- les données d’occupation des sols Corine Land Cover ;
- les contours communaux IGN.

Les fichiers lourds doivent être placés dans le dossier suivant :

data/raw/

Le script suivant permet de télécharger ou de préparer une partie des fichiers nécessaires lorsque cela est possible :

source("1_data_preparation/00_data_telechargement.R")

## Structure du dépôt

Le dépôt est structuré de la manière suivante :

```text

├── 1_data_preparation/
│   ├── 00_data_telechargement.R
│   ├── 01_agri_productivite.R
│   ├── 02_indicateurs_foret.R
│   ├── 03_base_twfe.R
│   └── 04_superficies_cultures.R
│
├── 2_statistiques_descriptives/
│   ├── 01_typologie_lca_cultures.R
│   ├── 02_ajouter_typologie_agricole.R
│   ├── 03_figures_descriptives.R
│   ├── 04_table_descriptive_rapport.R
│   └── 05_carte_lca_communes.R
│
├── 3_estimations/
│   ├── 00_seuil_switchers.R
│   ├── 01_twfe_benchmark.R
│   ├── 02_as_foret.R
│   ├── 03_as_lisiere.R
│   ├── 04_analyse_heterogeneite.R
│   ├── 05_robustesse_sens_variation.R
│   └── 06_robustesse_seuil_switchers.R
│
├── R/
│   ├── packages.R
│   ├── paths.R
│   └── utils.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── output/
│   ├── figures/
│   └── tables/
│
├── main.R
└── README.md
```

## Reproduire le pipeline

Le fichier principal est :

main.R

Il lance l’ensemble des étapes principales du projet :

- chargement des packages, chemins et fonctions utilitaires ;
- préparation des données agricoles ;
- construction des indicateurs de forêt et de lisière à partir des données Corine Land Cover ;
- construction de la base de panel communale ;
- construction des variables de superficies de cultures ;
- construction de la typologie agricole par LCA ;
- production des figures et statistiques descriptives ;
- estimation des modèles TWFE ;
- estimation des effets associés à l’évolution de la forêt ;
- estimation des effets associés à l’évolution de la lisière ;
- analyse d’hétérogénéité selon la typologie agricole ;
- vérifications de robustesse.

Pour lancer le pipeline complet :

source("main.R")

Attention : certaines étapes peuvent être longues, en particulier :

- l’extraction des indicateurs d’occupation des sols par commune ;
- la construction des variables de lisière agriculture-forêt ;
- la construction de la typologie agricole par LCA.
- Bases intermédiaires et finales

Les scripts de préparation construisent notamment les bases suivantes dans data/processed/ :

- data/processed/twfe_data.parquet
- data/processed/twfe_data_enrichie.parquet

La base twfe_data.parquet correspond à la base de panel principale.

La base twfe_data_enrichie.parquet correspond à la base de panel enrichie avec la typologie agricole issue de la LCA.

## Méthodes empiriques

Le projet mobilise plusieurs approches complémentaires.

### Statistiques descriptives

Les scripts de statistiques descriptives produisent :

- des distributions des variables de forêt, de lisière et de productivité ;
- des comparaisons entre types de communes ;
- une table descriptive utilisée dans le rapport ;
- une carte de la typologie agricole communale.

### Modèles TWFE

Un premier ensemble d’estimations repose sur des modèles à effets fixes commune et période.

Ces modèles servent de benchmark descriptif pour étudier l’association entre les variables d’occupation des sols et la productivité agricole.

### Estimateurs de type AS

Le projet estime ensuite séparément l’association entre l’évolution de la productivité agricole et :

- l’évolution de la part de forêt ;
- l’évolution de la part de lisière agriculture-forêt.

Ces estimations distinguent les communes selon leur statut de variation de traitement, notamment à l’aide de seuils définissant les communes dites “switchers”.

### Analyses d’hétérogénéité

Une analyse d’hétérogénéité est menée selon la typologie agricole communale obtenue par LCA.

L’objectif est d’étudier si l’association entre forêt, lisière et productivité varie selon les structures agricoles locales.

### Robustesses

Les scripts de robustesse étudient notamment :

- la sensibilité au sens de variation du traitement ;
- la sensibilité au seuil utilisé pour définir les communes switchers.

## Sorties produites

Les sorties principales sont stockées dans :

- output/tables/
- output/figures/

Les principaux fichiers de sortie incluent notamment :

- output/tables/resultats_twfe.txt
- output/tables/coefficients_twfe.csv
- output/tables/as_foret_resultats.csv
- output/tables/as_lisiere_resultats.csv
- output/tables/table_descriptive_rapport.csv

D’autres sorties concernent :

- les analyses d’hétérogénéité par typologie agricole ;
- les tests placebo ;
- les analyses de robustesse ;
- les figures descriptives ;
- les cartes.

Les packages nécessaires sont listés dans :

R/packages.R

Les chemins du projet sont définis dans :

R/paths.R

Les fonctions utilitaires sont regroupées dans :

R/utils.R

Si certains packages sont absents de l’environnement local, le script R/packages.R peut les installer automatiquement depuis CRAN.

## Remarques sur la reproductibilité

Pour reproduire les résultats, il est recommandé de :

- cloner le dépôt ;
- se placer à la racine du projet ;
- lancer : source("main.R")

Les sorties déjà générées sont conservées dans output/ afin de faciliter la lecture du projet sans relancer immédiatement les étapes les plus coûteuses.