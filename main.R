# -----------------------------------------------------------------------------
# main.R
# Pipeline court de reproduction des résultats
# -----------------------------------------------------------------------------
# Ce fichier lance uniquement les scripts rapides à partir des bases déjà
# construites dans data/processed/.
#
# Il ne lance pas :
#   - les scripts de préparation des données ;
#   - les scripts de construction de la typologie LCA.
#
# Bases attendues :
#   data/processed/twfe_data.parquet
#   data/processed/twfe_data_enrichie.parquet
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

# Préparer les données pour les estimations
source("1_data_preparation/00_data_telechargement.R")
source("1_data_preparation/01_agri_productivite.R")
source("1_data_preparation/02_indicateurs_foret.R")
source("1_data_preparation/03_base_twfe.R")
source("1_data_preparation/04_superficies_cultures.R")

# Statistiques descriptives
source("2_statistiques_descriptives/01_typologie_lca_cultures.R")
source("2_statistiques_descriptives/02_ajouter_typologie_agricole.R")
source("2_statistiques_descriptives/03_figures_descriptives.R")
source("2_statistiques_descriptives/04_table_descriptive_rapport.R")
source("2_statistiques_descriptives/05_carte_lca_communes.R")

# Estimations
source("3_estimations/01_twfe.R")
source("3_estimations/02_as_foret.R")
source("3_estimations/03_as_lisiere.R")

# Hétérogénéité LCA
source("3_estimations/04_analyse_heterogeneite.R")

# Robustesse
source("3_estimations/05_verifications_robustesse.R")