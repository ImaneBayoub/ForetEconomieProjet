# -----------------------------------------------------------------------------
# 04_table_descriptive_rapport.R
# Produire une table synthétique de statistiques descriptives pour le rapport
# -----------------------------------------------------------------------------
# Sorties :
#   output/tables/table_descriptive_rapport.csv
#   output/tables/table_descriptive_rapport_latex.txt
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Production de la table descriptive pour le rapport")

# -----------------------------------------------------------------------------
# 1. Charger la base unique
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  fichier <- path("data", "processed", "twfe_data.parquet")
}

if (!file.exists(fichier)) {
  stop(
    "Base TWFE introuvable. Lance d'abord les scripts de préparation des données.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

# -----------------------------------------------------------------------------
# 2. Fonction de résumé
# -----------------------------------------------------------------------------

resumer_variable <- function(nom_variable, vecteur) {
  vecteur <- vecteur[!is.na(vecteur)]
  
  tibble::tibble(
    variable = nom_variable,
    moyenne = mean(vecteur),
    mediane = stats::median(vecteur),
    ecart_type = stats::sd(vecteur),
    n = length(vecteur)
  )
}

# -----------------------------------------------------------------------------
# 3. Préparer les variations entre périodes
# -----------------------------------------------------------------------------

base_large <- base %>%
  dplyr::select(
    id,
    periode,
    productivite,
    pct_foret,
    pct_lisiere
  ) %>%
  tidyr::pivot_wider(
    names_from = periode,
    values_from = c(productivite, pct_foret, pct_lisiere),
    names_glue = "{.value}_p{periode}"
  ) %>%
  dplyr::mutate(
    delta_lisiere_2000_1990 = pct_lisiere_p2 - pct_lisiere_p1,
    delta_lisiere_2012_2000 = pct_lisiere_p3 - pct_lisiere_p2,
    delta_foret_2000_1990 = pct_foret_p2 - pct_foret_p1,
    delta_foret_2012_2000 = pct_foret_p3 - pct_foret_p2,
    delta_productivite_2000_1990 = productivite_p2 - productivite_p1,
    delta_productivite_2012_2000 = productivite_p3 - productivite_p2
  )

# -----------------------------------------------------------------------------
# 4. Construire la table descriptive
# -----------------------------------------------------------------------------

table_descriptive <- dplyr::bind_rows(
  resumer_variable("Lisière, panel complet", base$pct_lisiere),
  resumer_variable("Variation lisière 2000-1990", base_large$delta_lisiere_2000_1990),
  resumer_variable("Variation lisière 2012-2000", base_large$delta_lisiere_2012_2000),
  resumer_variable("Part de forêt, panel complet", base$pct_foret),
  resumer_variable("Variation forêt 2000-1990", base_large$delta_foret_2000_1990),
  resumer_variable("Variation forêt 2012-2000", base_large$delta_foret_2012_2000),
  resumer_variable("Productivité agricole, panel complet", base$productivite),
  resumer_variable("Variation productivité 2000-1990", base_large$delta_productivite_2000_1990),
  resumer_variable("Variation productivité 2012-2000", base_large$delta_productivite_2012_2000)
) %>%
  dplyr::mutate(
    moyenne = round(moyenne, 3),
    mediane = round(mediane, 3),
    ecart_type = round(ecart_type, 3)
  )

# -----------------------------------------------------------------------------
# 5. Export CSV
# -----------------------------------------------------------------------------

write_csv2(
  table_descriptive,
  path("output", "tables", "table_descriptive_rapport.csv")
)
