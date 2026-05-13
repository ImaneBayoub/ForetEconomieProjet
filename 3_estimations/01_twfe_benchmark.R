# -----------------------------------------------------------------------------
# 01_twfe_benchmark.R
# Estimations TWFE à partir de la base d'analyse unique
# -----------------------------------------------------------------------------
# Estimations principales :
#   log(productivité agricole) ~ part de forêt | effets fixes commune + période
#   log(productivité agricole) ~ part de lisière | effets fixes commune + période
#
# Hétérogénéité :
#   interactions avec la typologie agricole LCA
#
# Entrées :
#   data/processed/twfe_data_enrichie.parquet
#
# Sorties :
#   output/tables/twfe_resultats.txt
#   output/tables/twfe_coefficients.csv
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimations TWFE : forêt et lisière")

# -----------------------------------------------------------------------------
# 1. Charger la base unique
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  fichier <- path("data", "processed", "twfe_data.parquet")
}

if (!file.exists(fichier)) {
  stop(
    "Base TWFE introuvable. Lancez d'abord les scripts de préparation des données.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

# -----------------------------------------------------------------------------
# 2. Vérifications et préparation
# -----------------------------------------------------------------------------

check_required_cols(
  base,
  c(
    "id",
    "periode",
    "productivite",
    "pct_foret",
    "pct_lisiere",
    "pct_agri"
  ),
  "twfe_data"
)

base <- base %>%
  dplyr::mutate(
    id = as.factor(id),
    periode = as.factor(periode),
    type_lca = if ("type_lca" %in% names(.)) {
      as.factor(type_lca)
    } else {
      factor(NA_character_)
    },
    Y = safe_log(productivite)
  ) %>%
  dplyr::filter(
    !is.na(Y),
    !is.na(pct_foret),
    !is.na(pct_lisiere),
    !is.na(pct_agri)
  )

# -----------------------------------------------------------------------------
# 3. Estimations principales
# -----------------------------------------------------------------------------

modele_foret <- fixest::feols(
  Y ~ pct_foret | id + periode,
  data = base,
  cluster = ~ id
)

modele_lisiere <- fixest::feols(
  Y ~ pct_lisiere | id + periode,
  data = base,
  cluster = ~ id
)

modele_foret_lisiere <- fixest::feols(
  Y ~ pct_foret + pct_lisiere + pct_agri | id + periode,
  data = base,
  cluster = ~ id
)

modeles <- list(
  "Forêt" = modele_foret,
  "Lisière" = modele_lisiere,
  "Forêt + lisière + part agricole" = modele_foret_lisiere
)

# -----------------------------------------------------------------------------
# 4. Hétérogénéité selon la typologie agricole LCA
# -----------------------------------------------------------------------------

if ("type_lca" %in% names(base) &&
    dplyr::n_distinct(stats::na.omit(base$type_lca)) >= 2) {
  
  base_lca <- base %>%
    dplyr::filter(!is.na(type_lca))
  
  modele_lisiere_lca <- fixest::feols(
    Y ~ pct_lisiere:type_lca + pct_foret | id + periode,
    data = base_lca,
    cluster = ~ id
  )
  
  modele_foret_lca <- fixest::feols(
    Y ~ pct_foret:type_lca + pct_lisiere | id + periode,
    data = base_lca,
    cluster = ~ id
  )
  
  modeles[["Lisière x typologie agricole"]] <- modele_lisiere_lca
  modeles[["Forêt x typologie agricole"]] <- modele_foret_lca
}

# -----------------------------------------------------------------------------
# 5. Export des résultats au format texte
# -----------------------------------------------------------------------------

table_twfe <- capture.output(
  fixest::etable(
    modeles,
    se.below = TRUE,
    fitstat = ~ n + r2
  )
)

writeLines(
  table_twfe,
  path("output", "tables", "twfe_resultats.txt")
)

# -----------------------------------------------------------------------------
# 6. Export des coefficients au format exploitable
# -----------------------------------------------------------------------------

coefficients_twfe <- purrr::imap_dfr(
  modeles,
  ~ broom::tidy(.x, conf.int = TRUE) %>%
    dplyr::mutate(modele = .y)
)

write_csv2(
  coefficients_twfe,
  path("output", "tables", "twfe_coefficients.csv")
)

message("Résultats TWFE écrits dans output/tables/twfe_resultats.txt")
message("Coefficients TWFE écrits dans output/tables/twfe_coefficients.csv")
