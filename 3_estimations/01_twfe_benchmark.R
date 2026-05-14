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
# Placebo / pre-trends :
#   Pour chaque spécification, on ajoute une avance (lead +1) de chaque
#   traitement continu et on teste sa nullité par un test de Wald.
#   La p-value est exportée comme ligne supplémentaire dans le tableau.
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
  c("id", "periode", "productivite", "pct_foret", "pct_lisiere", "pct_agri"),
  "twfe_data"
)

base <- base %>%
  dplyr::mutate(
    id     = as.factor(id),
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
# 3. Création des avances (leads) pour les tests de pre-trends
#    On trie par commune puis par ordre chronologique de période.
# -----------------------------------------------------------------------------

base <- base %>%
  dplyr::arrange(id, as.integer(periode)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    pct_foret_lead1   = dplyr::lead(pct_foret,   n = 1),
    pct_lisiere_lead1 = dplyr::lead(pct_lisiere, n = 1)
  ) %>%
  dplyr::ungroup()

# Helper : extrait la p-value d'un test de Wald sur un vecteur de variables
# et renvoie NA si les variables sont absentes du modèle (ex. lead non estimé).
wald_pval <- function(modele, vars) {
  vars_presentes <- intersect(vars, names(stats::coef(modele)))
  if (length(vars_presentes) == 0L) return(NA_real_)
  fixest::wald(modele, vars_presentes)[["p"]]
}

# -----------------------------------------------------------------------------
# 4. Estimations principales
# -----------------------------------------------------------------------------

modele_foret <- fixest::feols(
  Y ~ pct_foret | id + periode,
  data    = base,
  cluster = ~id
)

modele_lisiere <- fixest::feols(
  Y ~ pct_lisiere | id + periode,
  data    = base,
  cluster = ~id
)

modeles <- list(
  "Forêt"   = modele_foret,
  "Lisière" = modele_lisiere
)

# -----------------------------------------------------------------------------
# 5. Tests de pre-trends : régressions avec avance du traitement
#    Le modèle augmenté sert uniquement au test ; on n'exporte pas ses
#    coefficients dans le tableau principal.
# -----------------------------------------------------------------------------

modele_foret_aug <- fixest::feols(
  Y ~ pct_foret + pct_foret_lead1 | id + periode,
  data    = base,
  cluster = ~id
)

modele_lisiere_aug <- fixest::feols(
  Y ~ pct_lisiere + pct_lisiere_lead1 | id + periode,
  data    = base,
  cluster = ~id
)

pvals_pretrend <- list(
  "Forêt"   = wald_pval(modele_foret_aug,   "pct_foret_lead1"),
  "Lisière" = wald_pval(modele_lisiere_aug, "pct_lisiere_lead1")
)

# -----------------------------------------------------------------------------
# 6. Hétérogénéité selon la typologie agricole LCA
# -----------------------------------------------------------------------------

if ("type_lca" %in% names(base) &&
    dplyr::n_distinct(stats::na.omit(base$type_lca)) >= 2) {

  base_lca <- base %>% dplyr::filter(!is.na(type_lca))

  modele_lisiere_lca <- fixest::feols(
    Y ~ pct_lisiere:type_lca + pct_foret | id + periode,
    data    = base_lca,
    cluster = ~id
  )

  modele_foret_lca <- fixest::feols(
    Y ~ pct_foret:type_lca + pct_lisiere | id + periode,
    data    = base_lca,
    cluster = ~id
  )

  # Régressions augmentées pour les tests de pre-trends (LCA)
  modele_lisiere_lca_aug <- fixest::feols(
    Y ~ pct_lisiere:type_lca + pct_foret +
      pct_lisiere_lead1 | id + periode,
    data    = base_lca,
    cluster = ~id
  )

  modele_foret_lca_aug <- fixest::feols(
    Y ~ pct_foret:type_lca + pct_lisiere +
      pct_foret_lead1 | id + periode,
    data    = base_lca,
    cluster = ~id
  )

  modeles[["Lisière × LCA"]] <- modele_lisiere_lca
  modeles[["Forêt × LCA"]]   <- modele_foret_lca

  pvals_pretrend[["Lisière × LCA"]] <-
    wald_pval(modele_lisiere_lca_aug, "pct_lisiere_lead1")
  pvals_pretrend[["Forêt × LCA"]] <-
    wald_pval(modele_foret_lca_aug, "pct_foret_lead1")
}

# -----------------------------------------------------------------------------
# 7. Mise en forme de la p-value pre-trends pour etable()
#    extralines attend une liste nommée : clé = libellé de ligne,
#    valeur = vecteur aligné sur l'ordre des modèles.
# -----------------------------------------------------------------------------

pvals_vec <- purrr::map_dbl(
  names(modeles),
  ~ pvals_pretrend[[.x]] %||% NA_real_
)

# Formatage : trois décimales, "<0.001" pour les très petites valeurs
format_pval <- function(p) {
  dplyr::case_when(
    is.na(p)    ~ "—",
    p < 0.001   ~ "<0.001",
    TRUE        ~ formatC(p, digits = 3, format = "f")
  )
}

extralines_table <- list(
  "p-val. pre-trends (placebo)" = format_pval(pvals_vec)
)

# -----------------------------------------------------------------------------
# 8. Export des résultats au format texte
# -----------------------------------------------------------------------------

table_twfe <- capture.output(
  fixest::etable(
    modeles,
    se.below    = TRUE,
    fitstat     = ~ n + r2,
    extralines  = extralines_table
  )
)

writeLines(
  table_twfe,
  path("output", "tables", "twfe_resultats.txt")
)

# -----------------------------------------------------------------------------
# 9. Export des coefficients au format exploitable
#    On joint également les p-values de pre-trends.
# -----------------------------------------------------------------------------

coefficients_twfe <- purrr::imap_dfr(
  modeles,
  ~ broom::tidy(.x, conf.int = TRUE) %>%
    dplyr::mutate(
      modele            = .y,
      pval_pretrend     = pvals_pretrend[[.y]] %||% NA_real_
    )
)

write_csv2(
  coefficients_twfe,
  path("output", "tables", "twfe_coefficients.csv")
)

message("Résultats TWFE écrits dans output/tables/twfe_resultats.txt")
message("Coefficients TWFE écrits dans output/tables/twfe_coefficients.csv")