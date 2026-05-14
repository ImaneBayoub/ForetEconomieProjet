# -----------------------------------------------------------------------------
# 04_as_heterogeneite.R
# Analyses d'hétérogénéité de l'estimateur AS selon la typologie agricole LCA
# -----------------------------------------------------------------------------
# Objectif :
#   Estimer séparément l'effet de la forêt et de la lisière selon le type
#   agricole des communes, défini à partir de la typologie LCA.
#
# Entrée :
#   data/processed/twfe_data_enrichie.parquet
#
# Sortie :
#   output/tables/as_par_type_lca.csv
#   output/figures/as_par_type_lca.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimations AS par typologie agricole LCA")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher_foret   <- 0.035
seuil_switcher_lisiere <- 0.02

alpha_placebo <- 0.05
n_bootstrap   <- 200
min_stayers   <- 30
min_switchers <- 15

set.seed(123)

# Active le parallélisme si {furrr} + {future} sont disponibles et qu'un plan
# multisession a été configuré en amont (ex. future::plan(multisession)).
use_parallel <- requireNamespace("furrr", quietly = TRUE) &&
                requireNamespace("future", quietly = TRUE)

# -----------------------------------------------------------------------------
# 2. Charger la base enrichie
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  stop(
    "Base enrichie introuvable : ", fichier,
    "\nLancez d'abord le script qui ajoute la typologie agricole LCA.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

check_required_cols(
  base,
  c("id", "periode", "productivite", "pct_foret", "pct_lisiere", "type_lca"),
  "twfe_data_enrichie"
)

# -----------------------------------------------------------------------------
# 3. Fonction locale d'estimation AS
# -----------------------------------------------------------------------------

estimer_as <- function(data, traitement, nom_traitement, groupe_lca) {

  seuil_switcher_traitement <- switch(
    nom_traitement,
    "forêt"   = seuil_switcher_foret,
    "lisière" = seuil_switcher_lisiere,
    stop("Traitement inconnu : ", nom_traitement, call. = FALSE)
  )

  # --- Constructeur de tibble résultat : NA par défaut, surcharges optionnelles
  construire_resultat <- function(commentaire = "",
                                  estimateur_as = NA_real_,
                                  erreur_standard = NA_real_,
                                  statistique_t = NA_real_,
                                  p_value = NA_real_,
                                  ic_95_bas = NA_real_,
                                  ic_95_haut = NA_real_,
                                  placebo_estimate = NA_real_,
                                  placebo_erreur_standard = NA_real_,
                                  placebo_statistique_t = NA_real_,
                                  placebo_p_value = NA_real_,
                                  placebo_rejet = NA,
                                  n_communes = 0L,
                                  n_stayers = 0L,
                                  n_switchers = 0L,
                                  methode_se = "Bootstrap par commune") {
    tibble::tibble(
      type_lca                = groupe_lca,
      traitement              = nom_traitement,
      variable_dependante     = "log(productivite)",
      seuil_switcher          = seuil_switcher_traitement,
      estimateur_as           = estimateur_as,
      erreur_standard         = erreur_standard,
      statistique_t           = statistique_t,
      p_value                 = p_value,
      ic_95_bas               = ic_95_bas,
      ic_95_haut              = ic_95_haut,
      placebo_estimate        = placebo_estimate,
      placebo_erreur_standard = placebo_erreur_standard,
      placebo_statistique_t   = placebo_statistique_t,
      placebo_p_value         = placebo_p_value,
      placebo_rejet           = placebo_rejet,
      n_communes              = n_communes,
      n_stayers               = n_stayers,
      n_switchers             = n_switchers,
      methode_se              = methode_se,
      commentaire             = commentaire
    )
  }

  # ---------------------------------------------------------------------------
  # Préparation des données en format large
  # ---------------------------------------------------------------------------

  df_large <- data %>%
    dplyr::select(id, periode, productivite, D = dplyr::all_of(traitement)) %>%
    dplyr::mutate(
      productivite = as.numeric(productivite),
      Y = safe_log(productivite),
      D = as.numeric(D)
    ) %>%
    dplyr::filter(!is.na(id), !is.na(periode), !is.na(Y), !is.na(D)) %>%
    tidyr::pivot_wider(
      id_cols     = id,
      names_from  = periode,
      values_from = c(Y, D),
      names_sep   = ""
    ) %>%
    dplyr::filter(
      !is.na(Y1), !is.na(Y2), !is.na(Y3),
      !is.na(D1), !is.na(D2), !is.na(D3)
    ) %>%
    dplyr::mutate(
      delta_D        = D3 - D2,
      S              = as.integer(abs(delta_D) > seuil_switcher_traitement),
      delta_logY     = Y3 - Y2,
      delta_logY_pre = Y2 - Y1
    )

  if (nrow(df_large) == 0) {
    return(construire_resultat(commentaire = "Aucune observation exploitable"))
  }

  # ---------------------------------------------------------------------------
  # Trimming sur le support des stayers (D2 ∈ [Q0, Q95])
  # ---------------------------------------------------------------------------

  bornes <- stats::quantile(
    df_large$D2[df_large$S == 0],
    probs = c(0, 0.95),
    na.rm = TRUE
  )

  if (any(is.na(bornes))) {
    return(construire_resultat(
      commentaire = "Support des stayers non calculable",
      n_communes  = dplyr::n_distinct(df_large$id),
      n_stayers   = sum(df_large$S == 0),
      n_switchers = sum(df_large$S == 1)
    ))
  }

  df_trim <- df_large %>%
    dplyr::filter(
      !is.na(delta_logY), !is.na(delta_logY_pre), !is.na(delta_D),
      D2 >= bornes[[1]], D2 <= bornes[[2]]
    )

  n_stayers   <- sum(df_trim$S == 0)
  n_switchers <- sum(df_trim$S == 1)
  n_communes  <- dplyr::n_distinct(df_trim$id)

  if (n_stayers < min_stayers || n_switchers < min_switchers) {
    return(construire_resultat(
      commentaire = "Trop peu de stayers ou de switchers",
      n_communes  = n_communes,
      n_stayers   = n_stayers,
      n_switchers = n_switchers
    ))
  }

  # ---------------------------------------------------------------------------
  # Fonction interne : retourne (AS, placebo) sur n'importe quel échantillon
  # Utilisée à la fois pour l'estimation principale et pour chaque réplication
  # bootstrap, ce qui garantit la cohérence des SE.
  # ---------------------------------------------------------------------------

  fit_as_placebo <- function(df_b) {

    # --- Placebo : régression des pré-tendances (indépendante du calcul AS) ---
    mod_pla <- tryCatch(
      lm(delta_logY_pre ~ D2 + delta_D, data = df_b, na.action = na.omit),
      error = function(e) NULL
    )
    pla_val <- if (!is.null(mod_pla) &&
                   "delta_D" %in% names(stats::coef(mod_pla))) {
      stats::coef(mod_pla)[["delta_D"]]
    } else {
      NA_real_
    }

    # --- AS via GAM sur les stayers ---
    stayers_b   <- df_b[df_b$S == 0, , drop = FALSE]
    switchers_b <- df_b[df_b$S == 1, , drop = FALSE]

    if (nrow(stayers_b) < min_stayers || nrow(switchers_b) < min_switchers) {
      return(list(as = NA_real_, placebo = pla_val))
    }

    denom <- sum(switchers_b$delta_D^2, na.rm = TRUE)
    if (is.na(denom) || denom == 0) {
      return(list(as = NA_real_, placebo = pla_val))
    }

    k_b <- min(10, floor(nrow(stayers_b) / 5))
    if (k_b < 4) {
      return(list(as = NA_real_, placebo = pla_val))
    }

    mod_gam <- tryCatch(
      mgcv::gam(delta_logY ~ s(D2, k = k_b),
                data = stayers_b, na.action = na.omit),
      error = function(e) NULL
    )
    if (is.null(mod_gam)) {
      return(list(as = NA_real_, placebo = pla_val))
    }

    y_hat <- stats::predict(mod_gam, newdata = switchers_b)
    as_val <- sum(switchers_b$delta_D * (switchers_b$delta_logY - y_hat),
                  na.rm = TRUE) / denom

    list(as = as_val, placebo = pla_val)
  }

  # ---------------------------------------------------------------------------
  # Estimation principale
  # ---------------------------------------------------------------------------

  est_main         <- fit_as_placebo(df_trim)
  estimateur_as    <- est_main$as
  placebo_estimate <- est_main$placebo

  if (is.na(estimateur_as)) {
    return(construire_resultat(
      commentaire      = "Estimateur AS non calculable",
      placebo_estimate = placebo_estimate,
      n_communes       = n_communes,
      n_stayers        = n_stayers,
      n_switchers      = n_switchers
    ))
  }

  # ---------------------------------------------------------------------------
  # Bootstrap par commune (cluster bootstrap)
  # ---------------------------------------------------------------------------
  # Pré-calcul : on stocke pour chaque id la liste des numéros de lignes dans
  # df_trim. Le rééchantillonnage devient ainsi un simple lookup + indexation,
  # ce qui évite le `filter(id == .x)` répété de la version d'origine.

  idx_par_id <- split(seq_len(nrow(df_trim)), df_trim$id)
  ids        <- names(idx_par_id)
  n_ids      <- length(ids)

  une_replication <- function() {
    boot_ids <- sample(ids, size = n_ids, replace = TRUE)
    rows     <- unlist(idx_par_id[boot_ids], use.names = FALSE)
    fit_as_placebo(df_trim[rows, , drop = FALSE])
  }

  boot_list <- if (use_parallel) {
    furrr::future_map(
      seq_len(n_bootstrap),
      function(.) une_replication(),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    replicate(n_bootstrap, une_replication(), simplify = FALSE)
  }

  boot_as      <- vapply(boot_list, function(x) x$as,      numeric(1))
  boot_placebo <- vapply(boot_list, function(x) x$placebo, numeric(1))

  boot_as      <- boot_as[!is.na(boot_as)]
  boot_placebo <- boot_placebo[!is.na(boot_placebo)]

  # ---------------------------------------------------------------------------
  # Inférence sur l'estimateur AS
  # ---------------------------------------------------------------------------

  if (length(boot_as) < 10) {
    erreur_standard <- NA_real_
    statistique_t   <- NA_real_
    p_value         <- NA_real_
    ic_95_bas       <- NA_real_
    ic_95_haut      <- NA_real_
    methode_se_as   <- sprintf("Bootstrap par commune (%d/%d réussies)",
                               length(boot_as), n_bootstrap)
    commentaire     <- "Bootstrap AS insuffisant"
  } else {
    erreur_standard <- stats::sd(boot_as)
    statistique_t   <- estimateur_as / erreur_standard
    p_value         <- 2 * (1 - stats::pnorm(abs(statistique_t)))
    ic_bornes       <- stats::quantile(boot_as, c(0.025, 0.975),
                                       na.rm = TRUE, names = FALSE)
    ic_95_bas       <- ic_bornes[1]
    ic_95_haut      <- ic_bornes[2]
    methode_se_as   <- sprintf("Bootstrap par commune (%d/%d réplications)",
                               length(boot_as), n_bootstrap)
    commentaire     <- "OK - bootstrap"
  }

  # ---------------------------------------------------------------------------
  # Inférence sur le placebo
  # ---------------------------------------------------------------------------

  if (length(boot_placebo) < 10 || is.na(placebo_estimate)) {
    placebo_erreur_standard <- NA_real_
    placebo_statistique_t   <- NA_real_
    placebo_p_value         <- NA_real_
    placebo_rejet           <- NA
  } else {
    placebo_erreur_standard <- stats::sd(boot_placebo)
    placebo_statistique_t   <- placebo_estimate / placebo_erreur_standard
    placebo_p_value         <- 2 * (1 - stats::pnorm(abs(placebo_statistique_t)))
    placebo_rejet           <- !is.na(placebo_p_value) &&
                               placebo_p_value <= alpha_placebo
    if (isTRUE(placebo_rejet) && commentaire == "OK - bootstrap") {
      commentaire <- "AS estimé mais placebo rejette les pré-tendances"
    }
  }

  # ---------------------------------------------------------------------------
  # Retour explicite
  # ---------------------------------------------------------------------------

  construire_resultat(
    commentaire             = commentaire,
    estimateur_as           = estimateur_as,
    erreur_standard         = erreur_standard,
    statistique_t           = statistique_t,
    p_value                 = p_value,
    ic_95_bas               = ic_95_bas,
    ic_95_haut              = ic_95_haut,
    placebo_estimate        = placebo_estimate,
    placebo_erreur_standard = placebo_erreur_standard,
    placebo_statistique_t   = placebo_statistique_t,
    placebo_p_value         = placebo_p_value,
    placebo_rejet           = placebo_rejet,
    n_communes              = n_communes,
    n_stayers               = n_stayers,
    n_switchers             = n_switchers,
    methode_se              = methode_se_as
  )
}

# -----------------------------------------------------------------------------
# 4. Estimations par type agricole
# -----------------------------------------------------------------------------

base_lca  <- base %>% dplyr::filter(!is.na(type_lca))
types_lca <- sort(unique(base_lca$type_lca))

resultats <- purrr::map_dfr(types_lca, function(type) {

  message("  > ", type, " ...")
  sous_base <- base_lca %>% dplyr::filter(type_lca == type)

  dplyr::bind_rows(
    estimer_as(sous_base, "pct_foret",   "forêt",   type),
    estimer_as(sous_base, "pct_lisiere", "lisière", type)
  )
})

# -----------------------------------------------------------------------------
# 5. Export du tableau
# -----------------------------------------------------------------------------

readr::write_csv2(
  resultats,
  path("output", "tables", "as_par_type_lca.csv")
)

# -----------------------------------------------------------------------------
# 6. Graphique des coefficients AS par traitement et par sous-groupe
# -----------------------------------------------------------------------------

# Fonction utilitaire pour récupérer les résultats totaux
charger_total <- function(fichier_csv, label_traitement) {
  if (!file.exists(fichier_csv)) {
    return(tibble::tibble())
  }
  total <- readr::read_csv(fichier_csv, show_col_types = FALSE)
  tibble::tibble(
    groupe        = "Total",
    traitement    = label_traitement,
    estimateur_as = total$estimateur_as,
    ic_95_bas     = total$ic_95_bas,
    ic_95_haut    = total$ic_95_haut
  )
}

totaux <- dplyr::bind_rows(
  charger_total(path("output", "tables", "as_foret_resultats.csv"),   "Forêt"),
  charger_total(path("output", "tables", "as_lisiere_resultats.csv"), "Lisière")
)

resultats_graph <- resultats %>%
  dplyr::filter(traitement %in% c("forêt", "lisière")) %>%
  dplyr::mutate(
    groupe = dplyr::recode(
      type_lca,
      annuel  = "Annuel",
      mixte   = "Mixte/Élevage",
      perenne = "Pérenne"
    ),
    traitement = dplyr::recode(
      traitement,
      "forêt"   = "Forêt",
      "lisière" = "Lisière"
    )
  ) %>%
  dplyr::bind_rows(totaux) %>%
  dplyr::filter(!is.na(estimateur_as)) %>%
  dplyr::mutate(
    groupe = factor(
      groupe,
      levels = c("Total", "Annuel", "Mixte/Élevage", "Pérenne")
    ),
    traitement = factor(traitement, levels = c("Forêt", "Lisière"))
  )

figure_coef_as <- ggplot2::ggplot(
  resultats_graph,
  ggplot2::aes(x = estimateur_as, y = groupe, color = traitement)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = ic_95_bas, xmax = ic_95_haut),
    height   = 0.18,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::geom_point(
    size     = 3,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::labs(
    title    = "Estimateur AS par spécialisation agricole",
    subtitle = "Comparaison des traitements forêt et lisière",
    x        = "Estimateur AS avec IC 95 %",
    y        = NULL,
    color    = "Traitement"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = path("output", "figures", "as_par_type_lca.png"),
  plot     = figure_coef_as,
  width    = 8,
  height   = 4.5
)

message(
  "Résultats d'hétérogénéité écrits dans output/tables/as_par_type_lca.csv",
  "\nFigure sauvegardée dans output/figures/as_par_type_lca.png"
)

message_step("AS par type agricole LCA terminé")