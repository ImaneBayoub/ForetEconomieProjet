# -----------------------------------------------------------------------------
# 05_robustesse_sens_variation.R
# Sensibilité des estimateurs AS à la séparation entre hausses et baisses
# de forêt et de lisière
# -----------------------------------------------------------------------------
# Objectif :
#   Estimer séparément les AS pour les hausses et les baisses de forêt et de
#   lisière, en appliquant la même méthodologie que dans les estimations
#   principales.  Cela permet de vérifier si les résultats globaux sont portés
#   par des effets symétriques ou s'ils sont principalement liés à un sens de
#   changement.
#
# Entrée :
#   data/processed/twfe_data_enrichie.parquet
#
# Sorties :
#   output/tables/as_hausses_baisses.csv
#   output/figures/as_hausses_baisses.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimation AS séparée pour hausses et baisses de forêt et de lisière")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher_foret   <- 0.035
seuil_switcher_lisiere <- 0.02

alpha_placebo  <- 0.05
n_bootstrap    <- 200
min_stayers    <- 30
min_switchers  <- 30

set.seed(123)

# Active le parallélisme si {furrr} + {future} sont disponibles et qu'un plan
# multisession a été configuré en amont (ex. future::plan(multisession)).
use_parallel <- requireNamespace("furrr",  quietly = TRUE) &&
                requireNamespace("future", quietly = TRUE)

# -----------------------------------------------------------------------------
# 2. Charger la base
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  fichier <- path("data", "processed", "twfe_data.parquet")
}

if (!file.exists(fichier)) {
  stop(
    "Base TWFE introuvable. Lancez d'abord les scripts de préparation.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

check_required_cols(
  base,
  c("id", "periode", "productivite", "pct_foret", "pct_lisiere"),
  "twfe_data"
)

# -----------------------------------------------------------------------------
# 3. Préparer la base en format large forêt / lisière
# -----------------------------------------------------------------------------

df_long <- base %>%
  dplyr::select(
    id,
    dplyr::any_of(c("nom_commune", "type_lca")),
    periode,
    productivite,
    pct_foret,
    pct_lisiere
  ) %>%
  dplyr::mutate(
    productivite = as.numeric(productivite),
    Y           = safe_log(productivite),
    pct_foret   = as.numeric(pct_foret),
    pct_lisiere = as.numeric(pct_lisiere)
  ) %>%
  dplyr::filter(!is.na(id), !is.na(periode), !is.na(Y)) %>%
  tidyr::pivot_longer(
    cols      = c(pct_foret, pct_lisiere),
    names_to  = "traitement",
    values_to = "D"
  ) %>%
  dplyr::mutate(
    traitement = dplyr::recode(
      traitement,
      pct_foret   = "foret",
      pct_lisiere = "lisiere"
    )
  ) %>%
  dplyr::filter(!is.na(D))

df_large <- df_long %>%
  tidyr::pivot_wider(
    id_cols     = dplyr::any_of(c("id", "nom_commune", "type_lca", "traitement")),
    names_from  = periode,
    values_from = c(Y, D),
    names_sep   = ""
  ) %>%
  dplyr::filter(
    !is.na(Y1), !is.na(Y2), !is.na(Y3),
    !is.na(D1), !is.na(D2), !is.na(D3)
  ) %>%
  dplyr::mutate(
    seuil_switcher = dplyr::case_when(
      traitement == "foret"   ~ seuil_switcher_foret,
      traitement == "lisiere" ~ seuil_switcher_lisiere,
      TRUE ~ NA_real_
    ),
    delta_D        = D3 - D2,
    delta_logY     = Y3 - Y2,
    delta_logY_pre = Y2 - Y1,
    type_changement = dplyr::case_when(
      delta_D >  seuil_switcher ~ "hausse",
      delta_D < -seuil_switcher ~ "baisse",
      abs(delta_D) <= seuil_switcher ~ "stayer",
      TRUE ~ NA_character_
    )
  )

message("Répartition des types de changement par traitement :")
print(table(df_large$traitement, df_large$type_changement, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 4. Fonction d'estimation AS par traitement et sens de changement
# -----------------------------------------------------------------------------

estimer_as_sens <- function(df_large, traitement_cible, sens) {

  stopifnot(traitement_cible %in% c("foret", "lisiere"))
  stopifnot(sens %in% c("hausse", "baisse"))

  seuil_switcher_traitement <- switch(
    traitement_cible,
    foret   = seuil_switcher_foret,
    lisiere = seuil_switcher_lisiere
  )

  # --- Constructeur de tibble résultat : NA par défaut, surcharges optionnelles
  # Centralise les 4 blocs tibble::tibble() redondants de la version d'origine.
  construire_resultat <- function(commentaire         = "",
                                  estimateur_as        = NA_real_,
                                  erreur_standard      = NA_real_,
                                  statistique_t        = NA_real_,
                                  p_value              = NA_real_,
                                  ic_95_bas            = NA_real_,
                                  ic_95_haut           = NA_real_,
                                  placebo_estimate     = NA_real_,
                                  placebo_erreur_standard = NA_real_,
                                  placebo_statistique_t   = NA_real_,
                                  placebo_p_value      = NA_real_,
                                  placebo_rejet        = NA,
                                  n_obs_avant_trim     = 0L,
                                  n_obs_trim           = 0L,
                                  n_stayers            = 0L,
                                  n_switchers          = 0L,
                                  lower_D2             = NA_real_,
                                  upper_D2             = NA_real_,
                                  n_bootstrap_reussis  = NA_integer_,
                                  methode_se           = "Bootstrap par commune") {
    tibble::tibble(
      traitement              = traitement_cible,
      sens_changement         = sens,
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
      n_observations_avant_trim = n_obs_avant_trim,
      n_observations_trim     = n_obs_trim,
      n_stayers               = n_stayers,
      n_switchers             = n_switchers,
      lower_D2                = lower_D2,
      upper_D2                = upper_D2,
      n_bootstrap_reussis     = n_bootstrap_reussis,
      methode_se              = methode_se,
      commentaire             = commentaire
    )
  }

  # ---------------------------------------------------------------------------
  # Filtrage sur le traitement et le sens de changement
  # ---------------------------------------------------------------------------

  df_sens <- df_large %>%
    dplyr::filter(
      traitement      == traitement_cible,
      type_changement %in% c("stayer", sens)
    ) %>%
    dplyr::mutate(S = as.integer(type_changement == sens))

  if (nrow(df_sens) == 0) {
    return(construire_resultat(commentaire = "Aucune observation exploitable"))
  }

  # ---------------------------------------------------------------------------
  # Trimming sur le support commun de D2 (Q0–Q95 des stayers)
  # ---------------------------------------------------------------------------

  bornes <- stats::quantile(
    df_sens$D2[df_sens$S == 0],
    probs = c(0, 0.95),
    na.rm = TRUE
  )

  df_trim <- df_sens %>%
    dplyr::filter(
      !is.na(delta_logY), !is.na(delta_logY_pre), !is.na(D2), !is.na(delta_D),
      D2 >= bornes[[1]], D2 <= bornes[[2]]
    )

  n_stayers   <- sum(df_trim$S == 0, na.rm = TRUE)
  n_switchers <- sum(df_trim$S == 1, na.rm = TRUE)

  if (n_stayers < min_stayers || n_switchers < min_switchers) {
    return(construire_resultat(
      commentaire     = "Trop peu de stayers ou de switchers",
      n_obs_avant_trim = nrow(df_sens),
      n_obs_trim      = nrow(df_trim),
      n_stayers       = n_stayers,
      n_switchers     = n_switchers,
      lower_D2        = bornes[[1]],
      upper_D2        = bornes[[2]]
    ))
  }

  # ---------------------------------------------------------------------------
  # Fonction interne : calcule (AS, placebo) sur n'importe quel échantillon.
  # Utilisée pour l'estimation principale ET chaque réplication bootstrap,
  # ce qui garantit la cohérence des SE (calqué sur 04_as_heterogeneite.R).
  # ---------------------------------------------------------------------------

  k_gam <- min(10, floor(n_stayers / 5))   # fixé sur l'échantillon complet

  fit_as_placebo <- function(df_b) {

    stayers_b   <- df_b[df_b$S == 0, , drop = FALSE]
    switchers_b <- df_b[df_b$S == 1, , drop = FALSE]

    # --- Placebo : le changement futur de D ne doit pas prédire ΔY passé ---
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

    # --- Garde-fous avant le GAM ---
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
      mgcv::gam(
        delta_logY ~ s(D2, k = k_b),
        data      = stayers_b,
        method    = "REML",
        na.action = na.omit
      ),
      error = function(e) NULL
    )
    if (is.null(mod_gam)) {
      return(list(as = NA_real_, placebo = pla_val))
    }

    y_hat  <- stats::predict(mod_gam, newdata = switchers_b)
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
      n_obs_avant_trim = nrow(df_sens),
      n_obs_trim       = nrow(df_trim),
      n_stayers        = n_stayers,
      n_switchers      = n_switchers,
      lower_D2         = bornes[[1]],
      upper_D2         = bornes[[2]]
    ))
  }

  # ---------------------------------------------------------------------------
  # Bootstrap cluster par commune
  # Pré-calcul des indices : lookup O(1) au lieu d'un filter() O(n) répété.
  # ---------------------------------------------------------------------------

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

  message(sprintf(
    "  Bootstrap [%s / %s] : %d/%d réplications AS réussies.",
    traitement_cible, sens, length(boot_as), n_bootstrap
  ))

  # ---------------------------------------------------------------------------
  # Inférence sur l'estimateur AS
  # ---------------------------------------------------------------------------

  if (length(boot_as) < 10) {
    erreur_standard <- NA_real_
    statistique_t   <- NA_real_
    p_value         <- NA_real_
    ic_95_bas       <- NA_real_
    ic_95_haut      <- NA_real_
    commentaire_as  <- "Bootstrap AS insuffisant"
  } else {
    erreur_standard <- stats::sd(boot_as)
    statistique_t   <- estimateur_as / erreur_standard
    p_value         <- 2 * (1 - stats::pnorm(abs(statistique_t)))
    ic_bornes       <- stats::quantile(boot_as, c(0.025, 0.975),
                                       na.rm = TRUE, names = FALSE)
    ic_95_bas       <- ic_bornes[1]
    ic_95_haut      <- ic_bornes[2]
    commentaire_as  <- "OK - bootstrap"
  }

  # ---------------------------------------------------------------------------
  # Inférence sur le placebo (bootstrap, cohérente avec l'AS)
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
    if (isTRUE(placebo_rejet) && commentaire_as == "OK - bootstrap") {
      commentaire_as <- "AS estimé mais placebo rejette les pré-tendances"
    }
  }

  construire_resultat(
    commentaire             = commentaire_as,
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
    n_obs_avant_trim        = nrow(df_sens),
    n_obs_trim              = nrow(df_trim),
    n_stayers               = n_stayers,
    n_switchers             = n_switchers,
    lower_D2                = bornes[[1]],
    upper_D2                = bornes[[2]],
    n_bootstrap_reussis     = length(boot_as),
    methode_se              = sprintf(
      "Bootstrap par commune (%d/%d réplications)", length(boot_as), n_bootstrap
    )
  )
}

# -----------------------------------------------------------------------------
# 5. Estimer séparément hausses et baisses, forêt et lisière
# -----------------------------------------------------------------------------

resultats_sens <- purrr::map_dfr(
  list(
    list(t = "foret",   s = "hausse"),
    list(t = "foret",   s = "baisse"),
    list(t = "lisiere", s = "hausse"),
    list(t = "lisiere", s = "baisse")
  ),
  function(x) {
    message(sprintf("> %s / %s ...", x$t, x$s))
    estimer_as_sens(df_large, x$t, x$s)
  }
) %>%
  dplyr::mutate(
    traitement_label = dplyr::recode(
      traitement,
      foret   = "Forêt",
      lisiere = "Lisière"
    ),
    sens_label = dplyr::recode(
      sens_changement,
      hausse = "Hausse",
      baisse = "Baisse"
    )
  )

# -----------------------------------------------------------------------------
# 6. Export du tableau
# -----------------------------------------------------------------------------

readr::write_csv2(
  resultats_sens,
  path("output", "tables", "as_hausses_baisses.csv")
)

# -----------------------------------------------------------------------------
# 7. Graphique des estimateurs avec IC 95 %
# -----------------------------------------------------------------------------

donnees_graphique <- resultats_sens %>%
  dplyr::filter(
    !is.na(estimateur_as),
    !is.na(ic_95_bas),
    !is.na(ic_95_haut)
  ) %>%
  dplyr::mutate(
    sens_label = factor(sens_label, levels = c("Baisse", "Hausse")),
    traitement_label = factor(traitement_label, levels = c("Forêt", "Lisière"))
  )

if (nrow(donnees_graphique) > 0) {

  position_traitement <- ggplot2::position_dodge(width = 0.45)

  p_sens <- ggplot2::ggplot(
    donnees_graphique,
    ggplot2::aes(x = sens_label, y = estimateur_as, color = traitement_label)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype   = "dashed",
      alpha      = 0.6
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ic_95_bas, ymax = ic_95_haut),
      position  = position_traitement,
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title    = "Estimateur AS selon le sens du changement",
      subtitle = "Comparaison des traitements forêt et lisière",
      x        = "Sens du changement",
      y        = "Estimateur AS avec IC 95 %",
      color    = "Traitement"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    filename = path("output", "figures", "as_hausses_baisses.png"),
    plot     = p_sens,
    width    = 7,
    height   = 5,
    dpi      = 300
  )
}

message(
  "Résultats écrits dans output/tables/as_hausses_baisses.csv",
  "\nFigure sauvegardée dans output/figures/as_hausses_baisses.png"
)

message_step("AS hausses/baisses terminé")