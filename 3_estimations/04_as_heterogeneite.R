# -----------------------------------------------------------------------------
# 04_as_heterogeneite.R
# Analyses d'hétérogénéité de l'estimateur AS selon la typologie agricole LCA
# -----------------------------------------------------------------------------
# Objectif :
#   Estimer séparément l'effet de la forêt et de la lisière selon le type agricole
#   des communes, défini à partir de la typologie LCA.
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

seuil_switcher_foret <- 0.035
seuil_switcher_lisiere <- 0.02

alpha_placebo <- 0.05

n_bootstrap <- 100

min_stayers <- 30
min_switchers <- 15

set.seed(123)

# -----------------------------------------------------------------------------
# 2. Charger la base enrichie
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  stop(
    "Base enrichie introuvable : ",
    fichier,
    "\nLancez d'abord le script qui ajoute la typologie agricole LCA.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

check_required_cols(
  base,
  c(
    "id",
    "periode",
    "productivite",
    "pct_foret",
    "pct_lisiere",
    "type_lca"
  ),
  "twfe_data_enrichie"
)

# -----------------------------------------------------------------------------
# 3. Fonction locale d'estimation AS
# -----------------------------------------------------------------------------

estimer_as <- function(data, traitement, nom_traitement, groupe_lca) {
  
  seuil_switcher_traitement <- if (nom_traitement == "forêt") {
    seuil_switcher_foret
  } else if (nom_traitement == "lisière") {
    seuil_switcher_lisiere
  } else {
    stop("Traitement inconnu : ", nom_traitement, call. = FALSE)
  }
  
  resultat_vide <- function(commentaire, n_communes = 0, n_stayers = 0, n_switchers = 0) {
    tibble::tibble(
      type_lca = groupe_lca,
      traitement = nom_traitement,
      variable_dependante = "log(productivite)",
      seuil_switcher = seuil_switcher_traitement,
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
      n_communes = n_communes,
      n_stayers = n_stayers,
      n_switchers = n_switchers,
      methode_se = "HC1 sur switchers",
      commentaire = commentaire
    )
  }
  
  df_large <- data %>%
    dplyr::select(
      id,
      periode,
      productivite,
      D = dplyr::all_of(traitement)
    ) %>%
    dplyr::mutate(
      productivite = as.numeric(productivite),
      Y = safe_log(productivite),
      D = as.numeric(D)
    ) %>%
    dplyr::filter(
      !is.na(id),
      !is.na(periode),
      !is.na(Y),
      !is.na(D)
    ) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = periode,
      values_from = c(Y, D),
      names_sep = ""
    ) %>%
    dplyr::filter(
      !is.na(Y1),
      !is.na(Y2),
      !is.na(Y3),
      !is.na(D1),
      !is.na(D2),
      !is.na(D3)
    ) %>%
    dplyr::mutate(
      delta_D = D3 - D2,
      S = as.integer(abs(delta_D) > seuil_switcher_traitement),
      delta_logY = Y3 - Y2,
      delta_logY_pre = Y2 - Y1
    )
  
  if (nrow(df_large) == 0) {
    return(
      resultat_vide(
        commentaire = "Aucune observation exploitable"
      )
    )
  }
  
  lower <- stats::quantile(
    df_large$D2[df_large$S == 0],
    0.0,
    na.rm = TRUE
  )
  
  upper <- stats::quantile(
    df_large$D2[df_large$S == 0],
    0.95,
    na.rm = TRUE
  )
  
  if (is.na(lower) || is.na(upper)) {
    return(
      resultat_vide(
        commentaire = "Support des stayers non calculable",
        n_communes = dplyr::n_distinct(df_large$id),
        n_stayers = sum(df_large$S == 0, na.rm = TRUE),
        n_switchers = sum(df_large$S == 1, na.rm = TRUE)
      )
    )
  }
  
  df_trim <- df_large %>%
    dplyr::filter(
      !is.na(delta_logY),
      !is.na(delta_logY_pre),
      !is.na(delta_D),
      D2 >= lower,
      D2 <= upper
    )
  
  n_stayers <- sum(df_trim$S == 0, na.rm = TRUE)
  n_switchers <- sum(df_trim$S == 1, na.rm = TRUE)
  
  if (n_stayers < min_stayers | n_switchers < min_switchers) {
    return(
      resultat_vide(
        commentaire = "Trop peu de stayers ou de switchers",
        n_communes = dplyr::n_distinct(df_trim$id),
        n_stayers = n_stayers,
        n_switchers = n_switchers
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Test placebo des pré-tendances
  # ---------------------------------------------------------------------------
  
  modele_placebo <- lm(
    delta_logY_pre ~ D2 + delta_D,
    data = df_trim,
    na.action = na.omit
  )
  
  placebo_vcov <- sandwich::vcovHC(
    modele_placebo,
    type = "HC1"
  )
  
  placebo_test <- lmtest::coeftest(
    modele_placebo,
    vcov. = placebo_vcov
  )
  
  if ("delta_D" %in% rownames(placebo_test)) {
    placebo_estimate <- placebo_test["delta_D", "Estimate"]
    placebo_erreur_standard <- placebo_test["delta_D", "Std. Error"]
    placebo_statistique_t <- placebo_test["delta_D", "t value"]
    placebo_p_value <- placebo_test["delta_D", "Pr(>|t|)"]
    placebo_rejet <- !is.na(placebo_p_value) && placebo_p_value <= alpha_placebo
  } else {
    placebo_estimate <- NA_real_
    placebo_erreur_standard <- NA_real_
    placebo_statistique_t <- NA_real_
    placebo_p_value <- NA_real_
    placebo_rejet <- NA
  }
  
  # ---------------------------------------------------------------------------
  # Estimation AS
  # ---------------------------------------------------------------------------
  
  stayers <- df_trim %>%
    dplyr::filter(S == 0)
  
  switchers <- df_trim %>%
    dplyr::filter(S == 1)
  
  k_gam <- min(10, floor(nrow(stayers) / 5))
  
  if (k_gam < 4) {
    stop("Trop peu de stayers pour estimer un GAM flexible.", call. = FALSE)
  }
  
  modele_stayers <- mgcv::gam(
    delta_logY ~ s(D2, k = k_gam),
    data = stayers,
    na.action = na.omit
  )
  
  delta_logY_hat <- stats::predict(
    modele_stayers,
    newdata = switchers
  )
  
  switchers <- switchers %>%
    dplyr::mutate(
      residu_as = delta_logY - delta_logY_hat
    )
  
  denominateur <- sum(
    switchers$delta_D^2,
    na.rm = TRUE
  )
  
  if (is.na(denominateur) || denominateur == 0) {
    return(
      tibble::tibble(
        type_lca = groupe_lca,
        traitement = nom_traitement,
        variable_dependante = "log(productivite)",
        seuil_switcher = seuil_switcher_traitement,
        estimateur_as = NA_real_,
        erreur_standard = NA_real_,
        statistique_t = NA_real_,
        p_value = NA_real_,
        ic_95_bas = NA_real_,
        ic_95_haut = NA_real_,
        placebo_estimate = placebo_estimate,
        placebo_erreur_standard = placebo_erreur_standard,
        placebo_statistique_t = placebo_statistique_t,
        placebo_p_value = placebo_p_value,
        placebo_rejet = placebo_rejet,
        n_communes = dplyr::n_distinct(df_trim$id),
        n_stayers = n_stayers,
        n_switchers = n_switchers,
        methode_se = "HC1 sur switchers",
        commentaire = "Dénominateur AS nul"
      )
    )
  }
  
  estimateur_as <- sum(
    switchers$delta_D * switchers$residu_as,
    na.rm = TRUE
  ) / denominateur
  
  # ---------------------------------------------------------------------------
  # Bootstrap par commune
  # ---------------------------------------------------------------------------
  calculer_as_boot <- function(data_boot) {
    
    stayers_boot <- data_boot %>%
      dplyr::filter(S == 0)
    
    switchers_boot <- data_boot %>%
      dplyr::filter(S == 1)
    
    if (nrow(stayers_boot) < min_stayers | nrow(switchers_boot) < min_switchers) {
      return(NA_real_)
    }
    
    denom_boot <- sum(
      switchers_boot$delta_D^2,
      na.rm = TRUE
    )
    
    if (is.na(denom_boot) || denom_boot == 0) {
      return(NA_real_)
    }
    
    mod_boot <- lm(
      delta_logY ~ D2,
      data = stayers_boot,
      na.action = na.omit
    )
    
    y_hat_boot <- stats::predict(
      mod_boot,
      newdata = switchers_boot
    )
    
    sum(
      switchers_boot$delta_D * (switchers_boot$delta_logY - y_hat_boot),
      na.rm = TRUE
    ) / denom_boot
  }

  ids <- unique(df_trim$id)

  boot_results <- numeric(n_bootstrap)

  for (b in seq_len(n_bootstrap)) {
    
    boot_ids <- sample(
      ids,
      size = length(ids),
      replace = TRUE
    )
    
    boot_df <- purrr::map_dfr(
      boot_ids,
      ~ df_trim %>% dplyr::filter(id == .x)
    )
    
    boot_results[b] <- calculer_as_boot(boot_df)
  }

  boot_results <- boot_results[!is.na(boot_results)]

  if (length(boot_results) < 10) {
    erreur_standard <- NA_real_
    statistique_t <- NA_real_
    p_value <- NA_real_
    ic_95_bas <- NA_real_
    ic_95_haut <- NA_real_
    methode_se <- "Bootstrap par commune"
    commentaire <- "Bootstrap insuffisant"
  } else {
    erreur_standard <- stats::sd(boot_results)
    statistique_t <- estimateur_as / erreur_standard
    p_value <- 2 * (1 - stats::pnorm(abs(statistique_t)))
    ic_95_bas <- stats::quantile(
      boot_results,
      0.025,
      na.rm = TRUE,
      names = FALSE
    )
    ic_95_haut <- stats::quantile(
      boot_results,
      0.975,
      na.rm = TRUE,
      names = FALSE
    )
    methode_se <- paste0(
      "Bootstrap par commune, ",
      length(boot_results),
      "/",
      n_bootstrap,
      " réplications réussies"
    )
    
    commentaire <- ifelse(
      isTRUE(placebo_rejet),
      "AS estimé mais placebo rejette les pré-tendances",
      "OK - bootstrap, placebo non rejeté"
    )
  }

# -----------------------------------------------------------------------------
# 4. Estimations par type agricole
# -----------------------------------------------------------------------------

base_lca <- base %>%
  dplyr::filter(!is.na(type_lca))

types_lca <- sort(unique(base_lca$type_lca))

resultats <- purrr::map_dfr(types_lca, function(type) {
  
  sous_base <- base_lca %>%
    dplyr::filter(type_lca == type)
  
  dplyr::bind_rows(
    estimer_as(
      data = sous_base,
      traitement = "pct_foret",
      nom_traitement = "forêt",
      groupe_lca = type
    ),
    estimer_as(
      data = sous_base,
      traitement = "pct_lisiere",
      nom_traitement = "lisière",
      groupe_lca = type
    )
  )
})

# -----------------------------------------------------------------------------
# 5. Export
# -----------------------------------------------------------------------------

readr::write_csv2(
  resultats,
  path("output", "tables", "as_par_type_lca.csv")
)

# -----------------------------------------------------------------------------
# 6. Graphique des coefficients AS par traitement et par sous-groupe
# -----------------------------------------------------------------------------

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
      "forêt" = "Forêt",
      "lisière" = "Lisière"
    )
  )

# Ajout des résultats totaux forêt et lisière
fichier_total_lisiere <- path("output", "tables", "as_lisiere_resultats.csv")
fichier_total_foret <- path("output", "tables", "as_foret_resultats.csv")

totaux <- tibble::tibble()

if (file.exists(fichier_total_lisiere)) {
  total_lisiere <- readr::read_csv(
    fichier_total_lisiere,
    show_col_types = FALSE
  )
  
  totaux <- dplyr::bind_rows(
    totaux,
    tibble::tibble(
      groupe = "Total",
      traitement = "Lisière",
      estimateur_as = total_lisiere$estimateur_as,
      ic_95_bas = total_lisiere$ic_95_bas,
      ic_95_haut = total_lisiere$ic_95_haut
    )
  )
}

if (file.exists(fichier_total_foret)) {
  total_foret <- readr::read_csv(
    fichier_total_foret,
    show_col_types = FALSE
  )
  
  totaux <- dplyr::bind_rows(
    totaux,
    tibble::tibble(
      groupe = "Total",
      traitement = "Forêt",
      estimateur_as = total_foret$estimateur_as,
      ic_95_bas = total_foret$ic_95_bas,
      ic_95_haut = total_foret$ic_95_haut
    )
  )
}

resultats_graph <- dplyr::bind_rows(
  resultats_graph,
  totaux
) %>%
  dplyr::filter(!is.na(estimateur_as)) %>%
  dplyr::mutate(
    groupe = factor(
      groupe,
      levels = c("Total", "Annuel", "Mixte/Élevage", "Pérenne")
    ),
    traitement = factor(
      traitement,
      levels = c("Forêt", "Lisière")
    )
  )

figure_coef_as <- resultats_graph %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = estimateur_as,
      y = groupe,
      color = traitement
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = ic_95_bas,
      xmax = ic_95_haut
    ),
    height = 0.18,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::geom_point(
    size = 3,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::labs(
    title = "Estimateur AS par spécialisation agricole",
    subtitle = "Comparaison des traitements forêt et lisière",
    x = "Estimateur AS avec IC 95 %",
    y = NULL,
    color = "Traitement"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = path("output", "figures", "as_par_type_lca.png"),
  plot = figure_coef_as,
  width = 8,
  height = 4.5
)

message(
  "Résultats d'hétérogénéité écrits dans output/tables/as_par_type_lca.csv",
  "\nFigure sauvegardée dans output/figures/as_par_type_lca.png"
)

message("AS par type agricole LCA terminé")