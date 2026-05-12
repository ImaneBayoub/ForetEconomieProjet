# -----------------------------------------------------------------------------
# 06_robustesse_seuil_switchers.R
# Sensibilité des estimateurs AS au seuil de définition des switchers
# -----------------------------------------------------------------------------
# Objectif :
#   Tester si les estimateurs AS pour la forêt et la lisière sont sensibles
#   au seuil minimal utilisé pour définir les switchers.
#
# Entrées :
#   data/processed/twfe_data_enrichie.parquet
#   ou data/processed/twfe_data.parquet
#
# Sorties :
#   output/tables/as_sensibilite_seuil.csv
#   output/figures/as_sensibilite_seuil.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Lancement des vérifications de robustesse AS")


# -----------------------------------------------------------------------------
# 0. Paramètres
# -----------------------------------------------------------------------------

grille_seuils <- seq(0.001, 0.1, by = 0.001)
alpha_placebo <- 0.05

# -----------------------------------------------------------------------------
# 1. Charger la base
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

check_required_cols(
  base,
  c("id", "periode", "productivite", "pct_foret", "pct_lisiere"),
  "twfe_data"
)

# -----------------------------------------------------------------------------
# 2. Fonction de préparation de la base large
# -----------------------------------------------------------------------------

preparer_base_large <- function(base, traitement) {
  
  base %>%
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
      delta_logY = Y3 - Y2,
      delta_logY_pre = Y2 - Y1
    )
}
# -----------------------------------------------------------------------------
# 3. Fonction d'estimation AS pour un seuil donné
# -----------------------------------------------------------------------------

estimer_as_seuil <- function(df_base, seuil, nom_traitement) {
  
  df_seuil <- df_base %>%
    dplyr::mutate(
      S = as.integer(abs(delta_D) > seuil)
    )
  
  lower <- stats::quantile(
    df_seuil$D2[df_seuil$S == 0],
    0.05,
    na.rm = TRUE
  )
  
  upper <- stats::quantile(
    df_seuil$D2[df_seuil$S == 0],
    0.95,
    na.rm = TRUE
  )
  
  df_seuil <- df_seuil %>%
    dplyr::filter(
      !is.na(delta_logY),
      !is.na(delta_logY_pre),
      D2 >= lower,
      D2 <= upper
    )
  
  n_switchers <- sum(df_seuil$S == 1, na.rm = TRUE)
  n_stayers <- sum(df_seuil$S == 0, na.rm = TRUE)
  n_observations_trim <- nrow(df_seuil)
  
  # ---------------------------------------------------------------------------
  # Test placebo des tendances parallèles
  # ---------------------------------------------------------------------------
  # On teste si la variation future du traitement, D3 - D2,
  # explique la variation passée de l'outcome, Y2 - Y1.
  #
  # Si le coefficient de delta_D est significatif, cela suggère un problème
  # de tendances parallèles.
  # ---------------------------------------------------------------------------
  
  placebo_result <- tryCatch(
    {
      modele_placebo <- lm(
        delta_logY_pre ~ D2 + delta_D,
        data = df_seuil,
        na.action = na.omit
      )
      
      coefs_placebo <- summary(modele_placebo)$coefficients
      
      if ("delta_D" %in% rownames(coefs_placebo)) {
        p_placebo <- coefs_placebo["delta_D", "Pr(>|t|)"]
      } else {
        p_placebo <- NA_real_
      }
      
      placebo_test <- dplyr::case_when(
        is.na(p_placebo) ~ "non calculable",
        p_placebo < alpha_placebo ~ "rejetée",
        p_placebo >= alpha_placebo ~ "non rejetée"
      )
      
      list(
        p_value_placebo = p_placebo,
        placebo_test = placebo_test
      )
    },
    error = function(e) {
      list(
        p_value_placebo = NA_real_,
        placebo_test = "non calculable"
      )
    }
  )
  
  p_value_placebo <- placebo_result$p_value_placebo
  placebo_test <- placebo_result$placebo_test
  
  if (n_switchers < 30 | n_stayers < 30) {
    return(
      tibble::tibble(
        traitement = nom_traitement,
        variable_dependante = "log(productivite)",
        seuil = seuil,
        estimateur_as = NA_real_,
        erreur_standard = NA_real_,
        statistique_t = NA_real_,
        p_value = NA_real_,
        ic_95_bas = NA_real_,
        ic_95_haut = NA_real_,
        p_value_placebo = p_value_placebo,
        `placebo test` = placebo_test,
        n_observations_trim = n_observations_trim,
        n_switchers = n_switchers,
        n_stayers = n_stayers,
        methode_se = "HC1 sur switchers",
        commentaire = "Trop peu de stayers ou de switchers"
      )
    )
  }
  
  stayers <- df_seuil %>%
    dplyr::filter(S == 0)
  
  switchers <- df_seuil %>%
    dplyr::filter(S == 1)
  
  mod <- lm(
    delta_logY ~ D2,
    data = stayers,
    na.action = na.omit
  )
  
  y_hat <- stats::predict(mod, newdata = switchers)
  
  switchers <- switchers %>%
    dplyr::mutate(
      delta_logY_hat = y_hat,
      residu_as = delta_logY - delta_logY_hat
    )
  
  effet <- sum(
    switchers$delta_D * switchers$residu_as,
    na.rm = TRUE
  ) /
    sum(
      switchers$delta_D^2,
      na.rm = TRUE
    )
  
  # ---------------------------------------------------------------------------
  # Erreur standard rapide sans bootstrap : HC1 sur les switchers
  # ---------------------------------------------------------------------------
  
  modele_as <- lm(
    residu_as ~ 0 + delta_D,
    data = switchers
  )
  
  vcov_as <- sandwich::vcovHC(
    modele_as,
    type = "HC1"
  )
  
  se_delta <- sqrt(diag(vcov_as))[["delta_D"]]
  
  if (is.na(se_delta) || se_delta == 0) {
    t_stat <- NA_real_
    p_val <- NA_real_
    ci_low <- NA_real_
    ci_high <- NA_real_
    commentaire <- "Erreur standard non calculable"
  } else {
    t_stat <- effet / se_delta
    p_val <- 2 * (1 - stats::pnorm(abs(t_stat)))
    ci_low <- effet - 1.96 * se_delta
    ci_high <- effet + 1.96 * se_delta
    commentaire <- "OK"
  }
  
  tibble::tibble(
    traitement = nom_traitement,
    variable_dependante = "log(productivite)",
    seuil = seuil,
    estimateur_as = effet,
    erreur_standard = se_delta,
    statistique_t = t_stat,
    p_value = p_val,
    ic_95_bas = ci_low,
    ic_95_haut = ci_high,
    p_value_placebo = p_value_placebo,
    `placebo test` = placebo_test,
    n_observations_trim = n_observations_trim,
    n_switchers = n_switchers,
    n_stayers = n_stayers,
    methode_se = "HC1 sur switchers",
    commentaire = commentaire
  )
}

# -----------------------------------------------------------------------------
# 4. Estimation sur une grille de seuils
# -----------------------------------------------------------------------------

df_large_foret <- preparer_base_large(
  base = base,
  traitement = "pct_foret"
)

df_large_lisiere <- preparer_base_large(
  base = base,
  traitement = "pct_lisiere"
)

sensibilite_foret <- purrr::map_dfr(
  grille_seuils,
  ~ estimer_as_seuil(
    df_base = df_large_foret,
    seuil = .x,
    nom_traitement = "forêt"
  )
)

sensibilite_lisiere <- purrr::map_dfr(
  grille_seuils,
  ~ estimer_as_seuil(
    df_base = df_large_lisiere,
    seuil = .x,
    nom_traitement = "lisière"
  )
)

sensibilite_seuil <- dplyr::bind_rows(
  sensibilite_foret,
  sensibilite_lisiere
)

# -----------------------------------------------------------------------------
# 5. Export du tableau
# -----------------------------------------------------------------------------

write_csv2(
  sensibilite_seuil,
  path("output", "tables", "as_sensibilite_seuil.csv")
)

# -----------------------------------------------------------------------------
# 6. Figure de sensibilité avec IC 95 %
# -----------------------------------------------------------------------------

donnees_graphique <- sensibilite_seuil %>%
  dplyr::filter(
    !is.na(estimateur_as),
    !is.na(ic_95_bas),
    !is.na(ic_95_haut)
  )

if (nrow(donnees_graphique) > 0) {
  
  p_seuil <- ggplot2::ggplot(
    donnees_graphique,
    ggplot2::aes(
      x = seuil,
      y = estimateur_as,
      color = traitement,
      fill = traitement
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ic_95_bas,
        ymax = ic_95_haut
      ),
      alpha = 0.18,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(alpha = 0.7, size = 1.7) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      alpha = 0.6
    ) +
    ggplot2::labs(
      title = "Sensibilité de l'estimateur AS au seuil de définition des switchers",
      subtitle = "Bandes : intervalles de confiance à 95 %",
      x = "Seuil de définition des switchers",
      y = "Estimateur AS",
      color = "Traitement",
      fill = "Traitement"
    ) +
    ggplot2::theme_minimal()
  
  ggplot2::ggsave(
    filename = path("output", "figures", "as_sensibilite_seuil.png"),
    plot = p_seuil,
    width = 8,
    height = 5,
    dpi = 300
  )
}

message_step("Vérifications de robustesse AS terminées")