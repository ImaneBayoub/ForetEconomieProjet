# -----------------------------------------------------------------------------
# 05_as_hausses_baisses_foret_lisiere.R
# Estimation AS séparée pour les hausses et les baisses de forêt et de lisière.
# -----------------------------------------------------------------------------
# Output :
# - output/tables/as_hausses_baisses.csv
# - output/figures/as_hausses_baisses.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimation AS séparée pour hausses et baisses de forêt et de lisière")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher_foret <- 0.05
seuil_switcher_lisiere <- 0.03

alpha_placebo <- 0.05

min_stayers <- 30
min_switchers <- 30

set.seed(123)

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
# 3. Préparer une base longue forêt / lisière
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
    Y = safe_log(productivite),
    pct_foret = as.numeric(pct_foret),
    pct_lisiere = as.numeric(pct_lisiere)
  ) %>%
  dplyr::filter(
    !is.na(id),
    !is.na(periode),
    !is.na(Y)
  ) %>%
  tidyr::pivot_longer(
    cols = c(pct_foret, pct_lisiere),
    names_to = "traitement",
    values_to = "D"
  ) %>%
  dplyr::mutate(
    traitement = dplyr::recode(
      traitement,
      pct_foret = "foret",
      pct_lisiere = "lisiere"
    )
  ) %>%
  dplyr::filter(!is.na(D))

df_large <- df_long %>%
  tidyr::pivot_wider(
    id_cols = dplyr::any_of(c("id", "nom_commune", "type_lca", "traitement")),
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
    seuil_switcher = dplyr::case_when(
      traitement == "foret" ~ seuil_switcher_foret,
      traitement == "lisiere" ~ seuil_switcher_lisiere,
      TRUE ~ NA_real_
    ),
    delta_D = D3 - D2,
    abs_delta_D = abs(delta_D),
    delta_logY = Y3 - Y2,
    delta_logY_pre = Y2 - Y1,
    type_changement = dplyr::case_when(
      delta_D > seuil_switcher ~ "hausse",
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
  
  if (!traitement_cible %in% c("foret", "lisiere")) {
    stop("traitement_cible doit être 'foret' ou 'lisiere'.", call. = FALSE)
  }
  
  if (!sens %in% c("hausse", "baisse")) {
    stop("sens doit être 'hausse' ou 'baisse'.", call. = FALSE)
  }

  seuil_switcher_traitement <- dplyr::case_when(
    traitement_cible == "foret" ~ seuil_switcher_foret,
    traitement_cible == "lisiere" ~ seuil_switcher_lisiere,
    TRUE ~ NA_real_
  )

  if (is.na(seuil_switcher_traitement)) {
    stop("Seuil switcher non défini pour : ", traitement_cible, call. = FALSE)
  }
  
  df_sens <- df_large %>%
    dplyr::filter(
      traitement == traitement_cible,
      type_changement %in% c("stayer", sens)
    ) %>%
    dplyr::mutate(
      S = as.integer(type_changement == sens)
    )
  
  if (nrow(df_sens) == 0) {
    return(
      tibble::tibble(
        traitement = traitement_cible,
        sens_changement = sens,
        variable_dependante = "log(productivite)",
        seuil_switcher = seuil_switcher_traitement,
        estimateur_as = NA_real_,
        erreur_standard = NA_real_,
        statistique_t = NA_real_,
        p_value = NA_real_,
        ic_95_bas = NA_real_,
        ic_95_haut = NA_real_,
        placebo_estimate = NA_real_,
        placebo_p_value = NA_real_,
        placebo_rejet = NA,
        n_observations_avant_trim = 0,
        n_observations_trim = 0,
        n_stayers = 0,
        n_switchers = 0,
        lower_D2 = NA_real_,
        upper_D2 = NA_real_,
        lower_pre = NA_real_,
        upper_pre = NA_real_,
        methode_se = "HC1 sur switchers",
        commentaire = "Aucune observation exploitable"
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Trimming sur le support commun de D2
  # ---------------------------------------------------------------------------
  
  lower_D2 <- stats::quantile(
    df_sens$D2[df_sens$S == 0],
    0.05,
    na.rm = TRUE
  )
  
  upper_D2 <- stats::quantile(
    df_sens$D2[df_sens$S == 0],
    0.95,
    na.rm = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Trimming optionnel sur les pré-tendances
  # ---------------------------------------------------------------------------
  
  lower_pre <- stats::quantile(
    df_sens$delta_logY_pre[df_sens$S == 0],
    0.05,
    na.rm = TRUE
  )
  
  upper_pre <- stats::quantile(
    df_sens$delta_logY_pre[df_sens$S == 0],
    0.95,
    na.rm = TRUE
  )
  
  df_trim <- df_sens %>%
    dplyr::filter(
      !is.na(delta_logY),
      !is.na(delta_logY_pre),
      !is.na(D2),
      !is.na(delta_D),
      D2 >= lower_D2,
      D2 <= upper_D2,
      delta_logY_pre >= lower_pre,
      delta_logY_pre <= upper_pre
    )
  
  n_stayers <- sum(df_trim$S == 0, na.rm = TRUE)
  n_switchers <- sum(df_trim$S == 1, na.rm = TRUE)
  
  if (n_stayers < min_stayers | n_switchers < min_switchers) {
    return(
      tibble::tibble(
        traitement = traitement_cible,
        sens_changement = sens,
        variable_dependante = "log(productivite)",
        seuil_switcher = seuil_switcher_traitement,
        estimateur_as = NA_real_,
        erreur_standard = NA_real_,
        statistique_t = NA_real_,
        p_value = NA_real_,
        ic_95_bas = NA_real_,
        ic_95_haut = NA_real_,
        placebo_estimate = NA_real_,
        placebo_p_value = NA_real_,
        placebo_rejet = NA,
        n_observations_avant_trim = nrow(df_sens),
        n_observations_trim = nrow(df_trim),
        n_stayers = n_stayers,
        n_switchers = n_switchers,
        lower_D2 = lower_D2,
        upper_D2 = upper_D2,
        lower_pre = lower_pre,
        upper_pre = upper_pre,
        methode_se = "HC1 sur switchers",
        commentaire = "Trop peu de stayers ou de switchers"
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Test placebo des pré-tendances
  # ---------------------------------------------------------------------------
  # On teste si le changement futur du traitement prédit la variation passée de Y.
  
  modele_placebo <- lm(
    delta_logY_pre ~ D2 + delta_D,
    data = df_trim,
    na.action = na.omit
  )
  
  placebo_table <- broom::tidy(modele_placebo)
  
  placebo_delta <- placebo_table %>%
    dplyr::filter(term == "delta_D")
  
  placebo_estimate <- placebo_delta$estimate[1]
  placebo_p_value <- placebo_delta$p.value[1]
  placebo_rejet <- !is.na(placebo_p_value) && placebo_p_value <= alpha_placebo
  
  # ---------------------------------------------------------------------------
  # Estimation AS
  # ---------------------------------------------------------------------------
  
  stayers <- df_trim %>%
    dplyr::filter(S == 0)
  
  switchers <- df_trim %>%
    dplyr::filter(S == 1)
  
  modele_stayers <- lm(
    delta_logY ~ D2 + delta_logY_pre,
    data = stayers,
    na.action = na.omit
  )
  
  switchers <- switchers %>%
    dplyr::mutate(
      delta_logY_hat = stats::predict(modele_stayers, newdata = switchers),
      residu_as = delta_logY - delta_logY_hat
    )
  
  denominateur <- sum(switchers$delta_D^2, na.rm = TRUE)
  
  if (is.na(denominateur) || denominateur == 0) {
    return(
      tibble::tibble(
        traitement = traitement_cible,
        sens_changement = sens,
        variable_dependante = "log(productivite)",
        seuil_switcher = seuil_switcher_traitement,
        estimateur_as = NA_real_,
        erreur_standard = NA_real_,
        statistique_t = NA_real_,
        p_value = NA_real_,
        ic_95_bas = NA_real_,
        ic_95_haut = NA_real_,
        placebo_estimate = placebo_estimate,
        placebo_p_value = placebo_p_value,
        placebo_rejet = placebo_rejet,
        n_observations_avant_trim = nrow(df_sens),
        n_observations_trim = nrow(df_trim),
        n_stayers = n_stayers,
        n_switchers = n_switchers,
        lower_D2 = lower_D2,
        upper_D2 = upper_D2,
        lower_pre = lower_pre,
        upper_pre = upper_pre,
        methode_se = "HC1 sur switchers",
        commentaire = "Dénominateur AS nul"
      )
    )
  }
  
  delta_AS <- sum(
    switchers$delta_D * switchers$residu_as,
    na.rm = TRUE
  ) / denominateur
  
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
    t_stat <- delta_AS / se_delta
    p_val <- 2 * (1 - stats::pnorm(abs(t_stat)))
    ci_low <- delta_AS - 1.96 * se_delta
    ci_high <- delta_AS + 1.96 * se_delta
    commentaire <- ifelse(
      placebo_rejet,
      "AS estimé mais placebo rejette les parallel trends",
      "OK - placebo non rejeté"
    )
  }
  
  tibble::tibble(
    traitement = traitement_cible,
    sens_changement = sens,
    variable_dependante = "log(productivite)",
    seuil_switcher = seuil_switcher_traitement,
    estimateur_as = delta_AS,
    erreur_standard = se_delta,
    statistique_t = t_stat,
    p_value = p_val,
    ic_95_bas = ci_low,
    ic_95_haut = ci_high,
    placebo_estimate = placebo_estimate,
    placebo_p_value = placebo_p_value,
    placebo_rejet = placebo_rejet,
    n_observations_avant_trim = nrow(df_sens),
    n_observations_trim = nrow(df_trim),
    n_stayers = n_stayers,
    n_switchers = n_switchers,
    lower_D2 = lower_D2,
    upper_D2 = upper_D2,
    lower_pre = lower_pre,
    upper_pre = upper_pre,
    methode_se = "HC1 sur switchers",
    commentaire = commentaire
  )
}

# -----------------------------------------------------------------------------
# 5. Estimer séparément hausses et baisses, forêt et lisière
# -----------------------------------------------------------------------------

resultats_sens <- dplyr::bind_rows(
  estimer_as_sens(df_large, "foret", "hausse"),
  estimer_as_sens(df_large, "foret", "baisse"),
  estimer_as_sens(df_large, "lisiere", "hausse"),
  estimer_as_sens(df_large, "lisiere", "baisse")
) %>%
  dplyr::mutate(
    traitement_label = dplyr::recode(
      traitement,
      foret = "Forêt",
      lisiere = "Lisière"
    ),
    sens_label = dplyr::recode(
      sens_changement,
      hausse = "Hausse",
      baisse = "Baisse"
    )
  )

readr::write_csv2(
  resultats_sens,
  path("output", "tables", "as_hausses_baisses.csv")
)

# -----------------------------------------------------------------------------
# 6. Graphique des estimateurs avec IC 95 %
# -----------------------------------------------------------------------------

donnees_graphique <- resultats_sens %>%
  dplyr::filter(
    !is.na(estimateur_as),
    !is.na(ic_95_bas),
    !is.na(ic_95_haut)
  ) %>%
  dplyr::mutate(
    sens_label = factor(
      sens_label,
      levels = c("Baisse", "Hausse")
    ),
    traitement_label = factor(
      traitement_label,
      levels = c("Forêt", "Lisière")
    )
  )

if (nrow(donnees_graphique) > 0) {
  
  position_traitement <- ggplot2::position_dodge(width = 0.45)
  
  p_sens <- ggplot2::ggplot(
    donnees_graphique,
    ggplot2::aes(
      x = sens_label,
      y = estimateur_as,
      color = traitement_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      alpha = 0.6
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = ic_95_bas,
        ymax = ic_95_haut
      ),
      position = position_traitement,
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Estimateur AS selon le sens du changement",
      subtitle = "Comparaison des traitements forêt et lisière",
      x = "Sens du changement",
      y = "Estimateur AS avec IC 95 %",
      color = "Traitement"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  ggplot2::ggsave(
    filename = path("output", "figures", "as_hausses_baisses.png"),
    plot = p_sens,
    width = 7,
    height = 5,
    dpi = 300
  )
}

message(
  "Résultats écrits dans output/tables/as_hausses_baisses.csv",
  "\nFigure sauvegardée dans output/figures/as_hausses_baisses.png"
)