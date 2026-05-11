# -----------------------------------------------------------------------------
# 05_verifications_robustesse.R
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
      D2 >= lower,
      D2 <= upper
    )
  
  n_switchers <- sum(df_seuil$S == 1, na.rm = TRUE)
  n_stayers <- sum(df_seuil$S == 0, na.rm = TRUE)
  
  if (n_switchers < 30 | n_stayers < 30) {
    return(
      tibble::tibble(
        traitement = nom_traitement,
        variable_dependante = "log(productivite)",
        seuil = seuil,
        estimateur_as = NA_real_,
        n_switchers = n_switchers,
        n_stayers = n_stayers,
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
  
  effet <- sum(
    switchers$delta_D * (switchers$delta_logY - y_hat),
    na.rm = TRUE
  ) /
    sum(
      switchers$delta_D^2,
      na.rm = TRUE
    )
  
  tibble::tibble(
    traitement = nom_traitement,
    variable_dependante = "log(productivite)",
    seuil = seuil,
    estimateur_as = effet,
    n_switchers = n_switchers,
    n_stayers = n_stayers,
    commentaire = "OK"
  )
}

# -----------------------------------------------------------------------------
# 4. Estimation sur une grille de seuils
# -----------------------------------------------------------------------------

grille_seuils <- seq(0.001, 0.03, by = 0.001)

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
# 6. Figure de sensibilité
# -----------------------------------------------------------------------------

donnees_graphique <- sensibilite_seuil %>%
  dplyr::filter(!is.na(estimateur_as))

if (nrow(donnees_graphique) > 0) {
  
  p_seuil <- ggplot2::ggplot(
    donnees_graphique,
    ggplot2::aes(
      x = seuil,
      y = estimateur_as,
      color = traitement
    )
  ) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::labs(
      title = "Sensibilité de l'estimateur AS au seuil de définition des switchers",
      x = "Seuil de définition des switchers",
      y = "Estimateur AS",
      color = "Traitement"
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

message("Vérifications de robustesse AS terminées.")
