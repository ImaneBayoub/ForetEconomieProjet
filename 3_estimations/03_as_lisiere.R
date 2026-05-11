# -----------------------------------------------------------------------------
# 03_as_lisiere.R
# Estimer l'effet AS de la variation de lisière agriculture-forêt
# sur la productivité agricole entre 2000 et 2012
# -----------------------------------------------------------------------------
# Entrée :
#   data/processed/base_twfe_enrichie.parquet
#   ou data/processed/base_twfe.parquet
#
# Sorties :
#   output/tables/as_lisiere_resultats.csv
#   output/tables/as_lisiere_placebo.csv
#   output/tables/as_lisiere_sensibilite_seuil.csv
#   output/figures/as_lisiere_support_commun.png
#   output/figures/as_lisiere_tendance_stayers.png
#   output/figures/as_lisiere_effet_selon_seuil.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimation AS : effet de la lisière")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher <- 0.005
n_bootstrap <- 200
set.seed(123)

# -----------------------------------------------------------------------------
# 2. Charger la base unique
# -----------------------------------------------------------------------------

fichier_base <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier_base)) {
  fichier_base <- path("data", "processed", "twfe_data.parquet")
}

if (!file.exists(fichier_base)) {
  stop(
    "Base TWFE introuvable. Lance d'abord les scripts de préparation.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier_base)

# -----------------------------------------------------------------------------
# 3. Construire une base longue standardisée
# -----------------------------------------------------------------------------
# Y = productivité agricole
# D = part de lisière agriculture-forêt
# Z = part de forêt, utilisée seulement comme variable descriptive/contrôle
# -----------------------------------------------------------------------------

df_long <- base %>%
  dplyr::select(
    id,
    dplyr::any_of("nom_commune"),
    periode,
    ratio_prod_surface,
    pct_foret,
    pct_lisiere,
    dplyr::any_of(c("cluster", "type_lca"))
  ) %>%
  dplyr::mutate(
    Y = as.numeric(productivite),
    D = as.numeric(pct_lisiere),
    Z = as.numeric(pct_foret)
  ) %>%
  dplyr::filter(
    !is.na(id),
    !is.na(periode),
    !is.na(Y),
    !is.na(D)
  )

# -----------------------------------------------------------------------------
# 4. Passage au format large pour calculer les deltas
# -----------------------------------------------------------------------------

df_large <- df_long %>%
  dplyr::select(
    id,
    dplyr::any_of(c("nom_commune", "cluster", "type_lca")),
    periode,
    Y,
    D,
    Z
  ) %>%
  tidyr::pivot_wider(
    names_from = periode,
    values_from = c(Y, D, Z),
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
    delta_Y = Y3 - Y2,
    delta_Z = Z3 - Z2,
    S = as.integer(abs(delta_D) > seuil_switcher),
    delta_logY = dplyr::if_else(Y2 > 0 & Y3 > 0, log(Y3) - log(Y2), NA_real_),
    delta_Y_pre = Y2 - Y1,
    delta_logY_pre = dplyr::if_else(Y1 > 0 & Y2 > 0, log(Y2) - log(Y1), NA_real_)
  )

# -----------------------------------------------------------------------------
# 5. Test placebo des tendances parallèles
# -----------------------------------------------------------------------------

modele_placebo <- lm(
  delta_logY_pre ~ D1 + delta_D,
  data = df_large,
  na.action = na.omit
)

placebo_table <- broom::tidy(modele_placebo)

write_csv2(
  placebo_table,
  path("output", "tables", "as_lisiere_placebo.csv")
)

print(summary(modele_placebo))

# -----------------------------------------------------------------------------
# 6. Support commun et trimming
# -----------------------------------------------------------------------------

lower <- stats::quantile(
  df_large$D2[df_large$S == 0],
  0.05,
  na.rm = TRUE
)

upper <- stats::quantile(
  df_large$D2[df_large$S == 0],
  0.95,
  na.rm = TRUE
)

df_trim <- df_large %>%
  dplyr::filter(
    !is.na(delta_logY),
    D2 >= lower,
    D2 <= upper,
    S == 0 | abs(delta_D) > seuil_switcher
  )

message(
  "Observations avant trimming : ", nrow(df_large),
  " | après trimming : ", nrow(df_trim)
)

message("Nombre de stayers et switchers après trimming :")
print(table(df_trim$S))

# Figure support commun
p_support <- ggplot2::ggplot(
  df_trim,
  ggplot2::aes(x = D2, fill = factor(S))
) +
  ggplot2::geom_density(alpha = 0.4) +
  ggplot2::labs(
    title = "Support commun après trimming",
    x = "Part de lisière en 2000",
    y = "Densité",
    fill = "Groupe"
  ) +
  ggplot2::scale_fill_discrete(
    labels = c("Stayers", "Switchers")
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "as_lisiere_support_commun.png"),
  plot = p_support,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 7. Estimation AS
# -----------------------------------------------------------------------------

stayers <- df_trim %>% dplyr::filter(S == 0)
switchers <- df_trim %>% dplyr::filter(S == 1)

if (nrow(stayers) < 30 | nrow(switchers) < 30) {
  stop("Trop peu de stayers ou de switchers pour estimer l'AS.", call. = FALSE)
}

modele_stayers <- mgcv::gam(
  delta_logY ~ s(D2, k = 10),
  data = stayers,
  na.action = na.omit
)

switchers <- switchers %>%
  dplyr::mutate(
    delta_logY_hat = stats::predict(modele_stayers, newdata = switchers)
  )

delta_AS <- with(
  switchers,
  sum(delta_D * (delta_logY - delta_logY_hat), na.rm = TRUE) /
    sum(delta_D^2, na.rm = TRUE)
)

# Figure tendance estimée chez les stayers
p_tendance <- ggplot2::ggplot(
  df_trim,
  ggplot2::aes(x = D2, y = delta_logY, color = factor(S))
) +
  ggplot2::geom_point(alpha = 0.25) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 10),
    se = TRUE
  ) +
  ggplot2::labs(
    title = "Évolution de la productivité selon la lisière initiale",
    x = "Part de lisière en 2000",
    y = "Variation log de productivité 2000-2012",
    color = "Groupe"
  ) +
  ggplot2::scale_color_discrete(
    labels = c("Stayers", "Switchers")
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "as_lisiere_tendance_stayers.png"),
  plot = p_tendance,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 8. Bootstrap rapide par commune
# -----------------------------------------------------------------------------

calculer_as <- function(data) {
  stayers_boot <- data %>% dplyr::filter(S == 0)
  switchers_boot <- data %>% dplyr::filter(S == 1)
  
  if (nrow(stayers_boot) < 30 | nrow(switchers_boot) < 30) {
    return(NA_real_)
  }
  
  mod <- mgcv::gam(
    delta_logY ~ s(D2, k = 10),
    data = stayers_boot,
    na.action = na.omit
  )
  
  y_hat <- stats::predict(mod, newdata = switchers_boot)
  
  sum(
    switchers_boot$delta_D * (switchers_boot$delta_logY - y_hat),
    na.rm = TRUE
  ) /
    sum(switchers_boot$delta_D^2, na.rm = TRUE)
}

ids <- unique(df_trim$id)

boot_results <- numeric(n_bootstrap)

for (b in seq_len(n_bootstrap)) {
  boot_ids <- sample(ids, size = length(ids), replace = TRUE)
  
  boot_df <- purrr::map_dfr(
    boot_ids,
    ~ df_trim %>% dplyr::filter(id == .x)
  )
  
  boot_results[b] <- calculer_as(boot_df)
}

boot_results <- boot_results[!is.na(boot_results)]

se_delta <- stats::sd(boot_results)
t_stat <- delta_AS / se_delta
p_val <- 2 * (1 - stats::pnorm(abs(t_stat)))
ci_low <- stats::quantile(boot_results, 0.025)
ci_high <- stats::quantile(boot_results, 0.975)

resultats_as <- tibble::tibble(
  traitement = "lisiere",
  seuil_switcher = seuil_switcher,
  estimateur_as = delta_AS,
  erreur_standard = se_delta,
  statistique_t = t_stat,
  p_value = p_val,
  ic_95_bas = ci_low,
  ic_95_haut = ci_high,
  n_observations_trim = nrow(df_trim),
  n_stayers = sum(df_trim$S == 0),
  n_switchers = sum(df_trim$S == 1),
  n_bootstrap_reussis = length(boot_results)
)

write_csv2(
  resultats_as,
  path("output", "tables", "as_lisiere_resultats.csv")
)

print(resultats_as)

# -----------------------------------------------------------------------------
# 9. Sensibilité au seuil de définition des switchers
# -----------------------------------------------------------------------------

estimer_as_seuil <- function(df_base, seuil) {
  
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
        seuil = seuil,
        estimateur_as = NA_real_,
        n_switchers = n_switchers,
        n_stayers = n_stayers
      )
    )
  }
  
  stayers <- df_seuil %>% dplyr::filter(S == 0)
  switchers <- df_seuil %>% dplyr::filter(S == 1)
  
  mod <- mgcv::gam(
    delta_logY ~ s(D2, k = 10),
    data = stayers,
    na.action = na.omit
  )
  
  y_hat <- stats::predict(mod, newdata = switchers)
  
  effet <- sum(
    switchers$delta_D * (switchers$delta_logY - y_hat),
    na.rm = TRUE
  ) /
    sum(switchers$delta_D^2, na.rm = TRUE)
  
  tibble::tibble(
    seuil = seuil,
    estimateur_as = effet,
    n_switchers = n_switchers,
    n_stayers = n_stayers
  )
}

grille_seuils <- seq(0.001, 0.03, by = 0.001)

sensibilite_seuil <- purrr::map_dfr(
  grille_seuils,
  ~ estimer_as_seuil(df_large, .x)
)

write_csv2(
  sensibilite_seuil,
  path("output", "tables", "as_lisiere_sensibilite_seuil.csv")
)

p_seuil <- ggplot2::ggplot(
  sensibilite_seuil,
  ggplot2::aes(x = seuil, y = estimateur_as)
) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_line(alpha = 0.5) +
  ggplot2::labs(
    title = "Sensibilité de l'estimateur AS au seuil",
    x = "Seuil de définition des switchers",
    y = "Estimateur AS"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "as_lisiere_effet_selon_seuil.png"),
  plot = p_seuil,
  width = 8,
  height = 5,
  dpi = 300
)

message("Estimation AS lisière terminée.")