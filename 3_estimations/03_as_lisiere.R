# -----------------------------------------------------------------------------
# 03_as_lisiere.R
# Estimer l'effet AS de la variation de lisière agriculture-forêt
# sur le log de la productivité agricole entre 2000 et 2012
# -----------------------------------------------------------------------------
# Entrées :
#   data/processed/twfe_data_enrichie.parquet
#   ou data/processed/twfe_data.parquet
#
# Sorties :
#   output/tables/as_lisiere_resultats.csv
#   output/tables/as_lisiere_placebo.csv
#   output/tables/as_lisiere_sensibilite_seuil.csv
#   output/figures/lisiere_support_commun.png
#   output/figures/lisiere_tendance_stayers.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimation AS : effet de la lisière")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher <- 0.03
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
    "Base TWFE introuvable. Lancez d'abord les scripts de préparation.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier_base)

check_required_cols(
  base,
  c("id", "periode", "productivite", "pct_foret", "pct_lisiere"),
  "twfe_data"
)

# -----------------------------------------------------------------------------
# 3. Construire une base longue standardisée
# -----------------------------------------------------------------------------
# Y = log de la productivité agricole
# D = part de lisière agriculture-forêt
# Z = part de forêt, utilisée seulement comme variable descriptive
# -----------------------------------------------------------------------------

df_long <- base %>%
  dplyr::select(
    id,
    dplyr::any_of("nom_commune"),
    periode,
    productivite,
    pct_foret,
    pct_lisiere,
    dplyr::any_of("type_lca")
  ) %>%
  dplyr::mutate(
    productivite = as.numeric(productivite),
    Y = safe_log(productivite),
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
    dplyr::any_of(c("nom_commune", "type_lca")),
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
    delta_logY = delta_Y,
    delta_Y_pre = Y2 - Y1,
    delta_logY_pre = delta_Y_pre
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
# 5b. Graphique placebo
# -----------------------------------------------------------------------------

coef_placebo <- broom::tidy(modele_placebo)
beta_delta <- stats::coef(modele_placebo)[["delta_D"]]
p_delta <- coef_placebo$p.value[coef_placebo$term == "delta_D"]

figure_placebo <- ggplot2::ggplot(
  df_large,
  ggplot2::aes(x = delta_D, y = delta_logY_pre)
) +
  ggplot2::geom_point(alpha = 0.05) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "#C00000"
  ) +
  ggplot2::labs(
    title = "Test placebo : variation future de lisière vs pré-tendance des rendements",
    subtitle = sprintf(
      "β = %.3f, p = %.3f — pente non significative",
      beta_delta, p_delta
    ),
    x = expression(paste(Delta, D[23], " (variation lisière 2000–2012)")),
    y = expression(paste(Delta, "log ", Y[12], " (variation rendements 1988–2000)"))
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "lisiere_placebo.png"),
  plot = figure_placebo,
  width = 7,
  height = 5,
  dpi = 300
)

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
  filename = path("output", "figures", "lisiere_support_commun.png"),
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

modele_stayers <- lm(
  delta_logY ~ D2,
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

p_tendance <- ggplot2::ggplot(
  df_trim,
  ggplot2::aes(x = D2, y = delta_logY, color = factor(S))
) +
  ggplot2::geom_point(alpha = 0.25) +
  ggplot2::geom_smooth(method = "lm", se = TRUE) +
  ggplot2::labs(
    title = "Évolution du log de productivité selon la lisière initiale",
    x = "Part de lisière en 2000",
    y = "Variation du log de productivité 2000-2012",
    color = "Groupe"
  ) +
  ggplot2::scale_color_discrete(
    labels = c("Stayers", "Switchers")
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "lisiere_tendance_stayers.png"),
  plot = p_tendance,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 8. Bootstrap par commune
# -----------------------------------------------------------------------------

calculer_as <- function(data) {
  
  stayers_boot <- data %>% dplyr::filter(S == 0)
  switchers_boot <- data %>% dplyr::filter(S == 1)
  
  if (nrow(stayers_boot) < 30 | nrow(switchers_boot) < 30) {
    return(NA_real_)
  }
  
  mod <- lm(
    delta_logY ~ D2,
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
  variable_dependante = "log(productivite)",
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
  path("output", "tables", "lisiere_resultats.csv")
)

message("Estimation AS lisière terminée.")


# -----------------------------------------------------------------------------
# 8. Erreur standard rapide sans bootstrap
# -----------------------------------------------------------------------------

switchers <- switchers %>%
  dplyr::mutate(
    residu_as = delta_logY - delta_logY_hat
  )

modele_as <- lm(
  residu_as ~ 0 + delta_D,
  data = switchers
)

vcov_as <- sandwich::vcovHC(
  modele_as,
  type = "HC1"
)

se_delta <- sqrt(diag(vcov_as))[["delta_D"]]

t_stat <- delta_AS / se_delta
p_val <- 2 * (1 - stats::pnorm(abs(t_stat)))

ci_low <- delta_AS - 1.96 * se_delta
ci_high <- delta_AS + 1.96 * se_delta

resultats_as <- tibble::tibble(
  traitement = "lisiere",
  variable_dependante = "log(productivite)",
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
  methode_se = "HC1 sur switchers"
)
