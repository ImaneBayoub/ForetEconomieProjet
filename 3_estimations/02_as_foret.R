# -----------------------------------------------------------------------------
# 02_as_foret.R
# Estimer l'effet AS de la variation de couverture forestière
# sur le log de la productivité agricole entre 2000 et 2012
# -----------------------------------------------------------------------------
# Entrées :
#   data/processed/twfe_data_enrichie.parquet
#   ou data/processed/twfe_data.parquet
#
# Sorties :
#   output/tables/as_foret_resultats.csv
#   output/tables/as_foret_placebo.csv
#   output/figures/foret_support_commun_effectifs.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimation AS : effet de la forêt")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher <- 0.035
n_bootstrap    <- 200
set.seed(123)

# Active le parallélisme si {furrr} + {future} sont disponibles et qu'un plan
# multisession a été configuré en amont (ex. future::plan(multisession)).
use_parallel <- requireNamespace("furrr",  quietly = TRUE) &&
                requireNamespace("future", quietly = TRUE)

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
# D = part de forêt
# Z = part de lisière agriculture-forêt, utilisée seulement comme variable descriptive
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
    D = as.numeric(pct_foret),
    Z = as.numeric(pct_lisiere)
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
# 5. Support commun et trimming
# -----------------------------------------------------------------------------

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

df_trim_plot <- df_trim %>%
  dplyr::filter(D2 >= 0.1)

p_support_effectifs <- ggplot2::ggplot(
  df_trim_plot,
  ggplot2::aes(x = D2, fill = factor(S))
) +
  ggplot2::geom_histogram(
    binwidth = 0.1,
    position = "dodge",
    alpha = 0.75,
    color = "white",
    boundary = 0
  ) +
  ggplot2::labs(
    title = "Support commun après trimming",
    subtitle = "Part de forêt > 10% pour conserver une échelle d'effectifs lisible",
    x = "Part de forêt en 2000",
    y = "Nombre de communes",
    fill = "Groupe"
  ) +
  ggplot2::scale_fill_discrete(
    labels = c("Stayers", "Switchers")
  ) +
  ggplot2::scale_x_continuous(
    limits = c(lower, upper),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "foret_support_commun_effectifs.png"),
  plot = p_support_effectifs,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 6. Test placebo des tendances parallèles
# -----------------------------------------------------------------------------

modele_placebo <- lm(
  delta_logY_pre ~ D1 + delta_D,
  data = df_trim,
  na.action = na.omit
)

placebo_table <- broom::tidy(modele_placebo)

print(summary(modele_placebo))

write.csv2(
  placebo_table,
  file = path("output", "tables", "as_foret_placebo.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 7. Estimation AS
# -----------------------------------------------------------------------------

stayers   <- df_trim %>% dplyr::filter(S == 0)
switchers <- df_trim %>% dplyr::filter(S == 1)

if (nrow(stayers) < 30 | nrow(switchers) < 30) {
  stop("Trop peu de stayers ou de switchers pour estimer l'AS.", call. = FALSE)
}

k_gam <- min(10, floor(nrow(stayers) / 5))

if (k_gam < 4) {
  stop("Trop peu de stayers pour estimer un GAM flexible.", call. = FALSE)
}

modele_stayers <- mgcv::gam(
  delta_logY ~ s(D2, k = k_gam),
  data = stayers,
  method = "REML",
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

# -----------------------------------------------------------------------------
# 8. Bootstrap par commune (version parallélisée)
# -----------------------------------------------------------------------------
# Pré-calcul des indices par id : le rééchantillonnage devient un simple lookup
# + indexation, ce qui évite le `filter(id == .x)` répété de la boucle for
# d'origine et accélère chaque réplication (séquentielle ou parallèle).

idx_par_id <- split(seq_len(nrow(df_trim)), df_trim$id)
ids        <- names(idx_par_id)
n_ids      <- length(ids)

# k_gam est fixé sur l'échantillon complet des stayers (constant entre
# réplications) pour garantir la comparabilité des modèles.
k_gam_boot <- k_gam

calculer_as_boot <- function() {

  boot_ids  <- sample(ids, size = n_ids, replace = TRUE)
  rows      <- unlist(idx_par_id[boot_ids], use.names = FALSE)
  boot_df   <- df_trim[rows, , drop = FALSE]

  stayers_b   <- boot_df[boot_df$S == 0, , drop = FALSE]
  switchers_b <- boot_df[boot_df$S == 1, , drop = FALSE]

  if (nrow(stayers_b) < 30 || nrow(switchers_b) < 30) {
    return(NA_real_)
  }

  denom <- sum(switchers_b$delta_D^2, na.rm = TRUE)
  if (is.na(denom) || denom == 0) {
    return(NA_real_)
  }

  mod <- tryCatch(
    mgcv::gam(
      delta_logY ~ s(D2, k = k_gam_boot),
      data      = stayers_b,
      method    = "REML",
      na.action = na.omit
    ),
    error = function(e) NULL
  )

  if (is.null(mod)) return(NA_real_)

  y_hat <- stats::predict(mod, newdata = switchers_b)

  sum(switchers_b$delta_D * (switchers_b$delta_logY - y_hat),
      na.rm = TRUE) / denom
}

boot_results <- if (use_parallel) {
  furrr::future_map_dbl(
    seq_len(n_bootstrap),
    function(.) calculer_as_boot(),
    .options = furrr::furrr_options(seed = TRUE)
  )
} else {
  vapply(seq_len(n_bootstrap), function(.) calculer_as_boot(), numeric(1))
}

boot_results <- boot_results[!is.na(boot_results)]

message(sprintf(
  "Bootstrap terminé : %d/%d réplications réussies.",
  length(boot_results), n_bootstrap
))

se_delta <- stats::sd(boot_results)
t_stat   <- delta_AS / se_delta
p_val    <- 2 * (1 - stats::pnorm(abs(t_stat)))
ci_low   <- stats::quantile(boot_results, 0.025)
ci_high  <- stats::quantile(boot_results, 0.975)

resultats_as <- tibble::tibble(
  traitement              = "foret",
  variable_dependante     = "log(productivite)",
  seuil_switcher          = seuil_switcher,
  estimateur_as           = delta_AS,
  erreur_standard         = se_delta,
  statistique_t           = t_stat,
  p_value                 = p_val,
  ic_95_bas               = ci_low,
  ic_95_haut              = ci_high,
  n_observations_trim     = nrow(df_trim),
  n_stayers               = sum(df_trim$S == 0),
  n_switchers             = sum(df_trim$S == 1),
  n_bootstrap_reussis     = length(boot_results)
)

write_csv2(
  resultats_as,
  path("output", "tables", "as_foret_resultats.csv")
)

message("Résultats de l'estimation écrits dans output/tables/as_foret_resultats.csv")
message("Résultats du test placebo écrits dans output/tables/as_foret_placebo.csv")
message("Graphique du support commun avec effectifs écrit dans output/figures/foret_support_commun_effectifs.png")

message("Estimation AS forêt terminée.")