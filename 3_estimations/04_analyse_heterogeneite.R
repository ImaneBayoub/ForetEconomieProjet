# -----------------------------------------------------------------------------
# 04_analyse_heterogeneite.R
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
#   output/tables/as_par_typologie_agricole.csv
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Estimations AS par typologie agricole LCA")

# -----------------------------------------------------------------------------
# 1. Paramètres
# -----------------------------------------------------------------------------

seuil_switcher <- 0.005
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
      S = as.integer(abs(delta_D) > seuil_switcher),
      delta_logY = Y3 - Y2,
      delta_logY_pre = Y2 - Y1
    )
  
  if (nrow(df_large) == 0) {
    return(tibble::tibble(
      type_lca = groupe_lca,
      traitement = nom_traitement,
      variable_dependante = "log(productivite)",
      estimateur_as = NA_real_,
      erreur_standard = NA_real_,
      p_value = NA_real_,
      ic_95_bas = NA_real_,
      ic_95_haut = NA_real_,
      n_communes = 0,
      n_stayers = 0,
      n_switchers = 0,
      commentaire = "Aucune observation exploitable"
    ))
  }
  
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
      D2 <= upper
    )
  
  n_stayers <- sum(df_trim$S == 0, na.rm = TRUE)
  n_switchers <- sum(df_trim$S == 1, na.rm = TRUE)
  
  if (n_stayers < min_stayers | n_switchers < min_switchers) {
    return(tibble::tibble(
      type_lca = groupe_lca,
      traitement = nom_traitement,
      variable_dependante = "log(productivite)",
      estimateur_as = NA_real_,
      erreur_standard = NA_real_,
      p_value = NA_real_,
      ic_95_bas = NA_real_,
      ic_95_haut = NA_real_,
      n_communes = dplyr::n_distinct(df_trim$id),
      n_stayers = n_stayers,
      n_switchers = n_switchers,
      commentaire = "Trop peu de stayers ou de switchers"
    ))
  }
  
  stayers <- df_trim %>%
    dplyr::filter(S == 0)
  
  switchers <- df_trim %>%
    dplyr::filter(S == 1)
  
  modele_stayers <- lm(
    delta_logY ~ D2,
    data = stayers,
    na.action = na.omit
  )
  
  delta_logY_hat <- stats::predict(
    modele_stayers,
    newdata = switchers
  )
  
  estimateur_as <- sum(
    switchers$delta_D * (switchers$delta_logY - delta_logY_hat),
    na.rm = TRUE
  ) /
    sum(
      switchers$delta_D^2,
      na.rm = TRUE
    )
  
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
      sum(
        switchers_boot$delta_D^2,
        na.rm = TRUE
      )
  }
  
  ids <- unique(df_trim$id)
  boot_results <- numeric(n_bootstrap)
  
  for (b in seq_len(n_bootstrap)) {
    boot_ids <- sample(ids, size = length(ids), replace = TRUE)
    
    boot_df <- purrr::map_dfr(
      boot_ids,
      ~ df_trim %>% dplyr::filter(id == .x)
    )
    
    boot_results[b] <- calculer_as_boot(boot_df)
  }
  
  boot_results <- boot_results[!is.na(boot_results)]
  
  if (length(boot_results) < 10) {
    erreur_standard <- NA_real_
    p_value <- NA_real_
    ic_95_bas <- NA_real_
    ic_95_haut <- NA_real_
    commentaire <- "Bootstrap insuffisant"
  } else {
    erreur_standard <- stats::sd(boot_results)
    p_value <- 2 * (1 - stats::pnorm(abs(estimateur_as / erreur_standard)))
    ic_95_bas <- stats::quantile(boot_results, 0.025)
    ic_95_haut <- stats::quantile(boot_results, 0.975)
    commentaire <- "OK"
  }
  
  tibble::tibble(
    type_lca = groupe_lca,
    traitement = nom_traitement,
    variable_dependante = "log(productivite)",
    estimateur_as = estimateur_as,
    erreur_standard = erreur_standard,
    p_value = p_value,
    ic_95_bas = ic_95_bas,
    ic_95_haut = ic_95_haut,
    n_communes = dplyr::n_distinct(df_trim$id),
    n_stayers = n_stayers,
    n_switchers = n_switchers,
    n_bootstrap_reussis = length(boot_results),
    commentaire = commentaire
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

write_csv2(
  resultats,
  path("output", "tables", "as_par_typologie_agricole.csv")
)

message(
  "Résultats d'hétérogénéité écrits dans output/tables/as_par_typologie_agricole.csv"
)

print(resultats)