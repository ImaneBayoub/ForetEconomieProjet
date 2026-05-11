# -----------------------------------------------------------------------------
# 03_figures_descriptives.R
# Produire les statistiques descriptives et les figures à partir de la base
# d'analyse enrichie
# -----------------------------------------------------------------------------
# Entrées possibles :
#   data/processed/base_twfe_enrichie.parquet
#   data/processed/base_twfe.parquet
#
# Sorties :
#   output/tables/descriptives_par_periode.csv
#   output/tables/evolutions_2000_2012.csv
#   output/tables/evolutions_par_cluster.csv, si la variable cluster existe
#   output/tables/evolutions_par_type_lca.csv, si la variable type_lca existe
#   output/figures/evolution_foret_lisiere.png
#   output/figures/variation_foret_lisiere.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Production des tableaux et figures descriptifs")

# -----------------------------------------------------------------------------
# 1. Lecture de la base
# -----------------------------------------------------------------------------

fichier <- path("data", "processed", "twfe_data_enrichie.parquet")

if (!file.exists(fichier)) {
  fichier <- path("data", "processed", "twfe_data.parquet")
}

if (!file.exists(fichier)) {
  stop(
    "Base TWFE introuvable. Lance d'abord les scripts de préparation des données.",
    call. = FALSE
  )
}

base <- arrow::read_parquet(fichier)

# -----------------------------------------------------------------------------
# 2. Statistiques descriptives par période
# -----------------------------------------------------------------------------

descriptives_par_periode <- base %>%
  dplyr::group_by(periode, libelle_periode) %>%
  dplyr::summarise(
    nb_communes = dplyr::n_distinct(id),
    productivite_moyenne = mean(productivite, na.rm = TRUE),
    productivite_mediane = stats::median(productivite, na.rm = TRUE),
    pct_agri_moyen = mean(pct_agri, na.rm = TRUE),
    pct_foret_moyen = mean(pct_foret, na.rm = TRUE),
    pct_lisiere_moyen = mean(pct_lisiere, na.rm = TRUE),
    .groups = "drop"
  )

write_csv2(
  descriptives_par_periode,
  path("output", "tables", "descriptives_par_periode.csv")
)

# -----------------------------------------------------------------------------
# 3. Évolutions entre 2000 et 2012
# -----------------------------------------------------------------------------

evolutions_2000_2012 <- base %>%
  dplyr::filter(periode %in% c(2L, 3L)) %>%
  dplyr::select(
    id,
    periode,
    productivite,
    pct_agri,
    pct_foret,
    pct_lisiere,
    dplyr::any_of(c("cluster", "classe_lca", "type_lca"))
  ) %>%
  tidyr::pivot_wider(
    names_from = periode,
    values_from = c(
      productivite,
      pct_agri,
      pct_foret,
      pct_lisiere
    ),
    names_glue = "{.value}_p{periode}"
  ) %>%
  dplyr::mutate(
    variation_productivite = productivite_p3 - productivite_p2,
    variation_pct_agri = pct_agri_p3 - pct_agri_p2,
    variation_pct_foret = pct_foret_p3 - pct_foret_p2,
    variation_pct_lisiere = pct_lisiere_p3 - pct_lisiere_p2
  )

write_csv2(
  evolutions_2000_2012,
  path("output", "tables", "evolutions_2000_2012.csv")
)

# -----------------------------------------------------------------------------
# 4. Évolutions moyennes par cluster territorial, si disponible
# -----------------------------------------------------------------------------

if ("cluster" %in% names(evolutions_2000_2012)) {
  
  evolutions_par_cluster <- evolutions_2000_2012 %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      nb_communes = dplyr::n(),
      variation_moyenne_productivite = mean(variation_productivite, na.rm = TRUE),
      variation_moyenne_pct_agri = mean(variation_pct_agri, na.rm = TRUE),
      variation_moyenne_pct_foret = mean(variation_pct_foret, na.rm = TRUE),
      variation_moyenne_pct_lisiere = mean(variation_pct_lisiere, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv2(
    evolutions_par_cluster,
    path("output", "tables", "evolutions_par_cluster.csv")
  )
}

# -----------------------------------------------------------------------------
# 5. Évolutions moyennes par type LCA, si disponible
# -----------------------------------------------------------------------------

if ("type_lca" %in% names(evolutions_2000_2012)) {
  
  evolutions_par_type_lca <- evolutions_2000_2012 %>%
    dplyr::group_by(type_lca) %>%
    dplyr::summarise(
      nb_communes = dplyr::n(),
      variation_moyenne_productivite = mean(variation_productivite, na.rm = TRUE),
      variation_moyenne_pct_agri = mean(variation_pct_agri, na.rm = TRUE),
      variation_moyenne_pct_foret = mean(variation_pct_foret, na.rm = TRUE),
      variation_moyenne_pct_lisiere = mean(variation_pct_lisiere, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv2(
    evolutions_par_type_lca,
    path("output", "tables", "evolutions_par_type_lca.csv")
  )
}

# -----------------------------------------------------------------------------
# 6. Figure : évolution moyenne de la forêt et de la lisière
# -----------------------------------------------------------------------------

donnees_figure_periode <- descriptives_par_periode %>%
  dplyr::select(
    libelle_periode,
    pct_foret_moyen,
    pct_lisiere_moyen
  ) %>%
  tidyr::pivot_longer(
    cols = c(pct_foret_moyen, pct_lisiere_moyen),
    names_to = "indicateur",
    values_to = "valeur"
  ) %>%
  dplyr::mutate(
    indicateur = dplyr::recode(
      indicateur,
      pct_foret_moyen = "Part forestière moyenne",
      pct_lisiere_moyen = "Part moyenne de lisière"
    )
  )

figure_periode <- ggplot2::ggplot(
  donnees_figure_periode,
  ggplot2::aes(
    x = libelle_periode,
    y = valeur,
    group = indicateur,
    color = indicateur
  )
) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Évolution moyenne de la forêt et de la lisière",
    x = "Période",
    y = "Pourcentage de la surface communale",
    color = "Indicateur"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "evolution_foret_lisiere.png"),
  plot = figure_periode,
  width = 7,
  height = 5
)

# -----------------------------------------------------------------------------
# 7. Figure : variation de la forêt et variation de la lisière
# -----------------------------------------------------------------------------

figure_variations <- ggplot2::ggplot(
  evolutions_2000_2012,
  ggplot2::aes(
    x = variation_pct_foret,
    y = variation_pct_lisiere
  )
) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::geom_smooth(method = "lm", se = FALSE) +
  ggplot2::labs(
    title = "Variation de la forêt et de la lisière entre 2000 et 2012",
    x = "Variation de la part forestière",
    y = "Variation de la part de lisière"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = path("output", "figures", "variation_foret_lisiere.png"),
  plot = figure_variations,
  width = 7,
  height = 5
)

# -----------------------------------------------------------------------------
# 8. Figure : distribution de la lisière par type LCA
# -----------------------------------------------------------------------------

if ("type_lca" %in% names(base) && !all(is.na(base$type_lca))) {

  figure_lisiere_type_lca <- base %>%
    dplyr::filter(!is.na(type_lca), !is.na(pct_lisiere)) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = type_lca, y = pct_lisiere, fill = type_lca
    )) +
    ggplot2::geom_boxplot(outlier.alpha = 0.1) +
    ggplot2::facet_wrap(~ libelle_periode) +
    ggplot2::labs(
      title = "Distribution de la lisière forêt-agriculture par type agricole",
      x = "Type LCA", y = "Part de lisière"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(
    filename = path("output", "figures", "distribution_lisiere_type_lca.png"),
    plot = figure_lisiere_type_lca,
    width = 8,
    height = 5
  )
}

message("Sorties descriptives écrites dans output/tables/ et output/figures/.")
