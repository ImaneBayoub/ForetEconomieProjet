# -----------------------------------------------------------------------------
# 05_verifications_robustesse.R
# Sensibilité des estimateurs AS au seuil de définition des switchers
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")
source("R/as_estimator.R")

message_step("Lancement des vérifications de robustesse AS")

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

seuils <- c(0.001, 0.0025, 0.005, 0.01, 0.02)
traitements <- c("pct_foret", "pct_lisiere")

robustesse <- purrr::map_dfr(traitements, function(traitement) {
  purrr::map_dfr(seuils, function(seuil) {
    estimate_as(
      data = base,
      treatment = traitement,
      outcome = "productivite",
      threshold_switcher = seuil,
      n_boot = 199,
      trend_model = "loess",
      group_label = "ensemble"
    )$results
  })
})

write_csv2(
  robustesse,
  path("output", "tables", "sensibilite_seuil_as.csv")
)

donnees_graphique <- robustesse %>%
  dplyr::filter(!is.na(as_estimate))

if (nrow(donnees_graphique) > 0) {
  
  graphique_robustesse <- ggplot2::ggplot(
    donnees_graphique,
    ggplot2::aes(
      x = threshold_switcher,
      y = as_estimate,
      color = treatment
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = ci_low,
        ymax = ci_high
      ),
      width = 0
    ) +
    ggplot2::labs(
      title = "Sensibilité de l'estimateur AS au seuil de définition des switchers",
      x = "Seuil minimal de variation du traitement",
      y = "Estimateur AS",
      color = "Traitement"
    ) +
    ggplot2::theme_minimal()
  
  ggplot2::ggsave(
    filename = path("output", "figures", "sensibilite_seuil_as.png"),
    plot = graphique_robustesse,
    width = 7,
    height = 5
  )
}

message("Vérifications de robustesse écrites dans output/tables/sensibilite_seuil_as.csv")