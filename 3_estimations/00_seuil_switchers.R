# -----------------------------------------------------------------------------
# 01_seuil_switchers.R
# Aide au choix du seuil de définition des switchers
# -----------------------------------------------------------------------------
# Objectif :
#   Examiner la distribution de |ΔD| entre 2000 et 2012 pour choisir un seuil
#   minimal de variation du traitement.
#
# Entrée :
#   data/processed/twfe_data_enrichie.parquet
#   ou data/processed/twfe_data.parquet
#
# Sorties :
#   output/tables/diagnostic_seuil_switchers.csv
#   output/figures/hist_abs_deltaD_foret.png
#   output/figures/hist_abs_deltaD_lisiere.png
#   output/figures/coude_seuil_switchers_foret.png
#   output/figures/coude_seuil_switchers_lisiere.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Diagnostic du seuil de définition des switchers")

# -----------------------------------------------------------------------------
# 0. Paramètres
# -----------------------------------------------------------------------------
# Seuils d'intérêt à afficher sur l'histogramme

seuil_foret = 0.035
seuil_lisiere = 0.02

# -----------------------------------------------------------------------------
# 1. Charger la base
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
  c("id", "periode", "pct_foret", "pct_lisiere"),
  "twfe_data"
)

# -----------------------------------------------------------------------------
# 2. Fonctions auxiliaires
# -----------------------------------------------------------------------------

preparer_delta_traitement <- function(base, traitement, nom_traitement) {
  
  base %>%
    dplyr::select(
      id,
      periode,
      D = dplyr::all_of(traitement)
    ) %>%
    dplyr::mutate(
      D = as.numeric(D)
    ) %>%
    dplyr::filter(
      !is.na(id),
      !is.na(periode),
      !is.na(D)
    ) %>%
    tidyr::pivot_wider(
      names_from = periode,
      values_from = D,
      names_prefix = "D"
    ) %>%
    dplyr::filter(
      !is.na(D2),
      !is.na(D3)
    ) %>%
    dplyr::mutate(
      traitement = nom_traitement,
      delta_D = D3 - D2,
      abs_delta_D = abs(delta_D)
    )
}

calculer_diagnostic_seuils <- function(delta_df) {
  
  delta_non_nul <- delta_df %>%
    dplyr::filter(abs_delta_D > 0)
  
  if (nrow(delta_non_nul) == 0) {
    warning("Aucune variation non nulle.")
    return(tibble::tibble())
  }
  
  diagnostic <- delta_non_nul %>%
    dplyr::group_by(traitement) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      max_graphe = purrr::map_dbl(
        data,
        ~ stats::quantile(.x$abs_delta_D, 0.99, na.rm = TRUE)
      ),
      seuils = purrr::map(
        max_graphe,
        ~ seq(from = 0, to = .x, length.out = 400)
      ),
      table_seuils = purrr::map2(
        data,
        seuils,
        function(df_traitement, grille_seuils) {
          
          purrr::map_dfr(grille_seuils, function(seuil) {
            
            n_switchers <- sum(
              df_traitement$abs_delta_D > seuil,
              na.rm = TRUE
            )
            
            part_switchers <- n_switchers / nrow(df_traitement)
            
            variation_totale <- sum(
              df_traitement$abs_delta_D,
              na.rm = TRUE
            )
            
            variation_conservee <- sum(
              df_traitement$abs_delta_D[df_traitement$abs_delta_D > seuil],
              na.rm = TRUE
            ) / variation_totale
            
            tibble::tibble(
              seuil = seuil,
              n_switchers = n_switchers,
              part_switchers = part_switchers,
              part_variation_conservee = variation_conservee
            )
          })
        }
      )
    ) %>%
    dplyr::select(traitement, table_seuils) %>%
    tidyr::unnest(table_seuils) %>%
    dplyr::ungroup()
  
  diagnostic
}

# -----------------------------------------------------------------------------
# 3. Calcul des variations de traitement
# -----------------------------------------------------------------------------

delta_foret <- preparer_delta_traitement(
  base = base,
  traitement = "pct_foret",
  nom_traitement = "Forêt"
)

delta_lisiere <- preparer_delta_traitement(
  base = base,
  traitement = "pct_lisiere",
  nom_traitement = "Lisière"
)

delta_traitements <- dplyr::bind_rows(
  delta_foret,
  delta_lisiere
)

delta_non_nul <- delta_traitements %>%
  dplyr::filter(abs_delta_D > 0)

if (nrow(delta_non_nul) == 0) {
  stop(
    "Aucune variation non nulle de forêt ou de lisière.",
    call. = FALSE
  )
}

# Borne commune pour comparer forêt et lisière sur le même axe.
max_graphe_commun <- stats::quantile(
  delta_non_nul$abs_delta_D,
  0.99,
  na.rm = TRUE
)

# -----------------------------------------------------------------------------
# 4. Diagnostics
# -----------------------------------------------------------------------------

diagnostic_seuils <- calculer_diagnostic_seuils(delta_traitements)

write_csv2(
  diagnostic_seuils,
  path("output", "tables", "diagnostic_seuil_switchers.csv")
)

message("Diagnostic du seuil écrit dans output/tables/diagnostic_seuil_switchers.csv")


# -----------------------------------------------------------------------------
# 4.1 Histogrammes séparés de |delta_D| : forêt et lisière
# -----------------------------------------------------------------------------

# Fenêtre d'affichage de l'histogramme
xmax_hist <- 1

# Largeur des bins de l'histogramme
largeur_bin_hist <- 0.005

# Fonction locale pour produire un histogramme par traitement
faire_histogramme_traitement <- function(data, traitement_cible, fichier_sortie) {
  
  data_hist <- data %>%
    dplyr::filter(
      traitement == traitement_cible,
      abs_delta_D <= xmax_hist
    )
  
  if (nrow(data_hist) == 0) {
    warning("Aucune observation pour l'histogramme : ", traitement_cible)
    return(NULL)
  }

  if (traitement_cible == "Forêt") {
    seuil_interet <- tibble::tibble(
      seuil = seuil_foret,
      label = paste0(seuil_foret * 100, " pp")
    )
  } else if (traitement_cible == "Lisière") {
    seuil_interet <- tibble::tibble(
      seuil = seuil_lisiere,
      label = paste0(seuil_lisiere * 100, " pp")
    )
  } else {
    warning("Traitement inconnu pour le seuil d'intérêt : ", traitement_cible)
    seuil_interet <- tibble::tibble(
      seuil = NA,
      label = NA
    )
  }
  
  p_hist <- ggplot2::ggplot(
    data_hist,
    ggplot2::aes(x = abs_delta_D)
  ) +
    ggplot2::geom_histogram(
      binwidth = largeur_bin_hist,
      alpha = 0.75,
      boundary = 0,
      fill = "orange",
      color = "white"
    ) +
    ggplot2::geom_vline(
      data = seuil_interet,
      ggplot2::aes(xintercept = seuil),
      linetype = "dashed",
      color = "red",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = seuil_interet,
      ggplot2::aes(
        x = seuil,
        y = Inf,
        label = label
      ),
      angle = 90,
      vjust = 1.2,
      hjust = 1.05,
      size = 3,
      color = "red",
      inherit.aes = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, xmax_hist),
      clip = "off"
    ) +
    ggplot2::labs(
      title = paste0("Distribution de |ΔD| — ", traitement_cible),
      subtitle = "Zoom sur les premiers seuils",
      x = "|ΔD| entre 2000 et 2012",
      y = "Nombre de communes"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 15, r = 20, b = 10, l = 10)
    )
  
  ggplot2::ggsave(
    filename = path(
      "output",
      "figures",
      fichier_sortie
    ),
    plot = p_hist,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  return(p_hist)
}

p_hist_foret <- faire_histogramme_traitement(
  data = delta_non_nul,
  traitement_cible = "Forêt",
  fichier_sortie = "histogramme_seuil_switchers_foret.png"
)

p_hist_lisiere <- faire_histogramme_traitement(
  data = delta_non_nul,
  traitement_cible = "Lisière",
  fichier_sortie = "histogramme_seuil_switchers_lisiere.png"
)

message("Histogramme forêt écrit dans output/figures/histogramme_seuil_switchers_foret.png")
message("Histogramme lisière écrit dans output/figures/histogramme_seuil_switchers_lisiere.png")