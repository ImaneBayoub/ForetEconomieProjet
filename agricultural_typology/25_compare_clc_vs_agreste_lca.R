suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
  library(ggplot2)
})

# ============================================================
# 1) Chemins
# ============================================================

# Sortie de ta méthode CLC
# Adapte si tu veux utiliser 2012 au lieu de 2018
clc_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/carte_france_surface_agricole_clc2018_communes.csv"

# Sortie de ta LCA commune sur 2000-2010
lca_classes_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_2000_2010_classes.csv"

# Fond de carte
gpkg_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"
gpkg_layer <- "COMMUNE"

# Sorties
out_compare_csv <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/compare_labels_clc_vs_agreste.csv"
out_confusion_3_csv <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/confusion_clc_vs_agreste_3modalites.csv"
out_confusion_2_csv <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/confusion_clc_vs_agreste_binaire.csv"
out_summary_csv <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/compare_labels_clc_vs_agreste_summary.csv"

out_map_3 <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/map_compare_clc_vs_agreste_3modalites.png"
out_map_2 <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/map_compare_clc_vs_agreste_binaire.png"

# ============================================================
# 2) Paramètres
# ============================================================

# seuil minimal pour considérer qu'une commune a assez d'agriculture côté CLC
min_part_agri_clc <- 0.05

# seuil minimal de dominance pour ne pas classer "par défaut"
# ex: si annual share = 0.52 et perennial share = 0.48, on peut appeler ça mixte
dominance_margin <- 0.10

# année Agreste/LCA à utiliser pour la comparaison
# 2010 paraît le plus naturel avec ta base Agreste
agreste_year <- 2010

# Regroupement LCA que tu proposais
lca_perenne_classes <- c(4, 6)
lca_non_perenne_classes <- c(1, 3, 5)
lca_mixte_classes <- c(2)

# ============================================================
# 3) Vérifications
# ============================================================

stopifnot(file.exists(clc_path))
stopifnot(file.exists(lca_classes_path))
stopifnot(file.exists(gpkg_path))

# ============================================================
# 4) Charger CLC
# ============================================================

clc <- read_csv(clc_path, show_col_types = FALSE) %>%
  mutate(
    code_commune = str_pad(str_trim(as.character(code_commune)), width = 5, side = "left", pad = "0")
  )

required_clc <- c("code_commune", "part_agri_commune", "surf_annual_ha", "surf_perennial_ha")
missing_clc <- setdiff(required_clc, names(clc))
if (length(missing_clc) > 0) {
  stop("Colonnes manquantes dans le fichier CLC : ", paste(missing_clc, collapse = ", "))
}

clc <- clc %>%
  mutate(
    surf_ann_per_ha = surf_annual_ha + surf_perennial_ha,
    share_annual = if_else(surf_ann_per_ha > 0, surf_annual_ha / surf_ann_per_ha, NA_real_),
    share_perennial = if_else(surf_ann_per_ha > 0, surf_perennial_ha / surf_ann_per_ha, NA_real_),
    score_type = if_else(
      surf_ann_per_ha > 0,
      (surf_annual_ha - surf_perennial_ha) / surf_ann_per_ha,
      NA_real_
    ),

    # Label à 3 modalités côté CLC
    label_clc_3 = case_when(
      is.na(part_agri_commune) | part_agri_commune < min_part_agri_clc ~ "mixte_ou_indetermine",
      is.na(score_type) ~ "mixte_ou_indetermine",
      score_type >= dominance_margin ~ "non_perenne",
      score_type <= -dominance_margin ~ "perenne",
      TRUE ~ "mixte_ou_indetermine"
    ),

    # Label binaire côté CLC
    label_clc_2 = case_when(
      is.na(part_agri_commune) | part_agri_commune < min_part_agri_clc ~ NA_character_,
      is.na(score_type) ~ NA_character_,
      score_type < 0 ~ "perenne",
      score_type >= 0 ~ "non_perenne"
    )
  ) %>%
  select(
    code_commune,
    part_agri_commune,
    surf_annual_ha,
    surf_perennial_ha,
    share_annual,
    share_perennial,
    score_type,
    label_clc_3,
    label_clc_2
  )

# ============================================================
# 5) Charger LCA / Agreste
# ============================================================

lca <- read_csv(lca_classes_path, show_col_types = FALSE) %>%
  mutate(
    insee = str_pad(str_trim(as.character(insee)), width = 5, side = "left", pad = "0"),
    classe = as.integer(classe)
  )

required_lca <- c("insee", "annee", "classe")
missing_lca <- setdiff(required_lca, names(lca))
if (length(missing_lca) > 0) {
  stop("Colonnes manquantes dans le fichier LCA : ", paste(missing_lca, collapse = ", "))
}

lca_year <- lca %>%
  filter(annee == agreste_year) %>%
  mutate(
    label_agreste_3 = case_when(
      classe %in% lca_perenne_classes ~ "perenne",
      classe %in% lca_non_perenne_classes ~ "non_perenne",
      classe %in% lca_mixte_classes ~ "mixte_ou_indetermine",
      TRUE ~ NA_character_
    ),
    label_agreste_2 = case_when(
      classe %in% lca_perenne_classes ~ "perenne",
      classe %in% lca_non_perenne_classes ~ "non_perenne",
      classe %in% lca_mixte_classes ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    code_commune = insee,
    annee,
    classe,
    prob_max,
    uncertainty,
    label_agreste_3,
    label_agreste_2
  )

# ============================================================
# 6) Jointure
# ============================================================

compare_df <- lca_year %>%
  inner_join(clc, by = "code_commune") %>%
  mutate(
    agree_3 = case_when(
      is.na(label_agreste_3) | is.na(label_clc_3) ~ NA,
      label_agreste_3 == label_clc_3 ~ TRUE,
      TRUE ~ FALSE
    ),
    agree_2 = case_when(
      is.na(label_agreste_2) | is.na(label_clc_2) ~ NA,
      label_agreste_2 == label_clc_2 ~ TRUE,
      TRUE ~ FALSE
    ),

    compare_3 = case_when(
      is.na(label_agreste_3) | is.na(label_clc_3) ~ "indetermine",
      label_agreste_3 == label_clc_3 ~ "accord",
      TRUE ~ "desaccord"
    ),
    compare_2 = case_when(
      is.na(label_agreste_2) | is.na(label_clc_2) ~ "indetermine",
      label_agreste_2 == label_clc_2 ~ "accord",
      TRUE ~ "desaccord"
    )
  )

# ============================================================
# 7) Fonctions utiles
# ============================================================

compute_kappa <- function(tab) {
  n <- sum(tab)
  if (n == 0) return(NA_real_)

  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / (n^2)

  if (isTRUE(all.equal(1 - pe, 0))) return(NA_real_)
  (po - pe) / (1 - pe)
}

# ============================================================
# 8) Confusions
# ============================================================

# 3 modalités
conf_3 <- compare_df %>%
  filter(!is.na(label_agreste_3), !is.na(label_clc_3)) %>%
  count(label_agreste_3, label_clc_3) %>%
  tidyr::pivot_wider(
    names_from = label_clc_3,
    values_from = n,
    values_fill = 0
  )

tab_3 <- with(
  compare_df %>% filter(!is.na(label_agreste_3), !is.na(label_clc_3)),
  table(label_agreste_3, label_clc_3)
)

write.csv(as.data.frame.matrix(tab_3), out_confusion_3_csv, row.names = TRUE)

# binaire
tab_2 <- with(
  compare_df %>% filter(!is.na(label_agreste_2), !is.na(label_clc_2)),
  table(label_agreste_2, label_clc_2)
)

write.csv(as.data.frame.matrix(tab_2), out_confusion_2_csv, row.names = TRUE)

# ============================================================
# 9) Résumés
# ============================================================

summary_df <- tibble(
  n_jointes = nrow(compare_df),
  n_compare_3 = sum(!is.na(compare_df$label_agreste_3) & !is.na(compare_df$label_clc_3)),
  n_compare_2 = sum(!is.na(compare_df$label_agreste_2) & !is.na(compare_df$label_clc_2)),
  part_accord_3_pct = 100 * mean(compare_df$agree_3, na.rm = TRUE),
  part_accord_2_pct = 100 * mean(compare_df$agree_2, na.rm = TRUE),
  kappa_3 = compute_kappa(tab_3),
  kappa_2 = compute_kappa(tab_2)
)

write_csv(summary_df, out_summary_csv)

# ============================================================
# 10) Export principal
# ============================================================

write_csv(compare_df, out_compare_csv)

# ============================================================
# 11) Carte des accords / désaccords
# ============================================================

communes_sf <- st_read(gpkg_path, layer = gpkg_layer, quiet = TRUE) %>%
  mutate(
    code_commune = str_pad(str_trim(as.character(code_insee)), width = 5, side = "left", pad = "0")
  )

map_sf <- communes_sf %>%
  left_join(
    compare_df %>%
      select(code_commune, compare_3, compare_2, label_agreste_3, label_clc_3, label_agreste_2, label_clc_2),
    by = "code_commune"
  )

p_map_3 <- ggplot(map_sf) +
  geom_sf(aes(fill = compare_3), color = NA) +
  scale_fill_manual(
    values = c(
      "accord" = "#1B9E77",
      "desaccord" = "#D95F02",
      "indetermine" = "grey85"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Accord entre CLC et Agreste/LCA",
    subtitle = "Comparaison a 3 modalites : perenne / non perenne / mixte-indetermine",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

p_map_2 <- ggplot(map_sf) +
  geom_sf(aes(fill = compare_2), color = NA) +
  scale_fill_manual(
    values = c(
      "accord" = "#1B9E77",
      "desaccord" = "#D95F02",
      "indetermine" = "grey85"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Accord entre CLC et Agreste/LCA",
    subtitle = "Comparaison binaire : perenne / non perenne",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave(out_map_3, p_map_3, width = 10, height = 12, dpi = 300, bg = "white")
ggsave(out_map_2, p_map_2, width = 10, height = 12, dpi = 300, bg = "white")

# ============================================================
# 12) Console
# ============================================================

cat("\nRésumé global\n")
print(summary_df)

cat("\nTable de confusion - 3 modalités\n")
print(tab_3)

cat("\nTable de confusion - binaire\n")
print(tab_2)

cat("\nFichiers exportés :\n")
cat(out_compare_csv, "\n")
cat(out_confusion_3_csv, "\n")
cat(out_confusion_2_csv, "\n")
cat(out_summary_csv, "\n")
cat(out_map_3, "\n")
cat(out_map_2, "\n")