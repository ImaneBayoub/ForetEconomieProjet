# ============================================================
# 14_prep_lisiere.R
# Calcule la lisière forêt/agricole pour chaque commune
# et remplace prop_foret_alt par la lisière comme traitement.
# ============================================================

library(dplyr)
library(tidyr)

# --- 1. Charger les indicateurs déjà calculés (1990, 2012) ---
indics <- read.csv("out_communes/indicateurs_communes_clc_1990_2012.csv",
                    stringsAsFactors = FALSE)

# Normaliser la lisière par la surface communale
indics <- indics %>%
  mutate(
    # Lisière = pixels agri adjacents à forêt (mesure côté agri)
    # Normalisée par le nombre total de pixels de la commune
    lisiere_pct_1990 = agri_adj_foret_1990 / pixels_total_1990,
    lisiere_pct_2012 = agri_adj_foret_2012 / pixels_total_2012,
    
    # Lisière en % des pixels agricoles (intensité de l'interface)
    lisiere_pct_agri_1990 = ifelse(agri_1990 > 0,
      agri_adj_foret_1990 / agri_1990, 0),
    lisiere_pct_agri_2012 = ifelse(agri_2012 > 0,
      agri_adj_foret_2012 / agri_2012, 0),
    
    # Lisière absolue (en nombre de pixels, utile avec FE)
    lisiere_px_1990 = agri_adj_foret_1990,
    lisiere_px_2012 = agri_adj_foret_2012
  )

# --- 2. Mettre en format long (id, time, valeur) ---
lisiere_long <- indics %>%
  select(insee, matches("lisiere_")) %>%
  pivot_longer(
    cols = -insee,
    names_to = c("var", "annee"),
    names_pattern = "(.+)_(\\d{4})$",
    values_to = "val"
  ) %>%
  pivot_wider(
    names_from = var,
    values_from = val
  ) %>%
  mutate(
    time = case_when(
      annee == "1990" ~ 1,
      annee == "2012" ~ 3
    )
  ) %>%
  filter(!is.na(time))

# --- 3. Fusionner avec les données TWFE ---
twfe <- read.csv("data/twfe_data.csv", stringsAsFactors = FALSE)
twfe$id <- as.character(twfe$id)

twfe_lisiere <- twfe %>%
  left_join(lisiere_long, by = c("id" = "insee", "time"))

cat("Obs avec lisiere_pct :", sum(!is.na(twfe_lisiere$lisiere_pct)), "\n")
cat("Obs sans lisiere_pct :", sum(is.na(twfe_lisiere$lisiere_pct)), "\n")
cat("Dimensions twfe_lisiere :", nrow(twfe_lisiere), "x", ncol(twfe_lisiere), "\n")

# --- 4. Sauvegarder ---
write.csv(twfe_lisiere, "data/twfe_data_lisiere.csv", row.names = FALSE)
cat("-> Sauvegardé : data/twfe_data_lisiere.csv\n")

# --- 5. Quand le raster CLC 2000 sera disponible ---
# Décommenter et adapter le chemin dans le script 14b :
# 
# Source : "data/Results/CLC2000/U2006_CLC2000_V2020_20u1.tif"
# 
# La procédure sera :
#   1. Charger le raster CLC 2000
#   2. Pour chaque commune, compter les pixels agri adjacents à forêt
#   3. Ajouter une ligne time=2 dans lisiere_long
#   4. Re-fusionner avec twfe

# ============================================================
# Nouveau traitement :
#   D = lisiere_pct   (au lieu de prop_foret_alt)
# ============================================================
