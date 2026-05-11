# =============================================================================
# 04_superficies_cultures.R
# Construire les superficies communales par grandes catégories de cultures
# à partir du fichier Agreste FDS_G_1013_2010
# =============================================================================

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Construction des superficies communales par culture en 2010")

# -----------------------------------------------------------------------------
# 1. Charger le fichier
# -----------------------------------------------------------------------------
fichier_entree <- path("data", "raw", "agreste", "FDS_G_1013_2010.txt")

df <- readr::read_delim(
  file = fichier_entree,
  delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

# -----------------------------------------------------------------------------
# 2. Garder uniquement le niveau commune
# -----------------------------------------------------------------------------

df <- df %>%
  dplyr::filter(COM != "............")

# -----------------------------------------------------------------------------
# 3. Garder uniquement les superficies en hectares
# -----------------------------------------------------------------------------

df <- df %>%
  dplyr::filter(G_1013_LIB_DIM4 == "Superficie correspondante (hectares)")

# -----------------------------------------------------------------------------
# 4. Garder les colonnes utiles
# -----------------------------------------------------------------------------

df <- df %>%
  dplyr::select(
    ANNREF,
    FRDOM,
    REGION,
    DEP,
    COM,
    G_1013_MOD_DIM2,
    G_1013_LIB_DIM2,
    VALEUR
  )

# -----------------------------------------------------------------------------
# 5. Renommer les colonnes
# -----------------------------------------------------------------------------

df <- df %>%
  dplyr::rename(
    annee = ANNREF,
    frdom = FRDOM,
    region = REGION,
    dep = DEP,
    com = COM,
    code_culture = G_1013_MOD_DIM2,
    culture = G_1013_LIB_DIM2,
    surface_ha = VALEUR
  )

# -----------------------------------------------------------------------------
# 6. Nettoyage des types
# -----------------------------------------------------------------------------

df <- df %>%
  dplyr::mutate(
    annee = as.integer(annee),
    com = stringr::str_pad(
      stringr::str_trim(as.character(com)),
      width = 5,
      side = "left",
      pad = "0"
    ),
    culture = stringr::str_trim(as.character(culture)),
    surface_ha = readr::parse_number(
      as.character(surface_ha),
      locale = readr::locale(decimal_mark = ".")
    )
  )

# -----------------------------------------------------------------------------
# 7. Garder uniquement les grandes catégories
# -----------------------------------------------------------------------------

grandes_categories <- c(
  "Cultures industrielles",
  "Fourrages et superficies toujours en herbe",
  "Pommes de terre et tubercules",
  "Légumes frais, fraises, melons",
  "Cultures permanentes entretenues",
  "Vignes",
  "Céréales",
  "Oléagineux, protéagineux, plantes à fibres  (Total)",
  "Légumes secs",
  "Fleurs et plantes ornementales",
  "Jachères"
)

df <- df %>%
  dplyr::filter(culture %in% grandes_categories)

# -----------------------------------------------------------------------------
# 8. Vérifications
# -----------------------------------------------------------------------------

message("Cultures conservées :")
print(sort(unique(df$culture)))
message("")

message("Nombre de lignes après filtrage : ", nrow(df))
message("Nombre de communes : ", dplyr::n_distinct(df$com))
message("")

lignes_sous_categories <- df %>%
  dplyr::filter(stringr::str_starts(culture, "_"))

message("Nb lignes commençant par '_' : ", nrow(lignes_sous_categories))
message("")

# -----------------------------------------------------------------------------
# 9. Pivot : une ligne par commune, une colonne par grande catégorie
# -----------------------------------------------------------------------------

df_large <- df %>%
  dplyr::group_by(
    annee,
    frdom,
    region,
    dep,
    com,
    culture
  ) %>%
  dplyr::summarise(
    surface_ha = sum(surface_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = culture,
    values_from = surface_ha,
    values_fill = 0
  )

# -----------------------------------------------------------------------------
# 10. Sauvegarder le résultat
# -----------------------------------------------------------------------------

write_csv2(df_large, path("data", "interim", "superficies_communes_2010.csv"))
write_parquet2(df_large, path("data", "interim", "superficies_communes_2010.parquet"))

# -----------------------------------------------------------------------------
# 11. Aperçu
# -----------------------------------------------------------------------------

message("Table créée avec succès : ", path("data", "interim", "superficies_communes_2010.parquet"))
message("Dimensions : ", paste(dim(df_large), collapse = " x "))
