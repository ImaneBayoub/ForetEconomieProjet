# -----------------------------------------------------------------------------
# 02_ajouter_typologie_agricole.R
# Ajouter la typologie agricole issue de la LCA à la base twfe_data
#
# Sortie :
#   data/processed/twfe_data_enrichie.csv/parquet
#   output/tables/lca_typologie.csv
#
# Entrée :
#   data/processed/twfe_data.parquet
#   output/tables/lca_communes_classes.csv
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Ajouter la typologie agricole issue de la LCA à la base twfe_data")

input_file <- path("data", "processed", "twfe_data.parquet")
if (!file.exists(input_file)) {
  stop("Missing twfe_data. Run preparation scripts first.", call. = FALSE)
}

twfe <- arrow::read_parquet(input_file)


lca_file <- path("output", "tables", "lca_communes_classes.csv")

if (is.na(lca_file)) {
  warning("No LCA typology file found. classe_lca/type_lca will be NA.")
  twfe <- twfe %>%
    dplyr::mutate(classe_lca = NA_character_, type_lca = NA_character_)
} else {
  lca <- readr::read_csv(lca_file, show_col_types = FALSE)
  id_col <- intersect(c("id", "insee", "INSEE", "code_insee", "CODGEO"), names(lca))[1]
  class_col <- intersect(c("classe_lca", "classe", "class", "lca_class", "cluster", "profil"), names(lca))[1]
  if (is.na(id_col) || is.na(class_col)) {
    stop("The LCA file must contain an INSEE id column and a class column.", call. = FALSE)
  }

  lca_clean <- lca %>%
    dplyr::transmute(
      id = standardise_id(.data[[id_col]]),
      classe_lca = as.character(.data[[class_col]])
    ) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(
      # Adapt this recoding if the meaning of the original LCA classes changes.
      type_lca = dplyr::case_when(
        classe_lca %in% c("1", "3", "5", "annuel", "annual") ~ "annuel",
        classe_lca %in% c("2", "mixte", "mixed") ~ "mixte",
        classe_lca %in% c("4", "6", "perenne", "pérenne", "perennial") ~ "perenne",
        TRUE ~ NA_character_
      )
    )

  twfe <- twfe %>%
    dplyr::select(-dplyr::any_of(c("classe_lca", "type_lca"))) %>%
    dplyr::left_join(lca_clean, by = "id")
}

write_csv2(twfe, path("data", "processed", "twfe_data_enrichie.csv"))
write_parquet2(twfe, path("data", "processed", "twfe_data_enrichie.parquet"))

summary <- twfe %>%
  dplyr::filter(periode == 2L) %>%
  dplyr::count(type_lca, name = "n_communes")
write_csv2(summary, path("output", "tables", "lca_typologie.csv"))

message("Typologie agricole ajoutée dans : output/tables/lca_typologie.csv")
