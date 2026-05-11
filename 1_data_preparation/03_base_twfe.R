# -----------------------------------------------------------------------------
# 03_base_twfe.R
# Créer la base d'analyse unique utilisée par tous les scripts descriptifs
# et les scripts d'estimation
# -----------------------------------------------------------------------------
# Sorties :
#   data/processed/twfe_data.csv
#   data/processed/twfe_data.parquet
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Construction de la base d'analyse unique pour les TWFE")

agri_panel <- arrow::read_parquet(path("data", "interim", "agri_panel.parquet"))

clc_indicateurs <- readr::read_csv(
  path("data", "interim", "clc_commune_indicateurs.csv"),
  show_col_types = FALSE
)

clc_long <- clc_indicateurs %>%
  select(
    id = insee,
    nom = nom,
    pixels_total_1990, pixels_total_2000, pixels_total_2012,
    pct_agri_1990, pct_agri_2000, pct_agri_2012,
    pct_foret_1990, pct_foret_2000, pct_foret_2012,
    pct_lisiere_1990, pct_lisiere_2000, pct_lisiere_2012
  ) %>%
  tidyr::pivot_longer(
    cols = -c(id, nom),
    names_to = c(".value", "clc_annee"),
    names_pattern = "(.*)_(1990|2000|2012)"
  ) %>%
  mutate(
    clc_annee = as.integer(clc_annee),
    periode = case_when(
      clc_annee == 1990 ~ 1L,
      clc_annee == 2000 ~ 2L,
      clc_annee == 2012 ~ 3L
    )
  )

agri_panel <- agri_panel %>%
  mutate(
    periode = case_when(
      agri_annee == 1988 ~ 1L,
      agri_annee == 2000 ~ 2L,
      agri_annee == 2010 ~ 3L
    )
  )

twfe_data <- agri_panel %>%
  inner_join(clc_long, by = c("id", "periode")) %>%
  filter(
    !is.na(productivite),
    !is.na(pct_foret),
    !is.na(pct_lisiere)
  )


periode_labels <- c(
  "1" = "1988/1990",
  "2" = "2000",
  "3" = "2010/2012"
)
twfe_data <- twfe_data %>%
  mutate(libelle_periode = periode_labels[as.character(periode)])


write_csv2(twfe_data, path("data", "processed", "twfe_data.csv"))
write_parquet2(twfe_data, path("data", "processed", "twfe_data.parquet"))

message("Base d'analyse écrite dans data/processed/twfe_data.csv")
