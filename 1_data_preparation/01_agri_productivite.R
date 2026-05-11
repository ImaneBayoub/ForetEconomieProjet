# -----------------------------------------------------------------------------
# 01_agri_productivite_.R
# Construire une base Agreste nettoyée de productivité agricole
# -----------------------------------------------------------------------------
# Sorties :
#   data/interim/agri_panel.parquet
#   data/interim/agri1988.parquet
#   data/interim/agri2000.parquet
#   data/interim/agri2010.parquet
#
# Fichiers bruts attendus dans data/raw/agreste/ :
#   FDS_G_2047_1988.txt
#   FDS_G_2047_2000.txt
#   FDS_G_2047_2010.txt
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Préparation des données de productivité agricole à partir d'Agreste")

AGRI_COLUMN_MAP <- list(
  id = c("id", "insee", "CODE_GEO", "code_geo", "COMMUNE", "V2"),
  variable = c("variable", "LIBELLE", "libelle", "NOM_VAR", "V4"),
  valeur = c("value", "VALEUR", "VALEUR_BRUTE", "valeur", "V5", "V6")
)

find_col <- function(data, candidates, required = TRUE) {
  hit <- intersect(candidates, names(data))
  if (length(hit) > 0) return(hit[[1]])
  if (required) stop("Impossible de trouver une colonne parmi : ", paste(candidates, collapse = ", "), call. = FALSE)
  NA_character_
}

read_agreste_raw <- function(year) {
  file <- path("data", "raw", "agreste", paste0("FDS_G_2047_", year, ".txt"))
  if (!file.exists(file)) {
    # Also allow already-converted parquet files, for compatibility with older work.
    parquet_file <- path("data", "raw", "agreste", paste0("base_agri", year, ".parquet"))
    if (file.exists(parquet_file)) return(arrow::read_parquet(parquet_file))
    stop("Fichier Agreste introuvable pour ", year, ": ", file, call. = FALSE)
  }

  data.table::fread(file, sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE) %>%
    tibble::as_tibble()
}

clean_agreste_year <- function(year) {
  
  message("Nettoyage Agreste : ", year)
  
  raw_path <- path(
    "data", "raw", "agreste",
    paste0("FDS_G_2047_", year, ".txt")
  )

  if (!file.exists(raw_path)) {
    stop("Fichier introuvable : ", raw_path)
  }
  
  raw <- data.table::fread(
    raw_path,
    sep = ";",
    encoding = "Latin-1",
    fill = TRUE,
    header = FALSE,
    colClasses = "character"
  )
  
  cleaned <- raw %>%
    dplyr::filter(
      V1 != "NOM",                 # retire la ligne d'en-tête
      !is.na(V7),
      V7 != "............",        # garde seulement les lignes communales
      !is.na(V16),
      !is.na(V17)
    ) %>%
    dplyr::mutate(
      dep = stringr::str_trim(V6),
      com = stringr::str_trim(V7),
      
      # Si V7 contient déjà un code commune complet, on le garde.
      # Sinon, on reconstruit l'INSEE avec DEP + COM.
      id = dplyr::case_when(
        stringr::str_detect(com, "^[0-9]{5}$") ~ com,
        stringr::str_detect(dep, "^[0-9]{2,3}$") &
          stringr::str_detect(com, "^[0-9]{3}$") ~ paste0(dep, com),
        TRUE ~ com
      ),
      
      indicateur = dplyr::case_when(
        stringr::str_detect(
          stringr::str_to_lower(V16),
          "superficie agricole"
        ) ~ "superficie",
        
        stringr::str_detect(
          stringr::str_to_lower(V16),
          "production brute standard"
        ) ~ "production",
        
        TRUE ~ NA_character_
      ),
      
      valeur = readr::parse_number(
        V17,
        locale = readr::locale(decimal_mark = ".")
      )
    ) %>%
    dplyr::filter(
      !is.na(indicateur),
      !is.na(valeur)
    ) %>%
    dplyr::select(
      id,
      agri_annee = V2,
      indicateur,
      valeur
    ) %>%
    dplyr::mutate(
      agri_annee = as.integer(agri_annee)
    ) %>%
    dplyr::group_by(id, agri_annee, indicateur) %>%
    dplyr::summarise(
      valeur = sum(valeur, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = indicateur,
      values_from = valeur
    )
  
  if (!"production" %in% names(cleaned)) {
    cleaned$production <- NA_real_
  }
  
  if (!"superficie" %in% names(cleaned)) {
    cleaned$superficie <- NA_real_
  }
  
  cleaned <- cleaned %>%
    dplyr::mutate(
      productivite = dplyr::if_else(
        !is.na(superficie) & superficie > 0,
        production / superficie,
        NA_real_
      )
    )
  
  return(cleaned)
}

years <- c(1988, 2000, 2010)
agri_list <- purrr::set_names(years) %>% purrr::map(clean_agreste_year)
agri_panel <- dplyr::bind_rows(agri_list)
write_parquet2(agri_panel, path("data", "interim", "agri_panel.parquet"))
write_csv2(agri_panel, path("data", "interim", "agri_panel.csv"))

message("Panel Agreste écrit dans data/interim/agri_panel.parquet")
