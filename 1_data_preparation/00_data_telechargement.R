# =============================================================================
# 00_data_telechargement.R
# Télécharge les données brutes depuis Zenodo si elles ne sont pas déjà présentes.
# =============================================================================

source("R/paths.R")

download_file_if_missing <- function(url, destfile, overwrite = FALSE) {
  if (file.exists(destfile) && !overwrite) {
    message("Déjà présent : ", destfile)
    return(invisible(destfile))
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  message("Téléchargement : ", url)

  utils::download.file(
    url = url,
    destfile = destfile,
    mode = "wb",
    method = "auto"
  )

  invisible(destfile)
}

unzip_if_missing <- function(zipfile, exdir, marker_file, overwrite = FALSE) {
  if (file.exists(marker_file) && !overwrite) {
    message("Données déjà extraites : ", exdir)
    return(invisible(exdir))
  }

  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  message("Décompression : ", zipfile)

  utils::unzip(
    zipfile = zipfile,
    exdir = exdir,
    overwrite = overwrite
  )

  invisible(exdir)
}

zenodo_url <- "https://zenodo.org/records/20114515/files/foret_agri_data_raw.zip?download=1"

zip_path <- path("data", "raw", "foret_agri_data_raw.zip")

download_file_if_missing(
  url = zenodo_url,
  destfile = zip_path
)

unzip_if_missing(
  zipfile = zip_path,
  exdir = path("data", "raw"),
  marker_file = path("data", "raw", "clc", "U2018_CLC2012_V2020_20u1.tif")
)

if (!file.exists(marker_file)) {
  stop(
    "Décompression terminée, mais fichier attendu introuvable : ",
    marker_file,
    "\nVérifie la structure interne du zip."
  )
}
