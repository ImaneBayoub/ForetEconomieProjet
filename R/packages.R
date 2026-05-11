# -----------------------------------------------------------------------------
# Packages utilisés dans le projet
# -----------------------------------------------------------------------------
# Ce fichier centralise l'installation et le chargement des packages.
# Si un package est manquant, il est automatiquement installé depuis CRAN,
# puis chargé.
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "forcats",
  "ggplot2", "arrow", "data.table", "fixest", "broom", "sandwich", "lmtest",
  "cluster", "terra", "sf", "poLCA", "sandwich"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  message(
    "Packages manquants détectés : ",
    paste(missing_packages, collapse = ", ")
  )
  
  message("Installation des packages manquants depuis CRAN...")
  
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

# Vérification après installation
still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing) > 0) {
  stop(
    "Certains packages n'ont pas pu être installés : ",
    paste(still_missing, collapse = ", "),
    "\nInstallez-les manuellement puis relancez le script.",
    call. = FALSE
  )
}

invisible(
  lapply(required_packages, library, character.only = TRUE)
)

message("Tous les packages nécessaires sont installés et chargés.")