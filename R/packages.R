# -----------------------------------------------------------------------------
# Packages utilisés dans le projet
# -----------------------------------------------------------------------------
# Ce fichier centralise le chargement des packages.
# Si un package est manquant, il est seulement signalé : aucune installation
# automatique n'est lancée, et le script ne s'arrête pas.
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "forcats",
  "ggplot2", "arrow", "data.table", "fixest", "broom", "sandwich", "lmtest",
  "cluster", "terra", "sf", "poLCA", "mgcv", "furrr", "future"
)

# Supprimer les doublons éventuels
required_packages <- unique(required_packages)

available_packages <- required_packages[
  vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

missing_packages <- setdiff(required_packages, available_packages)

if (length(missing_packages) > 0) {
  warning(
    "Packages manquants non chargés : ",
    paste(missing_packages, collapse = ", "),
    "\nCertaines parties du pipeline peuvent ne pas fonctionner si elles dépendent de ces packages",
    "\n(à l'exception de furrr et de future, qui sont seulement optionnels)."
    call. = FALSE
  )
}

# Chargement uniquement des packages disponibles
invisible(
  lapply(
    available_packages,
    function(pkg) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
    }
  )
)

message(
  "Packages chargés : ",
  paste(available_packages, collapse = ", ")
)

if (length(missing_packages) == 0) {
  message("Tous les packages nécessaires sont disponibles et chargés.")
}