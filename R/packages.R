# -----------------------------------------------------------------------------
# packages.R
# Chargement centralisé des packages du projet
# -----------------------------------------------------------------------------
# Comportement :
#   - Les packages manquants sont signalés mais n'arrêtent pas le script.
#   - furrr et future sont optionnels : leur absence dégrade seulement les
#     performances (pas de parallélisme) sans bloquer le pipeline.
#   - Si furrr et future sont tous les deux chargés, un plan multisession est
#     activé automatiquement (workers = nombre de cœurs logiques - 1, min 1).
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1. Liste des packages
# -----------------------------------------------------------------------------

required_packages <- unique(c(
  # Tidyverse
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "forcats",
  # Visualisation
  "ggplot2",
  # Données / IO
  "arrow", "data.table",
  # Économétrie
  "fixest", "broom", "sandwich", "lmtest", "cluster",
  # Données spatiales
  "terra", "sf",
  # Modélisation
  "poLCA", "mgcv",
  # Parallélisme (optionnels)
  "future", "furrr"
))

packages_optionnels <- c("future", "furrr")

# -----------------------------------------------------------------------------
# 2. Détection des packages disponibles / manquants
# -----------------------------------------------------------------------------

est_disponible <- vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

available_packages <- required_packages[est_disponible]
missing_packages   <- required_packages[!est_disponible]

missing_requis     <- setdiff(missing_packages, packages_optionnels)
missing_optionnels <- intersect(missing_packages, packages_optionnels)

# -----------------------------------------------------------------------------
# 3. Avertissements
# -----------------------------------------------------------------------------

if (length(missing_requis) > 0) {
  warning(
    "Packages requis manquants : ",
    paste(missing_requis, collapse = ", "),
    "\nCertaines parties du pipeline ne fonctionneront pas.",
    call. = FALSE
  )
}

if (length(missing_optionnels) > 0) {
  message(
    "[info] Packages optionnels non disponibles : ",
    paste(missing_optionnels, collapse = ", "),
    "\nLe parallélisme sera désactivé ; le pipeline reste fonctionnel."
  )
}

# -----------------------------------------------------------------------------
# 4. Chargement des packages disponibles
# -----------------------------------------------------------------------------

invisible(lapply(
  available_packages,
  function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
))

message("Packages chargés : ", paste(available_packages, collapse = ", "))

if (length(missing_packages) == 0) {
  message("Tous les packages nécessaires sont disponibles et chargés.")
}

# -----------------------------------------------------------------------------
# 5. Mise en place du plan multisession (si future + furrr sont chargés)
# -----------------------------------------------------------------------------

if (all(c("future", "furrr") %in% available_packages)) {

  # Nombre de workers : tous les cœurs logiques moins 1, avec un minimum de 1
  n_workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

  future::plan(future::multisession, workers = n_workers)

  message(sprintf(
    "[parallélisme] Plan multisession activé (%d worker%s).",
    n_workers,
    if (n_workers > 1) "s" else ""
  ))

} else {

  # Garantit un plan séquentiel explicite (évite d'hériter d'un plan parent)
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan(future::sequential)
  }
}