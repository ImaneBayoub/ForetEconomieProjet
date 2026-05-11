# -----------------------------------------------------------------------------
# Project paths
# -----------------------------------------------------------------------------
# All scripts should use these paths instead of hard-coded absolute paths.
# This makes the project portable across computers.

find_project_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  cur <- wd
  repeat {
    if (file.exists(file.path(cur, "README.md")) || file.exists(file.path(cur, "main.R"))) {
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) return(wd)
    cur <- parent
  }
}

PROJECT_ROOT <- find_project_root()

path <- function(...) file.path(PROJECT_ROOT, ...)

dir_create <- function(...) {
  x <- path(...)
  if (!dir.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
  invisible(x)
}

# Standard folders used by the pipeline
dir_create("data", "raw")
dir_create("data", "processed")
dir_create("output", "tables")
dir_create("output", "figures")
dir_create("output", "logs")
