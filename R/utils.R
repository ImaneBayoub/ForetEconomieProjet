# -----------------------------------------------------------------------------
# Utility functions
# -----------------------------------------------------------------------------

standardise_id <- function(x) {
  # INSEE municipal codes can contain leading zeros. Store them as 5-char strings.
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.0$", "")
  stringr::str_pad(x, width = 5, side = "left", pad = "0")
}

safe_log <- function(x) {
  # Log transform that avoids -Inf when a few zeros are present.
  # Observations with non-positive values are set to NA and dropped later.
  ifelse(is.na(x) | x <= 0, NA_real_, log(x))
}

check_required_cols <- function(data, cols, data_name = deparse(substitute(data))) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      "Colonnes manquantes dans ", data_name, " : ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_table_auto <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "parquet") return(arrow::read_parquet(file))
  if (ext %in% c("csv", "txt")) return(readr::read_csv(file, show_col_types = FALSE))
  stop("Format non pris en charge : ", file, call. = FALSE)
}

write_csv2 <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, file)
  invisible(file)
}

write_parquet2 <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, file)
  invisible(file)
}

message_step <- function(...) {
  message("\n", paste0("--- ", paste0(...), " ---"))
}
