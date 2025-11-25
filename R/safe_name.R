#' Make a safe filename
#'
#' Ensures file names contain only safe ASCII characters.
#'
#' @param x A character string to clean.
#'
#' @return A cleaned filename string.
#' @export
safe_name <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", x)
}
