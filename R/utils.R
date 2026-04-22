utils::globalVariables("complete.cases")
#' Internal helper: simple station name safe-ify
safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_\\-]", "_", x)
  x
}
