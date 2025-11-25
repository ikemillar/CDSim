#' Create or load station metadata
#'
#' Create a station metadata table (Station, LON, LAT) either by:
#' - loading from a CSV file,
#' - accepting an existing data.frame,
#' - or auto-generating synthetic stations in a bounding box.
#'
#' @param source Path to CSV file OR a data.frame with Station/LON/LAT OR NULL (to generate synthetic).
#' @param n Integer number of stations to generate when source = NULL. Default 10.
#' @param bbox numeric vector c(min_lon, max_lon, min_lat, max_lat). Default ~ Ghana bounding box.
#' @param seed Optional numeric to make generation reproducible.
#' @return A data.frame with columns Station, LON, LAT.
#' @examples
#' create_stations(n = 5, seed = 42)
#' create_stations(data.frame(Station="A", LON=0, LAT=5))
#' @export
#' @importFrom stats runif
#' @importFrom readr read_csv

create_stations <- function(source = NULL, n = 10, bbox = c(-3.5, 1.5, 4.5, 11.5), seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (is.character(source)) {
    if (!file.exists(source)) stop("File not found: ", source)
    message("Loading station list from file: ", source)
    df <- readr::read_csv(source, show_col_types = FALSE)
  } else if (is.data.frame(source)) {
    message("Using station metadata from supplied data.frame")
    df <- source
  } else if (is.null(source)) {
    message("Generating synthetic station network...")
    if (length(bbox) != 4 || !is.numeric(bbox)) {
      stop("bbox must be numeric vector: c(min_lon, max_lon, min_lat, max_lat)")
    }
    df <- data.frame(
      Station = paste0("Station_", seq_len(n)),
      LON = runif(n, bbox[1], bbox[2]),
      LAT = runif(n, bbox[3], bbox[4]),
      stringsAsFactors = FALSE
    )
    message("Generated ", n, " synthetic stations within bounding box.")
  } else {
    stop("`source` must be a file path, a data.frame, or NULL.")
  }

  # ensure required cols exist, try to coerce common alternatives
  names(df) <- make.names(names(df))
  if (!all(c("Station", "LON", "LAT") %in% names(df))) {
    stop("Station metadata must contain columns named: Station, LON, LAT")
  }
  df$Station <- as.character(df$Station)
  df$LON <- as.numeric(df$LON)
  df$LAT <- as.numeric(df$LAT)
  rownames(df) <- NULL
  return(df[, c("Station", "LON", "LAT")])
}
