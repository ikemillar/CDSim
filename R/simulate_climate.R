#' Simulate monthly climate time series for stations
#'
#' Simulate monthly Tmin, Tmax, monthly total rainfall (Sum.Rf) and mean daily rainfall (Avg.Rf)
#' for each station across a year range.
#'
#' @param stations data.frame from create_stations() (Station, LON, LAT)
#' @param start_year integer (e.g., 1981)
#' @param end_year integer (e.g., 2020)
#' @param seed optional numeric seed
#' @return A tidy data.frame with one row per station × month containing:
#'   Station, LON, LAT, Year, Month, Date, Avg.Tn, Avg.Tx, Sum.Rf, Avg.Rf
#' @seealso [write_station_csv()], [write_station_netcdf()]
#' @family IO Functions
#' @details This function simulates synthetic time-series climate data based on...
#' @examples
#' st <- create_stations(n = 3, seed = 1)
#' sim <- simulate_climate_series(st, 1981, 1982, seed = 42)
#' head(sim)
#' @export
simulate_climate_series <- function(stations, start_year = 1981, end_year = 2020, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(is.data.frame(stations))
  stopifnot(all(c("Station","LON","LAT") %in% names(stations)))
  dates <- seq(as.Date(sprintf("%04d-01-15", start_year)),
               as.Date(sprintf("%04d-12-15", end_year)), by = "month")
  monthly <- as.integer(format(dates, "%m"))

  out <- lapply(seq_len(nrow(stations)), function(i) {
    s <- stations[i, , drop = FALSE]
    # simple climatology baseline from latitude
    base_mean <- 25 - 0.1 * s$LAT
    amp <- 5 + 0.05 * abs(s$LAT - mean(stations$LAT))
    tmean <- base_mean + amp * sin(2 * pi * monthly / 12) + rnorm(length(dates), 0, 0.5)
    Tmin <- tmean - abs(rnorm(length(dates), mean = 6, sd = 1))
    Tmax <- tmean + abs(rnorm(length(dates), mean = 6, sd = 1))
    wet_factor <- ifelse(monthly %in% 4:10, 1.8, 0.4)
    r_mean <- pmax(1, 60 * wet_factor * (1 + 0.2 * sin(2*pi*(monthly-2)/12)))
    r_mean <- r_mean * (1 + rnorm(1, 0, 0.15))
    SumRf <- rgamma(length(dates), shape = 2, scale = r_mean / 2)
    AvgRf <- SumRf / lubridate::days_in_month(dates)
    data.frame(
      Station = s$Station,
      LON = s$LON,
      LAT = s$LAT,
      Year = as.integer(format(dates, "%Y")),
      Month = as.integer(format(dates, "%m")),
      Date = dates,
      Avg.Tn = round(Tmin, 1),
      Avg.Tx = round(Tmax, 1),
      Sum.Rf = round(SumRf, 1),
      Avg.Rf = round(AvgRf, 2),
      stringsAsFactors = FALSE
    )
  })
  simdf <- do.call(rbind, out)
  rownames(simdf) <- NULL
  return(simdf)
}
