#' Simulate monthly climate time series for stations
#'
#' Simulate monthly Tmin, Tmax, monthly total rainfall (Sum.Rf) and mean daily rainfall (Avg.Rf)
#' for each station across a year range.
#'
#' @param stations data.frame from create_stations() (Station, LON, LAT)
#' @param start_year integer (e.g., 1996)
#' @param end_year integer (e.g., 2025)
#' @param seed optional numeric seed
#' @param temp_trend_per_year temperature trend per year (°C/year warming)
#' @param rain_trend_per_year rain trend per year (slight drying trend)
#' @param phi_temp AR(1) persistence
#' @param sd standard deviation of the AR(1) innovation process controlling temperature variability
#' @param Tmin_min minimum value for minimum temperature
#' @param Tmin_max maximum value for minimum temperature
#' @param Tmax_min minimum value for maximum temperature
#' @param Tmax_max maximum value for maximum temperature
#' @return A tidy data.frame with one row per station × month containing:
#'   Station, LON, LAT, Year, Month, Date, Avg.Tn, Avg.Tx, Sum.Rf, Avg.Rf
#' @seealso [write_station_csv()], [write_station_netcdf()]
#' @family IO Functions
#' @details
#' This function generates synthetic monthly climate time series using a
#' stochastic, physically-informed modelling framework. Temperature is modeled
#' as a combination of deterministic seasonality, long-term trend, and
#' stochastic variability. The seasonal component is represented using a
#' sinusoidal function, while temporal persistence is introduced via an
#' autoregressive AR(1) process applied to the innovation term.
#'
#' Minimum temperature (Avg.Tn) is simulated using a truncated normal
#' distribution to enforce physically realistic lower and upper bounds.
#' Maximum temperature (Avg.Tx) is generated using a gamma-distributed
#' perturbation applied to the mean temperature, producing an asymmetric
#' distribution consistent with observed climatological behavior.
#'
#' Rainfall occurrence is modeled using a first-order Markov chain, allowing
#' for realistic wet–dry persistence. Conditional on occurrence, rainfall
#' intensity is drawn from a gamma distribution with seasonally varying mean.
#' A temporal trend term can be applied to represent long-term climatic changes
#' such as gradual drying or wetting.
#'
#' To ensure physical consistency between variables, a coupling mechanism is
#' introduced whereby increased rainfall (proxy for cloud cover) reduces
#' maximum temperature through a linear cooling adjustment. This enforces
#' a negative dependence between precipitation and temperature consistent
#' with atmospheric energy balance principles.
#'
#' Finally, a minimum diurnal temperature difference constraint is enforced
#' after rounding to guarantee that Avg.Tx > Avg.Tn at all time steps, while
#' preserving the statistical distribution of the simulated variables.
#'
#' The default parameterization reflects typical tropical conditions for Ghana,
#' but all parameters are user-configurable, allowing adaptation to other
#' climatic regions.
#' The modelling approach follows established stochastic weather generation
#' principles while extending them with distributional asymmetry and
#' cross-variable coupling for improved physical realism.
#' @examples
#' st <- create_stations(n = 3, seed = 1)
#' sim <- simulate_climate_series(st, 1996, 2025, seed = 42)
#' head(sim)
#' @export
#' @importFrom truncnorm rtruncnorm
#' @importFrom lubridate days_in_month
#' @importFrom stats arima.sim
simulate_climate_series <- function(
    stations,
    start_year = 1996,
    end_year = 2025,
    seed = NULL,
    temp_trend_per_year = 0.02,    # °C/year warming
    rain_trend_per_year = -0.003,  # slight drying trend
    phi_temp = 0.85,               # AR(1) persistence
    sd = 0.4,                      # standard deviation for AR(1)
    # --- Temperature bounds (Ghana defaults) ---
    Tmin_min = 18,
    Tmin_max = 30,
    Tmax_min = 24,
    Tmax_max = 42
) {
  if (!is.null(seed)) set.seed(seed)
  # --- validation checks ---
  stopifnot(is.data.frame(stations))
  stopifnot(all(c("Station", "LON", "LAT") %in% names(stations)))
  if (Tmin_min >= Tmin_max) {
    stop("Tmin_min must be less than Tmin_max")
  }
  if (Tmax_min >= Tmax_max) {
    stop("Tmax_min must be less than Tmax_max")
  }
  dates <- seq(as.Date(sprintf("%04d-01-15", start_year)),
               as.Date(sprintf("%04d-12-15", end_year)),
               by = "month")
  n <- length(dates)
  monthly <- as.integer(format(dates, "%m"))
  years <- as.numeric(format(dates, "%Y"))
  generate_ar1 <- function(n, phi, sigma) {
    as.numeric(arima.sim(n = n, list(ar = phi), sd = sigma))
  }
  out <- lapply(seq_len(nrow(stations)), function(i) {
    s <- stations[i, , drop = FALSE]
    # --- Seasonal structure ---
    base_mean <- 26 - 0.08 * s$LAT
    amp <- 4 + 0.03 * abs(s$LAT - mean(stations$LAT))
    seasonal <- amp * sin(2 * pi * (monthly - 1) / 12)
    trend <- temp_trend_per_year * (years - start_year)
    noise <- generate_ar1(n, phi_temp, sd)
    tmean <- base_mean + seasonal + trend + noise
    # --- Tmin (bounded truncated normal) ---
    Tmin <- truncnorm::rtruncnorm(
      n,
      a = Tmin_min,
      b = Tmin_max,
      mean = tmean - 5.5,
      sd = 1.2
    )
    # --- Tmax (gamma + bounds) ---
    Tmax_raw <- tmean + rgamma(n, shape = 4, scale = 1.2)
    # --- Rainfall occurrence (Markov chain) ---
    wet <- numeric(n)
    p_wet <- ifelse(monthly %in% 4:10, 0.65, 0.25)
    wet[1] <- rbinom(1, 1, p_wet[1])
    for (t in 2:n) {
      p <- if (wet[t - 1] == 1) 0.75 else 0.30
      wet[t] <- rbinom(1, 1, p)
    }
    # --- Rainfall intensity (fixed phase shift) ---
    r_mean <- 70 * (1 + 0.3 * sin(2 * pi * (monthly - 7)/12))
    rain_trend <- 1 + rain_trend_per_year * (years - start_year)
    r_mean <- pmax(5, r_mean * rain_trend)
    SumRf <- ifelse(
      wet == 1,
      rgamma(n, shape = 2.5, scale = r_mean / 2.5),
      0
    )
    # --- Rainfall–temperature coupling ---
    cloud_index <- as.numeric(scale(SumRf))
    cloud_index[is.na(cloud_index)] <- 0
    Tmax <- Tmax_raw - 1.5 * cloud_index
    # --- Enforce bounds smoothly ---
    Tmin <- pmin(pmax(Tmin, Tmin_min), Tmin_max)
    Tmax <- pmin(pmax(Tmax, Tmax_min), Tmax_max)
    idx <- Tmax >= Tmax_max
    if (any(idx)) {
      Tmax[idx] <- Tmax_max - runif(sum(idx), 0, 0.3)
    }
    Tmin <- round(Tmin, 1)
    # ensure Tmax > Tmin
    idx <- Tmax <= Tmin
    if (any(idx)) {
      Tmax[idx] <- Tmin[idx] + 0.5
    }
    Tmax <- round(Tmax, 1)
    AvgRf <- SumRf / lubridate::days_in_month(dates)
    data.frame(
      Station = s$Station,
      LON = s$LON,
      LAT = s$LAT,
      Year = years,
      Month = monthly,
      Date = dates,
      Avg.Tn = Tmin,
      Avg.Tx = Tmax,
      Sum.Rf = round(SumRf, 1),
      Avg.Rf = round(AvgRf, 2),
      stringsAsFactors = FALSE
    )
  })
  simdf <- do.call(rbind, out)
  rownames(simdf) <- NULL
  return(simdf)
}
