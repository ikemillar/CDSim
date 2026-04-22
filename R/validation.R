#' Validate simulated climate data against observations
#'
#' Performs statistical and physical validation of simulated climate data
#' against observed datasets, including distributional tests, mean comparison,
#' dependence structure, and temporal persistence.
#'
#' @param sim Simulated climate data.frame
#' @param obs Observed climate data.frame
#'
#' @return A list containing validation metrics and test results
#' @export
#' @importFrom stats rbinom ks.test t.test wilcox.test cor acf sd lm coef
validate_climate <- function(sim, obs) {
  stopifnot(is.data.frame(sim), is.data.frame(obs))
  required_cols <- c("Avg.Tx", "Avg.Tn", "Sum.Rf")
  stopifnot(all(required_cols %in% names(sim)))
  stopifnot(all(required_cols %in% names(obs)))
  # --- Helper function for safe tests ---
  safe_test <- function(expr) {
    tryCatch(expr, error = function(e) NA)
  }
  # --- Distribution tests (K-S) ---
  ks_tests <- list(
    Tmax = safe_test(ks.test(sim$Avg.Tx, obs$Avg.Tx)),
    Tmin = safe_test(ks.test(sim$Avg.Tn, obs$Avg.Tn)),
    Rain = safe_test(ks.test(sim$Sum.Rf, obs$Sum.Rf))
  )
  # --- Mean comparison ---
  mean_tests <- list(
    ttest_Tmax = safe_test(t.test(sim$Avg.Tx, obs$Avg.Tx)),
    ttest_Tmin = safe_test(t.test(sim$Avg.Tn, obs$Avg.Tn)),
    ttest_Rain = safe_test(t.test(sim$Sum.Rf, obs$Sum.Rf)),

    wilcox_Tmax = safe_test(wilcox.test(sim$Avg.Tx, obs$Avg.Tx)),
    wilcox_Tmin = safe_test(wilcox.test(sim$Avg.Tn, obs$Avg.Tn)),
    wilcox_Rain = safe_test(wilcox.test(sim$Sum.Rf, obs$Sum.Rf))
  )

  # --- Correlation structure (physical coupling) ---
  correlation <- list(
    sim = cor(sim[, c("Sum.Rf", "Avg.Tx", "Avg.Tn")], use = "complete.obs"),
    obs = cor(obs[, c("Sum.Rf", "Avg.Tx", "Avg.Tn")], use = "complete.obs")
  )

  # --- Autocorrelation (lag-1) ---
  acf_sim <- list(
    Tmax = acf(sim$Avg.Tx, plot = FALSE)$acf[2],
    Tmin = acf(sim$Avg.Tn, plot = FALSE)$acf[2],
    Rain = acf(sim$Sum.Rf, plot = FALSE)$acf[2]
  )
  acf_obs <- list(
    Tmax = acf(obs$Avg.Tx, plot = FALSE)$acf[2],
    Tmin = acf(obs$Avg.Tn, plot = FALSE)$acf[2],
    Rain = acf(obs$Sum.Rf, plot = FALSE)$acf[2]
  )

  # --- Trend analysis (Mann-Kendall) ---
  if (requireNamespace("trend", quietly = TRUE)) {
    trend_tests <- list(
      Tmax = trend::mk.test(sim$Avg.Tx),
      Rain = trend::mk.test(sim$Sum.Rf)
    )
  } else {
    trend_tests <- NA
  }
  # --- Summary metrics ---
  summary_stats <- data.frame(
    Variable = c("Tmax", "Tmin", "Rain"),
    Mean_sim = c(mean(sim$Avg.Tx), mean(sim$Avg.Tn), mean(sim$Sum.Rf)),
    Mean_obs = c(mean(obs$Avg.Tx), mean(obs$Avg.Tn), mean(obs$Sum.Rf)),
    SD_sim = c(sd(sim$Avg.Tx), sd(sim$Avg.Tn), sd(sim$Sum.Rf)),
    SD_obs = c(sd(obs$Avg.Tx), sd(obs$Avg.Tn), sd(obs$Sum.Rf))
  )
  return(list(
    ks_tests = ks_tests,
    mean_tests = mean_tests,
    correlation = correlation,
    autocorrelation = list(sim = acf_sim, obs = acf_obs),
    trend = trend_tests,
    summary = summary_stats
  ))
}


#' Internal validation of simulated climate data
#'
#' Evaluates physical plausibility and statistical properties of simulated
#' climate data in the absence of observational datasets. The function
#' assesses distributional characteristics, temporal persistence,
#' inter-variable relationships, and physical constraints.
#'
#' @param sim Simulated climate data.frame
#'
#' @return A list of validation diagnostics
#' @export
#' @importFrom stats cor acf sd lm coef
validate_climate_internal <- function(sim) {
  stopifnot(is.data.frame(sim))
  required_cols <- c("Avg.Tx", "Avg.Tn", "Sum.Rf")
  stopifnot(all(required_cols %in% names(sim)))
  # Remove NA safely
  sim <- sim[complete.cases(sim[, required_cols]), ]
  # --- Physical constraints ---
  checks <- list(
    Tmin_min = min(sim$Avg.Tn),
    Tmax_max = max(sim$Avg.Tx),
    Rain_min = min(sim$Sum.Rf),
    Tmax_gt_Tmin = all(sim$Avg.Tx > sim$Avg.Tn),
    Tmin_plausible = min(sim$Avg.Tn) >= 18
  )

  # --- Distribution properties ---
  safe_sd <- function(x) ifelse(sd(x) == 0, NA, sd(x))
  skewness <- function(x) {
    m <- mean(x)
    s <- safe_sd(x)
    if (is.na(s)) return(NA)
    mean((x - m)^3) / s^3
  }

  distribution <- list(
    rain_skewness = skewness(sim$Sum.Rf),
    Tmax_skewness = skewness(sim$Avg.Tx),
    Tmin_skewness = skewness(sim$Avg.Tn),
    sd_Tmax = sd(sim$Avg.Tx),
    sd_Tmin = sd(sim$Avg.Tn),
    sd_Rain = sd(sim$Sum.Rf)
  )

  # --- Correlation (physical coupling) ---
  correlation <- cor(sim[, c("Sum.Rf", "Avg.Tx", "Avg.Tn")],
                     use = "complete.obs")
  rain_temp_relationship <- correlation["Sum.Rf", "Avg.Tx"] < 0
  # --- Autocorrelation ---
  acf_vals <- list(
    Tmax = acf(sim$Avg.Tx, plot = FALSE)$acf[2],
    Tmin = acf(sim$Avg.Tn, plot = FALSE)$acf[2],
    Rain = acf(sim$Sum.Rf, plot = FALSE)$acf[2]
  )
  # --- Simple trend check (linear approximation) ---
  time_index <- seq_along(sim$Avg.Tx)
  trend <- list(
    Tmax_slope = coef(lm(sim$Avg.Tx ~ time_index))[2],
    Rain_slope = coef(lm(sim$Sum.Rf ~ time_index))[2]
  )
  # --- Seasonal pattern check ---
  seasonal_means <- tapply(sim$Sum.Rf, sim$Month, mean)
  seasonality <- list(
    peak_month = which.max(seasonal_means),
    trough_month = which.min(seasonal_means)
  )
  overall_valid <- all(
    checks$Tmax_gt_Tmin,
    checks$Tmin_plausible,
    rain_temp_relationship,
    acf_vals$Tmax > 0.5
  )
  return(list(
    summary = summary(sim),
    checks = checks,
    distribution = distribution,
    correlation = correlation,
    rain_temp_coupling = rain_temp_relationship,
    autocorrelation = acf_vals,
    trend = trend,
    seasonality = seasonality,
    valid = overall_valid
  ))
}
