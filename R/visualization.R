#' Visualization Functions for Climate Data
#'
#' @name visualization
NULL
#' Plot Station Time Series with Seasonal Detection
#'
#' @description Creates a time-series plot for climate variables with automatic hemisphere-based season detection.
#'
#' @param df A tidy dataset containing columns: `Station`, `Date`, `LAT`, and variables.
#' @param station Station name.
#' @param var Climate variable to plot.
#' @param smooth Add LOESS smoothing line.
#' @param theme_dark Use dark theme.
#'
#' @return A ggplot object.
#'
#' @examples
#' stations <- create_stations(n = 3)
#' sim <- simulate_climate_series(stations)
#' plot_station_timeseries(sim, station = "Station_1", var = "Avg.Tn")
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth labs scale_x_date scale_color_manual theme_minimal theme_dark element_text
#' @importFrom dplyr filter mutate case_when `%>%`
#' @importFrom lubridate month
#' @importFrom stats median rnorm rgamma
#' @export
plot_station_timeseries <- function(df, station, var = "Avg.Tn", smooth = TRUE, theme_dark = FALSE) {
  stopifnot(var %in% c("Avg.Tn","Avg.Tx","Sum.Rf","Avg.Rf"))
  dsub <- df %>% dplyr::filter(Station == station)
  # Determine hemisphere via median latitude
  hemi <- if (abs(median(dsub$LAT)) < 5) {
    "Equatorial"
  } else if (median(dsub$LAT) > 0) {
    "Northern"
  } else {
    "Southern"
  }
  # Add month column
  dsub <- dsub %>% dplyr::mutate(Month = lubridate::month(Date))
  # Assign seasons based on region
  dsub <- dsub %>%
    dplyr::mutate(
      Season = dplyr::case_when(
        # Equatorial tropical pattern
        hemi == "Equatorial" & Month %in% c(11,12,1,2,3) ~ "Dry Season",
        hemi == "Equatorial" ~ "Wet Season",
        # Northern Hemisphere
        hemi == "Northern" & Month %in% c(12,1,2) ~ "Winter",
        hemi == "Northern" & Month %in% c(3,4,5) ~ "Spring",
        hemi == "Northern" & Month %in% c(6,7,8) ~ "Summer",
        hemi == "Northern" & Month %in% c(9,10,11) ~ "Autumn",
        # Southern Hemisphere (inverted)
        hemi == "Southern" & Month %in% c(6,7,8) ~ "Winter",
        hemi == "Southern" & Month %in% c(9,10,11) ~ "Spring",
        hemi == "Southern" & Month %in% c(12,1,2) ~ "Summer",
        hemi == "Southern" & Month %in% c(3,4,5) ~ "Autumn",
        TRUE ~ "Unknown"
      )
    )

  # Color theme per variable
  var_colors <- list(
    "Avg.Tn" = list(line = "#3182bd", smooth = "#08519c"),
    "Avg.Tx" = list(line = "#e31a1c", smooth = "#a50f15"),
    "Sum.Rf" = list(line = "#084594", smooth = "#08306b"),
    "Avg.Rf" = list(line = "#2ca25f", smooth = "#006d2c")
  )
  # Universal seasonal palette
  season_palette <- c(
    "Winter" = "#81b1d2",
    "Spring" = "#4daf4a",
    "Summer" = "#e41a1c",
    "Autumn" = "#ff7f00",
    "Wet Season" = "#1b9e77",
    "Dry Season" = "#d95f02",
    "Unknown" = "grey60"
  )
  p <- ggplot2::ggplot(dsub, ggplot2::aes(x = Date, y = !!rlang::sym(var))) +
    ggplot2::geom_line(linewidth = 1, color = var_colors[[var]]$line) +
    ggplot2::geom_point(aes(color = Season), size = 2) +
    ggplot2::scale_color_manual(values = season_palette) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = paste(var, "Time Series at", station),
      subtitle = paste("Hemisphere:", hemi,
                       " | Period:", format(min(dsub$Date), "%Y"), "-", format(max(dsub$Date), "%Y")),
      x = "Date",
      y = var,
      color = "Season"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      legend.position = "bottom"
    )
  if (smooth) {
    p <- p + ggplot2::geom_smooth(
      method = "loess",
      span = 0.25,
      linewidth = 0.9,
      color = var_colors[[var]]$smooth,
      alpha = 0.25
    )
  }
  if (theme_dark) {
    p <- p + ggplot2::theme_dark()
  }
  return(p)
}
utils::globalVariables(c("Season"))
