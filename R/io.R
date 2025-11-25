#' Write station CSV
#' Exports a simulated climate station dataset to a CSV file.
#' @param df A dataframe returned by `simulate_climate_series()`.
#' @param file The output CSV filename.
#' @return Returns the file path invisibly.
#' @export
#' @examples
#' \dontrun{
#'   stations <- create_stations(n = 3)
#'   sim <- simulate_climate_series(stations)
#'   tmp <- tempfile(fileext = ".csv")
#'   write_station_csv(sim, tmp)
#' }
#'
#' @importFrom vroom vroom_write
write_station_csv <- function(df, file = "simulated_station_climate.csv") {
  stopifnot(is.data.frame(df))  # safety check
  vroom::vroom_write(
    x = df,
    file = file,
    delim = ","
  )
  invisible(file)
}

#'
#' Write station NetCDF (station x time)
#' @param df station x time long dataframe returned by simulate_climate_series()
#' @param out_nc Output NetCDF filename
#' @param fillvalue Value used for missing entries
#' @export
#' @importFrom dplyr arrange distinct select slice
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom ncdf4 nc_create ncvar_def ncdim_def ncvar_put ncatt_put nc_close
#' @importFrom tibble tibble
#' @examples
#' stations <- create_stations(n = 3)
#' sim <- simulate_climate_series(stations)
#' tmp <- tempfile(fileext = ".nc")
#' write_station_netcdf(sim, tmp)
write_station_netcdf <- function(df, out_nc = "simulated_station_climate.nc",
                                 fillvalue = -9999.0) {
  # Ensure required columns exist
  req <- c("Station","LON","LAT","Date","Avg.Tn","Avg.Tx","Sum.Rf","Avg.Rf")
  stopifnot(all(req %in% names(df)))
  # Normalize date format
  df <- df %>% mutate(Date = as.Date(Date))
  df2 <- df %>% arrange(Station, Date)
  # Helper function: reshape into matrix
  pivot_var <- function(var) {
    wide <- df2 %>%
      select(Station, Date, !!rlang::sym(var)) %>%
      tidyr::pivot_wider(names_from = Date, values_from = !!rlang::sym(var))
    mat <- as.matrix(wide[, -1])
    colnames(mat) <- format(as.Date(colnames(mat)), "%Y-%m-%d")
    rownames(mat) <- wide$Station
    list(mat = mat, stations = wide$Station, dates = as.Date(colnames(mat)))
  }
  # Pivot variables
  tmin <- pivot_var("Avg.Tn")
  tmax <- pivot_var("Avg.Tx")
  rsum <- pivot_var("Sum.Rf")
  ravg <- pivot_var("Avg.Rf")
  # Check time consistency
  if(!identical(tmin$dates, tmax$dates)) stop("Times misaligned (Tmin vs Tmax)")
  if(!identical(tmin$dates, rsum$dates)) stop("Times misaligned (Tmin vs Rain)")
  # Spatial data
  coords <- df2 %>%
    distinct(Station, .keep_all = TRUE) %>%
    select(Station, LON, LAT) %>%
    filter(Station %in% tmin$stations) %>%
    slice(match(tmin$stations, Station))
  # NetCDF dimensions
  nstations <- nrow(tmin$mat)
  time_vals <- as.numeric(tmin$dates - tmin$dates[1])
  station_dim <- ncdf4::ncdim_def("station", "", vals = 1:nstations, create_dimvar = FALSE)
  time_dim <- ncdf4::ncdim_def("time", paste0("days since ", tmin$dates[1]), vals = time_vals)
  # Define variables
  lon_var  <- ncdf4::ncvar_def("longitude", "degrees_east",  list(station_dim), fillvalue)
  lat_var  <- ncdf4::ncvar_def("latitude",  "degrees_north", list(station_dim), fillvalue)
  tmin_var <- ncdf4::ncvar_def("Tmin", "degC", list(station_dim, time_dim), fillvalue)
  tmax_var <- ncdf4::ncvar_def("Tmax", "degC", list(station_dim, time_dim), fillvalue)
  rsum_var <- ncdf4::ncvar_def("RainTotal", "mm", list(station_dim, time_dim), fillvalue)
  ravg_var <- ncdf4::ncvar_def("RainAvg", "mm", list(station_dim, time_dim), fillvalue)
  if (file.exists(out_nc)) file.remove(out_nc)
  # Create NetCDF safely
  nc <- ncdf4::nc_create(out_nc, list(lon_var, lat_var, tmin_var, tmax_var, rsum_var, ravg_var))
  # Always close on exit
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  # Write data
  ncdf4::ncvar_put(nc, lon_var,  coords$LON)
  ncdf4::ncvar_put(nc, lat_var,  coords$LAT)
  ncdf4::ncvar_put(nc, tmin_var, tmin$mat)
  ncdf4::ncvar_put(nc, tmax_var, tmax$mat)
  ncdf4::ncvar_put(nc, rsum_var, rsum$mat)
  ncdf4::ncvar_put(nc, ravg_var, ravg$mat)
  # Metadata
  ncdf4::ncatt_put(nc, 0, "title", "Simulated Station Climate Data")
  ncdf4::ncatt_put(nc, 0, "history", paste("Created on", Sys.time()))
  invisible(out_nc)
}
