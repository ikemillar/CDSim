---
title: "README"
output: html_document
---

# CDSim

Simulate station-based monthly climate data (Tmin/Tmax/rainfall), export station NetCDF, and provide plotting helpers.

## Latest Update (v0.1.2)

- Improved climate simulation realism
- Added validation framework
- Updated vignette with validation workflow

## Quick start

```r
# install devtools then load package folder locally
# devtools::load_all("path/to/CDSim")

library(CDSim)
stations <- create_stations(n = 11, seed = 42)
sim <- simulate_climate_series(stations, 1981, 1985, seed = 101)
plot_station_timeseries(sim, "Station_1", "Avg.Tx")
write_station_netcdf(sim, "sim_example.nc")
```
