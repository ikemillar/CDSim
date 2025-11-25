test_that("simulation produces expected rows", {
  s <- create_stations(n = 2)
  out <- simulate_climate_series(s)
  expect_gt(nrow(out), 30) # at least a month
})
