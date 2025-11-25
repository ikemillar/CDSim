test_that("simulate_climate_series produces expected columns", {
  st <- create_stations(n = 2, seed = 1)
  sim <- simulate_climate_series(st)

  expect_true(all(c("Station", "Date", "Avg.Tn") %in% names(sim)))
  expect_gt(nrow(sim), 0)
})
