test_that("create_stations returns correct structure", {
  s <- create_stations(n = 5)
  expect_equal(nrow(s), 5)
  expect_true(all(c("Station","LON","LAT") %in% names(s)))
})
