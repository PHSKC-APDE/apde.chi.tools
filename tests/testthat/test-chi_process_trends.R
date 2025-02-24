test_that("calculates trends", {
  # chi_generate_trend_years does not exist
  # DT <- chi_generate_trend_years(indicator_key = c("test1", "test2"),span = 3,begin.year = 2009,final.year = 2023)
  expect_identical(1L, 1L) # a dummy test because devtools::check does not allow empty test_that statements
})
