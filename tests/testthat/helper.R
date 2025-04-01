# The contents of this file will be run for all testthat tests

library(data.table)

# Setup function for creating test data
setup_test_data <- function() {
  # Sample analytic data ----
    set.seed(98104)

    test_analytic <- data.table(
      chi_year = rep(2015:2024, each = 100),
      chi_sex = sample(c("Female", "Male"),
                       size = 1000,  # 100 rows * 10 years
                       replace = TRUE),
      chi_geo_region = sample(c("East", "North", "Seattle", "South"),
                               size = 1000,
                               replace = TRUE)
    )

    test_analytic[, `:=`(
      indicator1 = sample(0:1, .N, replace = TRUE),
      indicator2 = sample(0:1, .N, replace = TRUE)
    )]

    test_analytic[, chi_geo_kc := 'King County']

    setorder(test_analytic, chi_year)

  # Sample analysis set ----
    test_analysis_set <- data.table(
      cat1 = c('Regions', 'Gender'),
      cat1_varname = c('chi_geo_region', 'chi_sex'),
      `_kingcounty` = c('x'),
      `_wastate` = NA_character_,
      demgroups = NA_character_,
      crosstabs = NA_character_,
      trends = NA_character_,
      set = 1,
      set_indicator_keys = 'indicator1, indicator2'
    )

    test_analysis_set_twosets <- data.table(
      cat1 = rep(c('Regions', 'Gender', 'Race/ethnicity'),2),
      cat1_varname = rep(c('chi_geo_region', 'chi_sex', 'race4'),2),
      `_kingcounty` = c('x'),
      `_wastate` = rep(c(rep(NA_character_,2),"x"),2),
      demgroups = rep(c(rep(NA_character_,2),"x"),2),
      crosstabs = rep(c(rep(NA_character_,2),"x"),2),
      trends = rep(c(rep(NA_character_,2),"x"),2),
      set = c(rep(1,3), rep(2,3)),
      set_indicator_keys = c(rep(c('indicator1, indicator2'),3), rep("indicator3",3))
    )

  # Sample instructions ----
    test_instructions <- data.table(
      indicator_key = c("indicator1", "indicator2", "indicator1", "indicator2"),
      tab = c("demgroups", "demgroups", "_kingcounty", "_kingcounty"),
      cat1 = c("Regions", "Gender", "King County", "King County"),
      cat1_varname = c("chi_geo_region", "chi_sex", "chi_geo_kc", "chi_geo_kc"),
      cat2 = NA_character_,
      cat2_varname = NA_character_,
      start = c(2019),
      end = c(2024)
    )

  # Sample estimates ----
    test_estimates <- data.table(
      indicator_key = c("indicatorX"),
      tab = c(rep('demgroups', 4), '_kingcounty'),
      year = c('2023'),
      cat1 = c('Region', 'Region', 'Region', 'Region', 'King County'),
      cat1_group = c("East", "North", "Seattle", "South", 'King County'),
      cat1_varname = c('chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_kc'),
      cat2 = NA_character_,
      cat2_group = NA_character_,
      cat2_varname = NA_character_,
      data_source = 'JustTesting',
      caution = NA_character_,
      suppression = NA_character_,
      chi = 1,
      source_date = Sys.Date(),
      run_date = Sys.Date(),
      numerator = c(111, 175, 210, 600, 430000),
      denominator = c(1000, 1500, 2000, 2500, 2200000)
    )
    test_estimates[, result := numerator / denominator]
    test_estimates[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates[, rse := 100 * se / result]
    test_estimates[, lower_bound := result - 1.96 * se]
    test_estimates[, upper_bound := result + 1.96 * se]

    test_estimates_old <- data.table(
      indicator_key = c("indicatorX"),
      tab = c(rep('demgroups', 4), '_kingcounty'),
      year = c('2022'),
      cat1 = c('Region', 'Region', 'Region', 'Region', 'King County'),
      cat1_group = c("East", "North", "Seattle", "South", 'King County'),
      cat1_varname = c('chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_kc'),
      cat2 = NA_character_,
      cat2_group = NA_character_,
      cat2_varname = NA_character_,
      data_source = 'JustTesting',
      caution = NA_character_,
      suppression = NA_character_,
      chi = 1,
      source_date = Sys.Date(),
      run_date = Sys.Date(),
      numerator = c(113, 177, 209, 400, 460000),
      denominator = c(1000, 1500, 2000, 2500, 2300000)
    )
    test_estimates_old[, result := numerator / denominator]
    test_estimates_old[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates_old[, rse := 100 * se / result]
    test_estimates_old[, lower_bound := result - 1.96 * se]
    test_estimates_old[, upper_bound := result + 1.96 * se]

  # Sample metadata ----
    test_metadata <- data.table(
      indicator_key = c("indicatorX"),
      result_type = c("proportion"),
      valid_years = c("2020 2021 2022 2022"),
      latest_year = c(2022),
      data_source = 'test',
      valence = 'positive',
      latest_year_result = 0.20,
      latest_year_kc_pop = 2300000,
      latest_year_count = 460000,
      map_type = 'hra',
      unit = 'person',
      chi = 1,
      run_date = Sys.Date()
      )

  # Return ----
  list(my.analytic = test_analytic,
       my.analysis_set = test_analysis_set,
       my.analysis_set_twosets = test_analysis_set_twosets,
       my.estimate= test_estimates,
       my.estimate_old= test_estimates_old,
       my.metadata = test_metadata,
       my.instructions = test_instructions)
}
