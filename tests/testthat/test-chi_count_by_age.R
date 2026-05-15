testthat::test_that("issues zip code warning when zip codes not declared in expeted chi variables are present", {


  set.seed(100)
  DT <- setup_test_data()$my.death_data
  DT[, zipcode := death_zip_code]
  analysis_set <- data.table(cat1 = c("Zip code","Regions"), cat1_varname = c("zipcode","chi_geo_region"), '_kingcounty' = NA, '_wastate' = NA, demgroups = "x", crosstabs = NA, trends = "x", set = 1, set_indicator_keys = c("malignant_neoplasm", "malignant_neoplasm") )
  tro_shell <- chi_generate_tro_shell(analysis_set, 2023, year.span = 8, trend.span = 3, trend.periods = 5)
  tro_shell[, year := paste0(start,"-",end)]
  testthat::expect_warning(chi_count_by_age(DT, ph.instructions = tro_shell, as.Date("2025-01-01")))
}
)

testthat::test_that("does not issue zip code warning when all zip codes are in expected chi variable list", {

  set.seed(100)
  DT <- setup_test_data()$my.death_data
  DT[, zipcode := death_zip_code]
  validZips <- chi_standard_varnames[varname == "zipcode",group]
  DT <- DT[zipcode %in% validZips,]
  missingZips <- validZips[!(validZips %in% DT[,zipcode])]
  #add missing zips to test warning
  for(i in 1:length(missingZips)) {
    newrow <- DT[1,]
    newrow$zipcode <- missingZips[i]
    DT <- rbind(DT, newrow)
  }

  analysis_set <- data.table(cat1 = c("Zip code","Regions"), cat1_varname = c("zipcode","chi_geo_region"), '_kingcounty' = NA, '_wastate' = NA, demgroups = "x", crosstabs = NA, trends = "x", set = 1, set_indicator_keys = c("malignant_neoplasm", "malignant_neoplasm") )
  tro_shell <- chi_generate_tro_shell(analysis_set, 2023, year.span = 8, trend.span = 3, trend.periods = 5)
  testthat::expect_no_warning(chi_count_by_age(DT, ph.instructions = tro_shell, as.Date("2025-01-01")))


}
)

