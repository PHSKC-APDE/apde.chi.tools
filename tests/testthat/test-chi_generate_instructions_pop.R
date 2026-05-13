
testthat::test_that("generate instructions pop generates accurately from both analysis instructions and count data", {


      set.seed(100)
      DT <- setup_test_data()$my.death_data
      DT[, zipcode := death_zip_code]
      analysis_set <- data.table(cat1 = c("Zip code","Regions"), cat1_varname = c("zipcode","chi_geo_region"), '_kingcounty' = NA, '_wastate' = NA, demgroups = "x", crosstabs = NA, trends = "x", set = 1, set_indicator_keys = c("malignant_neoplasm", "malignant_neoplasm") )
      tro_shell <- chi_generate_tro_shell(analysis_set, 2023, year.span = 8, trend.span = 3, trend.periods = 5)
      tro_shell[, year := paste0(start,"-",end)]
      DT.count.by.age <- chi_count_by_age(DT, ph.instructions = tro_shell, as.Date("2025-01-01"))
      pop.instructions_from_tro_sell <- apde.chi.tools::chi_generate_instructions_pop(tro_shell ,povgeo = "zip")
      pop.instructions_from_count_data <- apde.chi.tools::chi_generate_instructions_pop(DT.count.by.age ,povgeo = "zip")
      testthat::expect_true(
      all.equal(pop.instructions_from_tro_sell, pop.instructions_from_count_data, ignore.row.order = T, ignore.col.order = T )
      )


  }
)

