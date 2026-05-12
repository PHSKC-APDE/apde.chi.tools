
DT <- setup_test_data()$my.death_data
DT[, zipcode := death_zip_code]
analysis_set <- data.table(cat1 = "Zip code", cat1_varname = "zipcode", '_kingcounty' = NA, '_wastate' = NA, demgroups = "x", crosstabs = NA, trends = "x", set = 1, set_indicator_keys = "malignant_neoplasm" )
tro_shell <- chi_generate_tro_shell(analysis_set, 2023, year.span = 8, trend.span = 3, trend.periods = 5)
DT.count.by.age <- chi_count_by_age(DT, ph.instructions = tro_shell, as.Date("2025-01-01"))
pop.instructions <- apde.chi.tools::chi_generate_instructions_pop(DT.count.by.age ,povgeo = "zip")
