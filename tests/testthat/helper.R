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


    data_modeller <- function(ph.data, number_of_observations, return_code = TRUE, comments = TRUE) {
      ### receives a data table of public health data, number of observations and user decision if they want code (or a DT) and, if code, if it should be commented
      ### returns code or a DT of identical structure and similar, but non-correlated, values for each variable provided that can be modeled. If comments are enabled, will return comment for non modeled variables.
      ### warning: r has a run-instruction character limit of 4094. If code is requested, and the resulting instruction is longer, you must break this into seperate instructions, such making several smaller DTs and binding them together. (test if sourcing as a script is an exception to the limit)
      ### warning: (not implemented) currently will create multiple years, but reads the received data set as if it were one year, and models multiple years by repeating the model process with shifted seed
      ### warning: (not implemented) number.of.observations is of the final dataset. If the requested number does not divide evenly across the number of years, the result will be rounded up and the user should remove observations if necessary
      ### warning: the data returned is modelled on the data given but correlations between variables are not. This effectively anonymizes results as long as the underlying populations are diverse or large enough. A small enough population may provide sufficient certainty of the results. (note to self for vignette: build an example showing seperate runs. One of a DT built from multiple mono-race reuslts bound together, versus building results from a table with observations from multiple races. Show how the results in the former more closely resembles results by race from actual data.)

      if(!return_code & comments) {
        message("user has requested data, comments set to FALSE")
        comments <- FALSE
      }

      variable_modeller <- function(oneVariable, number_of_observations, varName = NA, comments = TRUE) {
        if(any(class(oneVariable) %in% "data.table")) {
          if(ncol(oneVariable) == 1) {
            message(class(oneVariable))
            oneVariable <- oneVariable[,1][[1]]
            message(class(oneVariable))
            message("caught DT")
          } else {
            stop("more than 1 column passed. Only pass a vector or one column")
          }
        }
        #note : ooooooocurrently setting 61 as categorical threshold because of HRAs.

        #if no match, report unmatched type
        instructions <- NA

        if(is.na(varName)){
          variableName <- sub(".*\\$.*?", "\\1", deparse(substitute(oneVariable)))
        } else {
          variableName <- varName
        }

        #factor
        if(is.na(instructions) & class(oneVariable) == "factor") {
          orderTF <- is.ordered(oneVariable)
          detectedLevels <- levels(oneVariable)
          instructions <- paste0(variableName," = factor(sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),")), levels = c('",paste0(detectedLevels, collapse = "', '"),"'), ordered = ", orderTF,")", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){
            instructions <- paste0(instructions, " # as a factor")
          }
        }

        #integer: categorical
        if(is.na(instructions) & (class(oneVariable) == "numeric" | class(oneVariable) == "integer") & (length(unique(oneVariable)) <= 61 & length(oneVariable) > 61)) {
          instructions <- paste0(variableName," = sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),"))", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){
            instructions <- paste0(instructions, " # as a categorical non factor")
          }
        }

        #character: categorical
        if(is.na(instructions) & class(oneVariable) == "character" & (length(unique(oneVariable)) <= 61 & length(oneVariable) > 61)) {
          instructions <- paste0(variableName," = sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),"))", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){

            instructions <- paste0(instructions, " # as a categorical non factor")
          }
        }

        #continuous
        if(is.na(instructions) & class(oneVariable) == "numeric" & (length(unique(oneVariable)) > 61 & length(oneVariable) > 61)) {
          #uniform distribution
          instructions <- paste0(variableName, " = runif(", number_of_observations,", ", min(oneVariable, na.rm = TRUE), ", ", max(oneVariable, na.rm = TRUE),")")
          if(comments){
            instructions <- paste0(instructions, " # continuous with uniform distribution")
          }
        }

        #if unmatched
        if(is.na(instructions)) {

          instructions <- paste0("`",variableName,"`", " = NA")
          if(comments){
            instructions <- paste0(instructions, " # data type not modelled")
          }
        }

        if(is.na(instructions)) {

        } else{
          return(instructions)
        }
      }

      batch_variable_modeller <- function(x) {
        variable_modeller(ph.data[,..x][[1]], number_of_observations, names(ph.data)[x], comments = comments)
      }

      codeList <- lapply(seq_along(ph.data), batch_variable_modeller)

      if(comments) {

        codeListFormatted <- c(gsub(" #", ", #", codeList[1:(length(codeList)-1)]), codeList[length(codeList)])
        codeText <- paste(unlist(codeListFormatted), collapse =" \n" )

      } else {
        codeText <- paste(unlist(codeList), collapse =", " )
      }

      if(return_code) {

        cat(codeText)
        return(codeList)

      } else {


        eval( parse(text = paste0("DT <- data.table(", codeText, ")",collapse =  "")))

        return(DT)
      }

    }


    generate_test_data <- function(dataset = "generic", observations = 100, seed = 1000, years = 2023){
      ### generates a synthetic data set appropriate for testing functions relying on APDE data structures and where you do not want to use real data
      ### receives description of data set to emulate, number of observations to include, a seed and number of years.
      ### returns a data.table of synthetic data. If dataset is "generic" the returned structure will have idealized chi values and generic indicators

      # input validation
      datasetOptions <- c("generic", "hys")
      dataset <- tolower(dataset)
      if(!(dataset %in% datasetOptions)) {
        stop(paste0("dataset must be one of: '", paste(datasetOptions, collapse = "', '"),"'"))
      }

      year_iterator <- function(observations, seed, years) {

      }

      if(dataset == "generic") {

        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            id = 1:observations,
            chi_geo_kc = sample(c(0,1), observations, replace = T),
            chi_race_4 = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), number_of_observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
            chi_sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
            chi_geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
            indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
            indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
            indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)),
            chi_year = year)
          if(exists("returnDT")) {
            returnDT <- rbind(returnDT, DTIteration)
          } else {
            returnDT <- DTIteration
          }
        }
      } else if(dataset == "hys") {
        returnDT <- data.table()



      }
      return(returnDT)
    }

    test_data_generic <- generate_test_data("generic", 100, 1000, c(2016:2023))
    # testHYS <- get_data_hys()
    # testBRFSS <- as_table_brfss(get_data_brfss())
    #
    #
    # inputDT <- testHYS
    #
    # testDT <- data_modeller(inputDT, number_of_observations = 1000, years = 2020, return_code = FALSE, comments = TRUE)
    # testCode <- data_modeller(inputDT, number_of_observations = 1000, years = 2020, return_code = TRUE, comments = TRUE)


    test_analysis_set_twosets <- data.table(
      #this should work with the generic data set
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

    test_analysis_set_twosets_estimates <- data.table(
      for(indicator in c("indicator1","indicator2")) {
        partialDT <- data.table(
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
      }


    )
    test_estimates[, result := numerator / denominator]
    test_estimates[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates[, rse := 100 * se / result]
    test_estimates[, lower_bound := result - 1.96 * se]
    test_estimates[, upper_bound := result + 1.96 * se]

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
      indicator_key = c("indicator1"),
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

    test_estimates_twosets <- data.table(
      indicator_key = c("indicator1"),
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
    test_estimates_twosets[, result := numerator / denominator]
    test_estimates_twosets[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates_twosets[, rse := 100 * se / result]
    test_estimates_twosets[, lower_bound := result - 1.96 * se]
    test_estimates_twosets[, upper_bound := result + 1.96 * se]



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
       my.generic_data = test_data_generic,
       my.estimate= test_estimates,
       my.estimate_old= test_estimates_old,
       my.metadata = test_metadata,
       my.instructions = test_instructions)
}
