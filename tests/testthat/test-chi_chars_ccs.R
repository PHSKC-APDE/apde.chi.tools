library(testthat)
library(data.table)
library(rads)

# Create mock_chars data ----
    set.seed(98104)

    # ICD codes for asthma and non-asthma conditions
    icd9_asthma <- c("49300", "49301", "49302", "49310", "49311", "49312", "49320",
                     "49321", "49322", "49380", "49381", "49382", "49390", "49391", "49392")

    icd10_asthma <- c("J45", "J452", "J4520", "J4521", "J4522", "J453", "J4530", "J4531",
                      "J4532", "J454", "J4540", "J4541", "J4542", "J455", "J4550", "J4551",
                      "J4552", "J459", "J4590", "J45901", "J45902", "J45909", "J4599",
                      "J45990", "J45991", "J45998")

    icd9_non_asthma <- c("E9855", "36190", "V1508", "76425", "36642", "66061", "V7040",
                         "V7710", "V4530", "V0660", "76830", "85406")

    icd10_non_asthma <- c("S80241D", "S52134B", "S62256S", "S60922S", "V629", "K9420",
                          "S59241G", "S52042R", "J8417", "S83136D", "M06851", "T363X3D")

    # Create 1000 sample CHARS records
    n_samples <- 1000
    half_samples <- n_samples / 2

    # Create ICD-9 era data (pre-2016)
    icd9_data <- data.table(
      seq_no = 1:half_samples,  # Unique identifier
      chi_year = sample(2013:2015, half_samples, replace = TRUE),
      chi_age = sample(0:99, half_samples, replace = TRUE),
      chi_geo_kc = rep("King County", half_samples),
      wastate = rep("Washington State", half_samples),
      race3_hispanic = sample(c("Hispanic", "Non-Hispanic"), half_samples, replace = TRUE),
      chi_geo_region = sample(c("Seattle", "South", "East", "North"), half_samples, replace = TRUE),
      chi_sex = sample(c("Female", "Male"), half_samples, replace = TRUE)
    )

    # Ensure at least 100 asthma cases
    asthma_cases <- 100
    non_asthma_cases <- half_samples - asthma_cases

    icd9_data[1:asthma_cases, diag1 := sample(icd9_asthma, asthma_cases, replace = TRUE)]
    icd9_data[(asthma_cases+1):half_samples, diag1 := sample(icd9_non_asthma, non_asthma_cases, replace = TRUE)]

    # Create ICD-10 era data (2016 and after)
    icd10_data <- data.table(
      seq_no = (half_samples+1):n_samples,  # Continue sequence numbers
      chi_year = sample(2016:2022, half_samples, replace = TRUE),
      chi_age = sample(0:99, half_samples, replace = TRUE),
      chi_geo_kc = rep("King County", half_samples),
      wastate = rep("Washington State", half_samples),
      race3_hispanic = sample(c("Hispanic", "Non-Hispanic"), half_samples, replace = TRUE),
      chi_geo_region = sample(c("Seattle", "South", "East", "North"), half_samples, replace = TRUE),
      chi_sex = sample(c("Female", "Male"), half_samples, replace = TRUE)
    )

    # Ensure at least 100 asthma cases
    icd10_data[1:asthma_cases, diag1 := sample(icd10_asthma, asthma_cases, replace = TRUE)]
    icd10_data[(asthma_cases+1):half_samples, diag1 := sample(icd10_non_asthma, non_asthma_cases, replace = TRUE)]

    # Combine the data
    mock_chars <- rbindlist(list(icd9_data, icd10_data))

# Create mock_instructions ----
    mock_instructions <- data.table(
      indicator_key = rep(c("hos1803000_v1", "hos1803000_v2"), 3),
      tab = rep(c("trends", "_wastate"), each = 3),
      cat1 = rep("Ethnicity", 6),
      cat1_varname = rep("race3_hispanic", 6),
      cat2 = c(NA_character_, NA_character_, "Sex", "Sex", NA_character_, NA_character_),
      cat2_varname = c(NA_character_, NA_character_, "chi_sex", "chi_sex", NA_character_, NA_character_),
      end = c(2017, 2022, 2017, 2022, 2015, 2015),
      start = c(2013, 2018, 2014, 2016, 2013, 2013)
    )

# Create mock_chars_def ----
    mock_chars_def <- data.table(
      indicator_name = c("Asthma hospitalizations (all ages)", "Asthma hospitalizations (children)"),
      indicator_key = c("hos1803000_v1", "hos1803000_v2"),
      intent = c(NA_character_, NA_character_),
      mechanism = c(NA_character_, NA_character_),
      superlevel = c(NA_character_, NA_character_),
      broad = c("RESP", NA_character_),
      midlevel = c(NA_character_, "Asthma"),
      detailed = c(NA_character_, NA_character_),
      age_start = c(0, 0),
      age_end = c(120, 17)
    )

# Create function for mock ccs_table ----
    # Function to create mock CCS reference tables
    create_mock_ccs_table <- function(icdcm_version) {
      if (icdcm_version == 9) {
        icd_codes <- c("49300", "49301", "49302", "49310", "49311", "49312", "49320",
                       "49321", "49322", "49380", "49381", "49382", "49390", "49391", "49392",
                       "E9855", "36190", "V1508", "76425", "36642", "66061", "V7040",
                       "V7710", "V4530", "V0660", "76830", "85406")

        # Ensure all asthma codes are properly classified
        asthma_codes <- c("49300", "49301", "49302", "49310", "49311", "49312", "49320",
                          "49321", "49322", "49380", "49381", "49382", "49390", "49391", "49392")
      } else { # ICD-10
        icd_codes <- c("J45", "J452", "J4520", "J4521", "J4522", "J453", "J4530", "J4531",
                       "J4532", "J454", "J4540", "J4541", "J4542", "J455", "J4550", "J4551",
                       "J4552", "J459", "J4590", "J45901", "J45902", "J45909", "J4599",
                       "J45990", "J45991", "J45998",
                       "S80241D", "S52134B", "S62256S", "S60922S", "V629", "K9420",
                       "S59241G", "S52042R", "J8417", "S83136D", "M06851", "T363X3D")

        # Ensure all asthma codes are properly classified
        asthma_codes <- c("J45", "J452", "J4520", "J4521", "J4522", "J453", "J4530", "J4531",
                          "J4532", "J454", "J4540", "J4541", "J4542", "J455", "J4550", "J4551",
                          "J4552", "J459", "J4590", "J45901", "J45902", "J45909", "J4599",
                          "J45990", "J45991", "J45998")
      }

      # Create the data table
      ccs_table <- data.table(
        icdcm_code = icd_codes,
        icdcm = paste0("Description for ", icd_codes),
        superlevel = NA_character_,
        broad = NA_character_,
        midlevel = NA_character_,
        detailed = NA_character_,
        icdcm_version = icdcm_version
      )

      # Assign RESP to all asthma codes
      ccs_table[icdcm_code %in% asthma_codes, broad := "RESP"]

      # Assign Asthma to all asthma codes
      ccs_table[icdcm_code %in% asthma_codes, midlevel := "Asthma"]

      return(ccs_table)
    }

# Create vector of expected column order ----
    expectedCols <- c('indicator_key', 'year', 'chi_age', 'hospitalizations', 'tab', 'cat1', 'cat1_varname', 'cat1_group', 'cat2', 'cat2_varname', 'cat2_group')

# Test validation ----
    test_that("chi_chars_ccs validates inputs correctly", {
      # Test missing ph.indicator
      expect_error(chi_chars_ccs(ph.indicator = NA,
                                 ph.data = mock_chars,
                                 myinstructions = mock_instructions,
                                 chars.defs = mock_chars_def),
                   "ph.indicator must be provided")

      # Test missing ph.data
      expect_error(chi_chars_ccs(ph.indicator = "hos1803000_v1",
                                 ph.data = NULL,
                                 myinstructions = mock_instructions,
                                 chars.defs = mock_chars_def),
                   "ph.data must be specified")

      # Test indicator not found in instructions
      expect_error(chi_chars_ccs(ph.indicator = "not_an_indicator",
                                 ph.data = mock_chars,
                                 myinstructions = mock_instructions,
                                 chars.defs = mock_chars_def),
                   "not found in myinstructions")

      # Test invalid column in instructions
      bad_instructions <- copy(mock_instructions)
      bad_instructions[1, cat1_varname := "not_a_column"]

      expect_error(chi_chars_ccs(ph.indicator = "hos1803000_v1",
                                 ph.data = mock_chars,
                                 myinstructions = bad_instructions,
                                 chars.defs = mock_chars_def),
                   "don't exist in ph.data")
    })

# Test function handles ICD-9 data correctly ----
    test_that("chi_chars_ccs processes ICD-9 data correctly", {
      # Filter instructions to only include pre-2016 data
      icd9_instructions <- mock_instructions[end < 2016]

      # Run function
      result <- chi_chars_ccs(
        ph.indicator = "hos1803000_v1",
        ph.data = mock_chars,
        myinstructions = icd9_instructions,
        chars.defs = mock_chars_def
      )

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # Check that all data is from ICD-9 era (pre-2016)
      expect_true(all(grepl("201[3-5]", result$year)))

      # Check hospitalization count is as expected
      expect_equal(sum(result$hospitalizations), 100)

      # Check age filtering worked correctly
      expect_gt(max(as.numeric(result$chi_age)), 50) # is for all ages, so should definitely have some > 50 yrs old

      # Check correct indicator is used
      expect_true(all(result$indicator_key == "hos1803000_v1"))

      # Check if have the same number of observations for each age
      expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
    })

# Test function handles ICD-10 data correctly ----
    test_that("chi_chars_ccs processes ICD-10 data correctly", {
      # Filter instructions to only include post-2016 data
      icd10_instructions <- mock_instructions[start >= 2016]

      # Run function
      result <- chi_chars_ccs(
        ph.indicator = "hos1803000_v2",
        ph.data = mock_chars,
        myinstructions = icd10_instructions,
        chars.defs = mock_chars_def
      )

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # Check that all data is from ICD-10 era (2016+)
      expect_true(all(grepl("201[6-9]|202[0-2]", result$year)))

      # Check hospitalization count is as expected
      expect_lt(sum(result$hospitalizations), 100) # All ICD10 should be 100, so limiting to up to 17 should be less

      # Check age filtering worked correctly
      expect_equal(max(as.numeric(result$chi_age)), 17) # for children, so max is 17 yrs old

      # Check correct indicator is used
      expect_true(all(result$indicator_key == "hos1803000_v2"))

      # Check if have the same number of observations for each age
      expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
    })

# Test function handles mixed ICD-9/ICD-10 data correctly ----
    test_that("chi_chars_ccs processes mixed ICD-9/ICD-10 data correctly", {
      # Filter data to include instructions that span the 2016 transition
      mixed_instructions <- mock_instructions[start < 2016 & end >= 2016]

      # Run function
      result <- chi_chars_ccs(
        ph.indicator = "hos1803000_v1",
        ph.data = mock_chars,
        myinstructions = mixed_instructions,
        chars.defs = mock_chars_def
      )

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # Verify that year ranges are formatted correctly
      expect_true(all(grepl("^\\d{4}-\\d{4}$", result$year)))

      # Verify that the range spans the transition
      expect_true(any(grepl("201[3-5]-201[6-9]", result$year)))

      # Verify we have all cat1_group values
      expect_equal(unique(c("Hispanic", "Non-Hispanic")),  unique(result$cat1_group) )

      # Verify we have the proper year spans
      expect_equal(sort(unique(result$year)),
                   sort(unique(mixed_instructions[, year := paste0(start, '-', end)]$year)))

      # Check if have the same number of observations for each age
      expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
    })

# Test that instructions with different indicator variables work ----
    test_that("chi_chars_ccs handles different indicator variables correctly", {
      # Instructions with two category variables
      cat2_instructions <- mock_instructions[!is.na(cat2_varname)]

      # Run function
      result <- rbindlist(lapply(c("hos1803000_v1", "hos1803000_v2"), function(indicator) {
        chi_chars_ccs(
          ph.indicator = indicator,
          ph.data = mock_chars,
          myinstructions = cat2_instructions,
          chars.defs = mock_chars_def)
      }), fill = TRUE)

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # Check that both category values are correctly filled
      expect_true(all(!is.na(result$cat1_group)))
      expect_true(all(!is.na(result$cat2_group)))

      # Check all ph.indicator (indicator_keys) exist
      expect_equal(sort(unique(cat2_instructions$indicator_key)),
                   sort(unique(result$indicator_key)))

      # Check cat1_group and cat2_group are all there
      expect_equal(
        setorder(unique(result[, .(cat1_group, cat2_group)]), cat1_group, cat2_group),
        data.table(cat1_group = rep(c("Hispanic", "Non-Hispanic"), each = 2), cat2_group = rep(c("Female", "Male"), times = 2))
      )

      # Check if have the same number of observations for each age (within an indicator)
      expect_equal(uniqueN(result[indicator_key == "hos1803000_v1", .N, chi_age]$N), 1)
      expect_equal(uniqueN(result[indicator_key == "hos1803000_v2", .N, chi_age]$N), 1)
    })

# Test that WA state filtering works ----
    test_that("chi_chars_ccs handles WA state filtering correctly", {
      # Instructions for WA state
      wa_instructions <- mock_instructions[tab == "_wastate"]

      # Run function
      result <- chi_chars_ccs(
        ph.indicator = "hos1803000_v1",
        ph.data = mock_chars,
        myinstructions = wa_instructions,
        chars.defs = mock_chars_def
      )

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # Check that all rows are for WA state
      expect_true(all(result$tab == "_wastate"))

      # Check if have the same number of observations for each age
      expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
    })

# Test child-specific age filtering ----
    test_that("chi_chars_ccs handles age filtering correctly", {
      # Test children-only indicator (hos1803000_v2 has age_end = 17)
      result <- chi_chars_ccs(
        ph.indicator = "hos1803000_v2",
        ph.data = mock_chars,
        myinstructions = mock_instructions[indicator_key == "hos1803000_v2"],
        chars.defs = mock_chars_def
      )

      # Check if result has expected structure
      expect_true(is.data.table(result))
      expect_equal(names(result), expectedCols)

      # check correct age range
      expect_true(all(result$chi_age %in% 0:17))

      # Check if have the same number of observations for each age
      expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
    })

# Test what happens when instructions filter out all rows----
    test_that("When some instructions filter out all rows, expect it to work but returning warnings", {
      warnings <- capture_warnings({
        result <- rbindlist(lapply(c("hos1803000_v1", "hos1803000_v2"), function(indicator) {
          chi_chars_ccs(
            ph.indicator = indicator,
            ph.data = copy(mock_chars)[, chi_geo_kc := 'KC'],
            myinstructions = mock_instructions,
            chars.defs = mock_chars_def)
        }), fill = TRUE)
      })

      # Check that we got exactly 2 warnings
      expect_length(warnings, 2)

      # Check content of first warning
      expect_match(warnings[1],
                   "No data found for the following 2 instruction\\(s\\) for indicator 'hos1803000_v1'")

      # Check content of second warning
      expect_match(warnings[2],
                   "No data found for the following 1 instruction\\(s\\) for indicator 'hos1803000_v2'")

      expect_equal(  # expect three of the instructions 'worked' because _wastate was not corrupted
        3,
        uniqueN(result[, list(indicator_key, tab, cat1, cat1_varname, cat2, cat2_varname,
                              start = as.numeric(substr(year, 1, 4)),
                              end = as.numeric(substr(year, nchar(year) - 3, nchar(year))) )])
        )

    })
