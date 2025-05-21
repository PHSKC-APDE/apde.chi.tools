######################################### ----
# HELPER FUNCTIONS for chi_get_proper_pop ----
######################################### ----
# Input Validation: validate_inputs() ----
#' Input Validation: validate_inputs() ----
#' @noRd
  validate_inputs <- function(pop.template, pop.genders, pop.ages, is_chars) {
    # pop.template
    if (is.null(pop.template)) {
      stop("\n\U1F6D1 pop.template parameter is required")
    }

    if (!is.data.frame(pop.template)) {
      stop("\n\U1F6D1 pop.template must be a data.frame or data.table")
    } else {
      pop.template <- data.table::setDT(data.table::copy(pop.template))
    }

    # Check for required columns
    required_columns <- c("year", "cat1", "cat1_varname", "cat2", "cat2_varname",
                          "start", "stop", "race_type", "geo_type", "tab")
    missing_columns <- required_columns[!required_columns %in% names(pop.template)]

    if (length(missing_columns) > 0) {
      stop("\n\U1F6D1 pop.template is missing required columns: ",
           paste(missing_columns, collapse = ", "))
    }

    # pop.genders
    if (is.null(pop.genders)) {
      gender_values <- c("f", "m")
    } else {
      if (!tolower(pop.genders) %in% c('f', 'female', 'm', 'male')) {
        stop("\n\U0001f47f if pop.genders is specified, it is limited to one of the following values: 'F', 'f', 'Female', 'female', 'M', 'm', 'Male', or 'male'")
      } else {
        gender_values <- pop.genders
      }
    }

    # pop.ages
    if (is.null(pop.ages)) {
      age_values <- c(0:100)
    } else {
      if (!is.integer(pop.ages)) {
        stop("\n\U0001f47f if pop.ages is specified it must be vector of integers, e.g., c(0:65)")
      } else {
        age_values <- pop.ages
      }
    }

    # is_chars
    if(!is.logical(is_chars)) {
      stop("\n\U0001f47f The `is_chars` argument must be a logical, i.e., TRUE | FALSE")
    }
    if(length(is_chars) != 1) {
      stop("\n\U0001f47f The `is_chars` argument must be of length 1")
    }

    return(list(
      pop.template = pop.template,
      gender_values = gender_values,
      age_values = age_values,
      is_chars = is_chars
    ))
  }

# Standardize Category Names: standardize_category_names() ----
#' Standardize Category Names: standardize_category_names() ----
#' @noRd
  standardize_category_names <- function(pop.template) {
    # Remove birthing person prefixes to standardize maternal data categories
    pop.template[grepl("birthing person", cat1, ignore.case = TRUE),
                 cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
    pop.template[grepl("birthing person", cat2, ignore.case = TRUE),
                 cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

    return(pop.template)
  }

# Create Query Keys for Consolidation: create_query_keys() ----
#' Create Query Keys for Consolidation: create_query_keys() ----
#' @noRd
  create_query_keys <- function(pop.template) {
    # Create a key to identify unique population query patterns
    pop.template[, query_key := paste(
      cat1, cat1_varname, cat2, cat2_varname,
      race_type, geo_type,
      ifelse(is.na(group_by1), "NA", group_by1),
      ifelse(is.na(group_by2), "NA", group_by2),
      sep = "||"
    )]

    data.table::setorder(pop.template, query_key) # for consistent representative_row_index

    return(pop.template)
  }

# Batch Similar Querys: batch_population_queries() ----
#' Batch Similar Querys: batch_population_queries() ----
#' @noRd
  batch_population_queries <- function(pop.template) {
    # For each unique query key, find the min start year and max stop year
    batched_queries <- pop.template[, list(
      min_start = min(start),
      max_stop = max(stop),
      representative_row_index = .I[1] # Keep representative row per pattern
    ), by = query_key]

    # Add a unique ID for each batched query
    batched_queries[, batched_id := .I]

    # Join back to original template
    pop.template <- merge(
      pop.template,
      batched_queries[, list(query_key, batched_id, min_start, max_stop)],
      by = "query_key"
    )

    return(list(
      pop.template = pop.template,
      batched_queries = batched_queries
    ))
  }

# Get Population Data for a batched Query: get_batched_population() ----
#' Get Population Data for a batched Query: get_batched_population() ----
#' @noRd
   get_batched_population <- function(query_id,
                                     pop.template,
                                     batched_queries,
                                     gender_values,
                                     age_values,
                                     is_chars) {
    # Get the batched query info
    current_query <- batched_queries[batched_id == query_id]

    # Get a representative row from the original template
    template_row <- pop.template[current_query$representative_row_index]

    # Show progress
    message(paste0("Process ID ", Sys.getpid(), ": get_population call ",
               query_id, " (covers ", sum(pop.template$batched_id == query_id),
               " original queries: ", current_query$query_key, ")"))

    # Build grouping parameters for population query
    grouping_vars <- unique(c(
      c("ages", 'years', "geo_id"),
      setdiff(c(template_row$group_by1, template_row$group_by2), c(NA))
    ))

    # Call get_population with the batched year range
    if (is_chars && template_row$geo_type == 'kc') {
      # For CHARS data, use ZIP aggregation method for King County
      population_data <- rads::get_population(
        kingco = FALSE, # intentionally FALSE to retrieve ALL ZIP codes, which we'll filter later
        group_by = grouping_vars,
        geo_type = 'zip', # note this is purposefully not 'kc'
        race_type = template_row$race_type,
        years = current_query$min_start:current_query$max_stop,
        genders = gender_values,
        ages = age_values,
        round = FALSE
      )

      # Filter to Define King County as ZIPs that begin with 980/981
      population_data <- population_data[grepl('^980|^981', geo_id)]

      # Sum the population across all ZIP codes while maintaining other dimensions
      group_cols <- setdiff(names(population_data), c("geo_id", "pop"))
      population_data <- population_data[, list(pop = sum(pop)), by = group_cols]

      # Add King County identifier for consistent processing
      population_data[, geo_type := 'kc']
      population_data[, geo_id := 'King County']
    } else if (is.na(template_row$geo_type)) {
      population_data <- rads::get_population(
        group_by = grouping_vars,
        race_type = template_row$race_type,
        years = current_query$min_start:current_query$max_stop,
        genders = gender_values,
        ages = age_values,
        round = FALSE
      )
    } else {
      population_data <- rads::get_population(
        group_by = grouping_vars,
        geo_type = template_row$geo_type,
        race_type = template_row$race_type,
        years = current_query$min_start:current_query$max_stop,
        genders = gender_values,
        ages = age_values,
        round = FALSE
      )
    }

    # Add batched_id to population data for later processing
    population_data[, batched_id := query_id]

    return(population_data)
  }

# Process Age Group Patterns: process_age_patterns() ----
#' Process Age Group Patterns: process_age_patterns() ----
#' @noRd
  process_age_patterns <- function(age_var) {
    # Check if this age variable exists in misc_chi_byvars
    chi_age_groups <- rads.data::misc_chi_byvars[cat == "Age" & varname == age_var]

    if (nrow(chi_age_groups) == 0) {
      stop(paste0("\n\U1F6D1 Age variable ", age_var, " not found in rads.data::misc_chi_byvars[cat == \"Age\"]"))
    }

    # Create a result table with age ranges
    age_ranges <- data.table(group_value = character(), min_age = numeric(), max_age = numeric())

    # Process each age group
    for (g in unique(chi_age_groups$group)) {
      # Pattern: "<#" (e.g., "<18")
      if (grepl("^<\\d+$", g)) {
        max_age <- as.numeric(gsub("<", "", g)) - 1
        age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = 0, max_age = max_age))
      }
      # Pattern: "#-#" (e.g., "18-24")
      else if (grepl("^\\d+-\\d+$", g)) {
        range_parts <- as.numeric(strsplit(g, "-")[[1]])
        min_age <- range_parts[1]
        max_age <- range_parts[2]
        age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = min_age, max_age = max_age))
      }
      # Pattern: "#+", (e.g., "75+")
      else if (grepl("^\\d+\\+$", g)) {
        min_age <- as.numeric(gsub("\\+", "", g))
        age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = min_age, max_age = Inf))
      }
      # Other valid groups like "All", "Adults", "Children", "Seniors"
      else if (g %in% c("All", "Adults", "Children", "Seniors")) {
        if (g == "All") {
          age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = 0, max_age = Inf))
        } else if (g == "Adults") {
          age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = 18, max_age = Inf))
        } else if (g == "Children") {
          age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = 0, max_age = 17))
        } else if (g == "Seniors") {
          age_ranges <- rbind(age_ranges, data.table(group_value = g, min_age = 65, max_age = Inf))
        }
      }
      # Error for unrecognized patterns
      else {
        stop(paste0("\n\U1F6D1 Age group ", g, " in age_var = ", age_var,
                    " does not follow the expected pattern (<#, #-#, or #+) and cannot be used",
                    "\n Valid options are found in rads.data::misc_chi_byvars[cat == \"Age\"]"))
      }
    }

    return(age_ranges)
  }

# Assign Race/Ethnicity Categories: assign_race_ethnicity() ----
#' Assign Race/Ethnicity Categories: assign_race_ethnicity() ----
#' @noRd
  assign_race_ethnicity <- function(population_data, catnum, catvarname, catgroup, race_type) {
    # Standardize race_eth into race
    if('race' %in% names(population_data)) {
      population_data[, race := as.character(race)]
    }

    if('race_eth' %in% names(population_data)) {
      population_data[, race_eth := as.character(race_eth)]
    }

    if('race' %in% names(population_data) & 'race_eth' %in% names(population_data)) {
      population_data[is.na(race) | race == 'NA', race := race_eth]
    }

    if(!'race' %in% names(population_data) & 'race_eth' %in% names(population_data)) {
      data.table::setnames(population_data, 'race_eth', 'race')
    }

    # Process specific race/ethnicity groups
    population_data[get(catvarname) %in% c('race4', 'race3'), (catgroup) := race]

    # Filter out Ethnicity unless the group == 'Hispanic'
    population_data <- population_data[get(catnum) != "Ethnicity" |
                                         (get(catnum) == "Ethnicity" &
                                            get(catgroup) == 'Hispanic'), ]

    # Standardize "Multiple" label
    population_data[get(catgroup) == "Multiple race", (catgroup) := "Multiple"]

    # Process race_aic (alone or in combination) categories
    if (race_type == 'race_aic') {
      # Filter to keep only relevant race_aic combinations
      population_data <- population_data[
        !(grepl('_aic_', get(catvarname)) &
            !((get(catvarname) == 'chi_race_aic_aian' & race_aic == 'AIAN') |
                (get(catvarname) == 'chi_race_aic_asian' & race_aic == 'Asian') |
                (get(catvarname) == 'chi_race_aic_black' & race_aic == 'Black') |
                (get(catvarname) == 'chi_race_aic_his' & race_aic == 'Hispanic') |
                (get(catvarname) == 'chi_race_aic_nhpi' & race_aic == 'NHPI') |
                (get(catvarname) == 'chi_race_aic_wht' & race_aic == 'White'))
        )
      ]

      # Assign race_aic value to group
      population_data[grep('_aic', get(catvarname)), (catgroup) := race_aic]
    }

    return(population_data)
  }

# Assign Age Groupings using misc_chi_byvars:assign_age_groupings() ----
#' Assign Age Groupings using misc_chi_byvars:assign_age_groupings() ----
#' @noRd
  assign_age_groupings <- function(population_data, catnum, catvarname, catgroup) {
    # Check if this is an age category
    if (population_data[1, get(catnum)] == "Age") {
      # Get the age variable name
      age_var <- population_data[1, get(catvarname)]

      # Get age ranges
      age_ranges <- process_age_patterns(age_var)

      # Assign each age range to the population data
      for (i in 1:nrow(age_ranges)) {
        group_value <- age_ranges[i, group_value]
        min_age <- age_ranges[i, min_age]
        max_age <- age_ranges[i, max_age]

        if (is.infinite(max_age)) {
          population_data[get(catvarname) == age_var & age >= min_age, (catgroup) := group_value]
        } else {
          population_data[get(catvarname) == age_var & age >= min_age & age <= max_age,
                          (catgroup) := group_value]
        }
      }
    }

    return(population_data)
  }

# Assign Geographic Crosswalks: assign_geographic_crosswalks() ----
#' Assign Geographic Crosswalks: assign_geographic_crosswalks() ----
#' @noRd
  assign_geographic_crosswalks <- function(population_data, catnum, catgroup, catvarname, geo_type) {
    # Process HRAs
    if (geo_type == 'blk' && population_data[1, get(catnum)] == 'Cities/neighborhoods') {
      hra_crosswalk <- rads.data::spatial_block20_to_hra20_to_region20[,
                                                                       list(geo_id = GEOID20,
                                                                            hra20_name)]

      population_data <- merge(population_data,
                               hra_crosswalk,
                               by = "geo_id",
                               all.x = TRUE,
                               all.y = FALSE)

      population_data[, (catgroup) := hra20_name]
    }

    # Process Region crosswalks
    # Block to Region
    if (geo_type == 'blk' && population_data[1, get(catnum)] == 'Regions') {
      region_crosswalk <- rads.data::spatial_block20_to_hra20_to_region20[,list(geo_id = GEOID20, region_name)]

      population_data <- merge(population_data,
                               region_crosswalk,
                               by = 'geo_id',
                               all.x = TRUE,
                               all.y = FALSE)

      population_data[, (catgroup) := region_name]
    }

    # HRA to Region
    if (geo_type == 'hra' && population_data[1, get(catnum)] == 'Regions') {
      region_crosswalk <- rads.data::spatial_hra20_to_region20[, list(geo_id = hra20_name, region_name)]

      population_data <- merge(population_data,
                               region_crosswalk,
                               by = 'geo_id',
                               all.x = TRUE,
                               all.y = FALSE)

      population_data[, (catgroup) := region_name]
    }

    # ZIP to Region with population weighting
    if (geo_type == 'zip' && population_data[1, get(catnum)] == 'Regions') {
      # Create ZIP to region crosswalk with population weights
      zip_region_crosswalk <- rads.data::spatial_zip_to_hra20_pop

      zip_region_crosswalk <- merge(zip_region_crosswalk,
                                    rads.data::spatial_hra20_to_region20[, list(hra20_name, region = region_name)],
                                    by = 'hra20_name',
                                    all = TRUE)

      # Aggregate fractional populations to region level
      zip_region_crosswalk <- zip_region_crosswalk[,
                                                   list(s2t_fraction = sum(s2t_fraction)),
                                                   list(geo_id = as.character(source_id), region)]

      # Assign population weighting by region
      population_data <- merge(population_data,
                               zip_region_crosswalk,
                               by = "geo_id",
                               all.x = TRUE,
                               all.y = FALSE,
                               allow.cartesian = TRUE)
      population_data[, pop := pop * s2t_fraction] # Assign weight to population
      population_data[, (catgroup) := region]
    }

    # Process Big Cities
    if (population_data[1, get(catnum)] == 'Big cities') {
      # Block to big city crosswalk
      if (geo_type == 'blk') {
        # Two-step crosswalk: block to HRA to big city
        block_to_hra <- rads.data::spatial_block20_to_hra20_to_region20[, list(geo_id = GEOID20, hra20_name)]
        hra_to_bigcity <- rads.data::spatial_hra20_to_bigcities[, list(hra20_name, bigcity)]

        block_to_bigcity <- merge(hra_to_bigcity,
                                  block_to_hra,
                                  by = 'hra20_name',
                                  all.x = TRUE,
                                  all.y = FALSE)[, hra20_name := NULL]

        population_data <- merge(population_data,
                                 block_to_bigcity,
                                 by = "geo_id",
                                 all.x = TRUE,
                                 all.y = FALSE)
      }

      # HRA to big city crosswalk
      if (geo_type == 'hra') {
        hra_to_bigcity <- rads.data::spatial_hra20_to_bigcities[, list(hra20_name, bigcity)]
        population_data <- merge(population_data,
                                 hra_to_bigcity,
                                 by.x = 'geo_id',
                                 by.y = 'hra20_name',
                                 all.x = TRUE,
                                 all.y = FALSE)
      }

      # Assign big city name to group
      population_data[, (catgroup) := bigcity]
    }

    # Process poverty groupings
    # Block level poverty
    if (geo_type == 'blk' && population_data[1, get(catvarname)] == 'pov200grp') {
      # Extract tract ID from block ID (first 11 characters)
      population_data[, geo_tract2020 := substr(geo_id, 1, 11)]

      # Join poverty group data
      population_data <- merge(
        population_data,
        rads.data::misc_poverty_groups[geo_type == 'Tract'][, list(geo_tract2020 = geo_id, pov200grp)],
        by = "geo_tract2020",
        all.x = TRUE,
        all.y = FALSE
      )

      # Assign poverty group
      population_data[, (catgroup) := pov200grp]
    }

    # ZIP level poverty
    if (geo_type == 'zip' && population_data[1, get(catvarname)] == 'pov200grp') {
      # Join poverty group data
      population_data <- merge(
        population_data,
        rads.data::misc_poverty_groups[geo_type == 'ZCTA'][,list(geo_id, pov200grp)],
        by = 'geo_id',
        all.x = TRUE,
        all.y = FALSE
      )

      # Assign poverty group
      population_data[, (catgroup) := pov200grp]
    }

    return(population_data)
  }

# Create demographic shell for population analysis: create_demographic_shell() ----
#' Create demographic shell for population analysis: create_demographic_shell() ----
#' @noRd
  create_demographic_shell <- function(population_data, template_row, age_values = NULL) {
    # Use age_values if provided, otherwise default to 0:100
    age_range <- if(!is.null(age_values)) age_values else 0:100

    # Function to handle age group processing
    process_age_category <- function(population_data, cat_num) {
      # Define prefix and complementary prefix
      cat_prefix <- paste0("cat", cat_num)
      other_cat_prefix <- if(cat_num == 1) "cat2" else "cat1"

      # Get the variable name for this category
      cat_varname <- population_data[1][[paste0(cat_prefix, "_varname")]]

      # Create a data table with all possible ages
      age_groups <- data.table(chi_age = age_range) # age_range is defined above in create_demographic_shell

      # Get age ranges
      age_ranges <- process_age_patterns(cat_varname)

      # Assign each age range to the age groups table
      for (i in 1:nrow(age_ranges)) {
        group_value <- age_ranges[i, group_value]
        min_age <- age_ranges[i, min_age]
        max_age <- age_ranges[i, max_age]

        if (is.infinite(max_age)) {
          age_groups[chi_age >= min_age, paste0(cat_prefix, "_group") := group_value]
        } else {
          age_groups[chi_age >= min_age & chi_age <= max_age,
                     paste0(cat_prefix, "_group") := group_value]
        }
      }

      # Filter out ages without assigned groups
      age_groups <- age_groups[!is.na(get(paste0(cat_prefix, "_group")))]

      # Add category info
      age_groups[, (cat_prefix) := "Age"]
      age_groups[, paste0(cat_prefix, "_varname") := cat_varname]

      # Combine demographic dimensions with age groups
      cols_to_select <- c("year", other_cat_prefix,
                          paste0(other_cat_prefix, "_varname"),
                          paste0(other_cat_prefix, "_group"))

      unique_pop_data <- unique(population_data[, cols_to_select, with = FALSE][, mykey := 1])

      complete_demographics <- merge(unique_pop_data,
                                     age_groups[, mykey := 1],
                                     by = 'mykey',
                                     allow.cartesian = TRUE)

      complete_demographics[, mykey := NULL]

      return(complete_demographics)
    }

    # Use function to handle age group processing
    if (population_data[1]$cat1 == "Age") {
      complete_demographics <- process_age_category(population_data, 1)
    } else if (population_data[1]$cat2 == "Age") {
      complete_demographics <- process_age_category(population_data, 2)
    } else {
      # Get unique cat1 groups
      cat1_groups <- unique(population_data[, list(cat1, cat1_varname, cat1_group, mykey = 1)])

      # Get unique cat2 groups
      cat2_groups <- unique(population_data[, list(cat2, cat2_varname, cat2_group, mykey = 1)])

      # All combos of cat1 and cat2 groups
      complete_demographics <- merge(cat1_groups,
                                     cat2_groups,
                                     by = 'mykey',
                                     allow.cartesian = TRUE)

      # Create year and age combos
      year_age <- data.table(year = as.character(template_row$year),
                             chi_age = age_range,
                             mykey = 1)

      # Get combos for each year/age cat1/cat2 combo
      complete_demographics <- merge(complete_demographics,
                                     year_age,
                                     by = 'mykey',
                                     allow.cartesian = TRUE)

      # Drop key
      complete_demographics[, mykey := NULL]
    }

    return(complete_demographics)
  }

# Process a single template row: process_template_row() ----
#' Process a single template row: process_template_row() ----
#' @noRd
  process_template_row <- function(row_index, population_data, pop.template, age_values = NULL) {
    # Basic subsetting tidying ----
      current_row <- pop.template[row_index, ]

      # Set the correct year format
      population_data[, year := current_row$year]

    # Process demographic categories ----
      for (catnum in c("cat1", "cat2")) {
        # Define variable names
        catvarname <- paste0(catnum, '_varname')
        catgroup <- paste0(catnum, '_group')
        temp.groupby <- paste0("group_by", gsub('cat', '', catnum))

        # Set basic category info from template
        population_data[, (catnum) := current_row[[catnum]]]
        population_data[, (catvarname) := current_row[[catvarname]]]
        population_data[, (catgroup) := current_row[[temp.groupby]]]

        # Process standard geographic categories
        # King County
        population_data[get(catnum) == "King County", (catgroup) := "King County"]

        # Washington State
        population_data[get(catnum) == "Washington State", (catgroup) := "Washington State"]

        # Handle NA values
        suppressWarnings(
          population_data[get(catnum) == "NA" | is.na(get(catnum)),
                          c(catnum, catgroup, catvarname) := "NA"]
        )

        # Cities/neighborhoods and Regions
        population_data[get(catnum) %in% c("Cities/neighborhoods", "Regions") &
                          current_row$geo_type != 'blk', (catgroup) := geo_id]

        # Process gender
        population_data[get(catnum) %in% c("Gender"), (catgroup) := gender]

        # Process 'Overall'
        population_data[get(catnum) %in% c("Overall"), (catgroup) := "Overall"]

        # Process race/ethnicity categories
        if (!is.na(population_data[1, get(catnum)]) &&
            (population_data[1, get(catnum)] %in% c("Race", "Race/ethnicity", "Race/Ethnicity", "Ethnicity") ||
             (current_row$race_type == 'race_aic' && grepl('_aic_', get(catvarname))))) {
                population_data <- assign_race_ethnicity(population_data,
                                                         catnum,
                                                         catvarname,
                                                         catgroup,
                                                         current_row$race_type)
        }

        # Assign geographic crosswalks
        if (!is.na(population_data[1, get(catnum)]) &&
            (population_data[1, get(catnum)] %in% c("Cities/neighborhoods", "Regions", "Big cities") ||
             population_data[1, get(catvarname)] == 'pov200grp')) { # pov200grp is neighborhood poverty, so geography based
                population_data <- assign_geographic_crosswalks(population_data,
                                                                catnum,
                                                                catgroup,
                                                                catvarname,
                                                                current_row$geo_type)
        }

        # Assign age groupings
        if (!is.na(population_data[1, get(catnum)]) && population_data[1, get(catnum)] == "Age") {
          population_data <- assign_age_groupings(population_data,
                                                  catnum,
                                                  catvarname,
                                                  catgroup)
          }

      }

    # Filter and clean results ----
      # Remove rows with missing primary category group
      population_data <- population_data[!is.na(cat1_group)]

      # Remove rows with missing secondary category group (when category exists)
      population_data <- population_data[is.na(cat2) | cat2 == 'NA' |
                                           (!is.na(cat2) & cat2 != 'NA' &
                                              !is.na(cat2_group) & cat2_group != 'NA'),]

      # Aggregate population data
      population_data <- population_data[, list(pop = sum(pop)),
                                         list(chi_age = age,
                                              year,
                                              cat1, cat1_varname, cat1_group,
                                              cat2, cat2_varname, cat2_group)]

      # Generate complete demographic combinations
      complete_demographics <- create_demographic_shell(population_data, current_row, age_values)


      # Merge population data with complete demographics grid
      population_data <- suppressWarnings(merge(population_data,
                                                complete_demographics,
                                                all = TRUE))

      # Fill missing population values with zero
      population_data[is.na(pop), pop := 0]

      # Add tab column
      population_data[, tab := current_row$tab]

      # Convert placeholder "NA" strings back to true NA values
      population_data[cat2 == "NA", c("cat2", "cat2_varname", "cat2_group") := NA]

    return(population_data)
  }

