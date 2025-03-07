#' Generate Age-Specific Counts for Community Health Indicators
#'
#' @description
#' Creates a detailed breakdown of counts by age for CHI data analysis. Primarily
#' used for age standardization and rate calculations when combined with
#' population estimates. Processes data according to provided instructions and
#' handles demographic groupings with special treatment for race and ethnicity
#' variables.
#'
#' @param ph.data Input data frame or data table containing CHI data
#' @param ph.instructions Data frame or data table containing calculation specifications with columns:
#'  \itemize{
#'    \item indicator_key: Name of the health metric to calculate
#'    \item tab: Visualization tab type (kingcounty, demgroups, crosstabs)
#'    \item cat1_varname, cat2_varname: Variable names for stratification
#'    \item cat1, cat2: Human-friendly labels for these variables
#'    \item start, end: Year range for the calculation
#'  }
#' @param source_date Date of data source, added to output metadata
#'
#' @return A data.table containing age-specific counts with standard CHI groupings:
#'  \itemize{
#'    \item indicator_key: Health metric identifier
#'    \item year: Year range of data (e.g., "2019-2021" or single year)
#'    \item tab: Visualization tab type
#'    \item cat1, cat1_varname, cat1_group: Primary stratification variable details
#'    \item cat2, cat2_varname, cat2_group: Secondary stratification variable details (if applicable)
#'    \item chi_age: Age value (0-100)
#'    \item count: Number of cases in that demographic-age group
#'    \item source_date: Date of data source (if provided)
#'  }
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}} which creates ph.instructions used by
#' \code{chi_count_by_age}
#'
#' \code{\link{chi_generate_instructions_pop}} which uses the output of the output
#' of \code{chi_count_by_age}
#'
#' @importFrom data.table setDT rbindlist setnames := setorder data.table CJ
#' @importFrom rads calc
#' @importFrom future.apply future_lapply
#' @importFrom progressr handlers progressor with_progress
#' @export
#'
chi_count_by_age <- function(ph.data = NULL,
                             ph.instructions = NULL,
                             source_date = NULL) {

  # Basic validation of inputs ----
    if (is.null(ph.data) || is.null(ph.instructions)) {
      stop("Both ph.data and ph.instructions parameters are required")
    }

    if (!is.data.frame(ph.data)) stop("\n\U1F6D1 ph.data must be a data.frame or data.table")
    if (!is.data.frame(ph.instructions)) stop("\n\U1F6D1 ph.instructions must be a data.frame or data.table")
    if (!is.null(source_date) & !inherits(source_date, "Date")) stop("\n\U1F6D1 source_date must be of type Date, if it is provided")

    # Convert inputs to data.table if they're not already
    ph.data <- setDT(copy(ph.data))
    ph.instructions <- setDT(copy(ph.instructions))

  # Create 'Overall' category if needed for crosstabs ----
    if (!"overall" %in% names(ph.data)) {
      ph.data[, overall := ifelse(chi_geo_kc == "King County", "Overall", NA_character_)]
    }

  # Check for required variables in the data ----
    # Extract all variable names needed from instructions
      needed_byvars <- setdiff(
        unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)),
        c(NA)
      )

    # Special case: race3 requires race3_hispanic by definition
      # race3 in the Tableau viz is presented as a single variables, but in reality
      # it is made up of a race indicator and a hispanic ethnicity indicator. So,
      # when instructions request race3, they need race3_hispanic as well to calculate
      # the final race3 values (which is race, with Hispanic as ethnicity)
      if ("race3" %in% needed_byvars & !"race3_hispanic" %in% needed_byvars) {
        needed_byvars <- c(needed_byvars, "race3_hispanic")
      }

    # Combine byvar names with indicator keys to get all required variables
      needed_vars <- setdiff(
        unique(c(ph.instructions$indicator_key, needed_byvars)),
        c(NA)
      )

    # Check if any required variables are missing from the data
      missing_vars <- setdiff(needed_vars, names(ph.data))
      if (length(missing_vars) > 0) {
        stop(paste0(
          "\n\U2620 ph.data is missing the following columns that are specified in ph.instructions: ",
          paste0(missing_vars, collapse = ", "), ". ",
          "\nIf `race3_hispanic` is listed, that is because, by definition, `race3` cannot have a Hispanic ethnicity in the same variable. ",
          "\nTwo variables (`race3` & `race3_hispanic`) will be processed and in the output, it will be called `race3`"
        ))
      } else {
        message("\U0001f642 All specified variables exist in ph.data")
      }

  # Verify that categorical variables follow CHI encoding standards ----
    # Get standardized byvar values from reference data
      std_byvars <- rads.data::misc_chi_byvars
      std_byvars <- std_byvars[varname %in% setdiff(unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)), c(NA))]
      std_byvars <- std_byvars[, list(varname, group, keepme, reference = 1)]

    # Handle race3 and Hispanic special case
      std_byvars[group %in% c("Hispanic", "Non-Hispanic") & varname == "race3",
                 varname := "race3_hispanic"]

    # Extract unique values for each byvar in the actual data
      data_byvars <- rbindlist(lapply(
        X = as.list(needed_byvars),
        FUN = function(x) {
          data.table(
            varname = x,
            group = setdiff(unique(ph.data[[x]]), NA),
            ph.data = 1
          )
        }
      ))

    # Merge actual values to reference standards
      byvar_comparison <- merge(std_byvars,
                                data_byvars,
                                by = c("varname", "group"),
                                all = TRUE)

    # Check for any mismatches between data and reference standards
      if (nrow(byvar_comparison[is.na(reference) | is.na(ph.data)]) > 0) {
        print(byvar_comparison[is.na(reference) | is.na(ph.data)])
        stop("\n\U2620 The table above shows the varname/group combinations that do not align between the reference table and your ph.data.")
      } else {
        message("\U0001f642 All specified cat1_group and cat2_group values align with the reference standard.")
      }

  # Generate counts for each row in instructions ----
    message("\U023F3 Be patient! The function is generating counts for each row of ph.instructions.")

      progressr::handlers(handler_progress())
      with_progress({
        p <- progressor(nrow(ph.instructions))
        count_results <- rbindlist(future_lapply(
          X = as.list(seq_len(nrow(ph.instructions))),
          FUN = function(row_idx) {
            p(paste0("Processing row ", row_idx, " of ", nrow(ph.instructions) ))
            # Set up calculations ----
              # Extract parameters for this calculation
              current_instruction <- ph.instructions[row_idx]
              primary_byvar <- current_instruction[["cat1_varname"]]
              secondary_byvar <- current_instruction[["cat2_varname"]]
              if (is.null(secondary_byvar) || length(secondary_byvar) == 0) {
                secondary_byvar <- NA
              }

              # Combine all byvars including age
              all_byvars <- setdiff(c(primary_byvar, secondary_byvar), c(NA))
              all_byvars <- unique(c(all_byvars, "chi_age"))

            # Calculate counts using rads::calc ----
              if (any(grepl("wastate", all_byvars))) {
                # Washington state calculations
                age_counts <- rads::calc(
                  ph.data = ph.data[chi_year >= current_instruction[["start"]] & chi_year <= current_instruction[["end"]]],
                  what = current_instruction[["indicator_key"]],
                  by = all_byvars,
                  metrics = c("numerator")
                )
              } else {
                # King County calculations
                age_counts <- rads::calc(
                  ph.data = ph.data[chi_year >= current_instruction[["start"]] & chi_year <= current_instruction[["end"]] & chi_geo_kc == "King County"],
                  what = current_instruction[["indicator_key"]],
                  by = all_byvars,
                  metrics = c("numerator")
                )
              }

            # Add cat1/cat2 information to results ----
              # Add cat1# info
              age_counts[, cat1 := current_instruction[["cat1"]]]
              setnames(age_counts, primary_byvar, "cat1_group")
              age_counts[, cat1_varname := primary_byvar]

              # Add cat2# info
              age_counts[, cat2 := current_instruction[["cat2"]]]
              if (!is.na(secondary_byvar) & primary_byvar != secondary_byvar) {
                setnames(age_counts, secondary_byvar, "cat2_group")
              } else {
                age_counts[, cat2_group := NA]
              }
              age_counts[, cat2_varname := secondary_byvar]

              # Filter out invalid combinations
              age_counts <- age_counts[!is.na(cat1_group)]
              age_counts <- age_counts[!(is.na(cat2_group) & !is.na(cat2))]
              age_counts <- age_counts[!is.na(chi_age)]

              # Keep only necessary columns
              age_counts <- age_counts[, list(
                cat1, cat1_varname, cat1_group,
                cat2, cat2_varname, cat2_group,
                chi_age, count = numerator
              )]

            # Create complete reference table for all combinations ----
              # When there is no data for a specific combination (e.g., NHPI 85+ in
              # particular geography), the row will be missing. We want the row to be
              # noted but with the count of 0, not NA.

              # cat1 summary table
              cat1_table <- data.table(
                cat1 = current_instruction[["cat1"]],
                cat1_varname = primary_byvar,
                cat1_group = sort(setdiff(as.character(unique(ph.data[[primary_byvar]])), NA))
              )

              # cat2 summary table
              if (!is.na(secondary_byvar)) {
                cat2_table <- data.table(
                  cat2 = current_instruction[["cat2"]],
                  cat2_varname = secondary_byvar,
                  cat2_group = sort(setdiff(as.character(unique(ph.data[[secondary_byvar]])), NA))
                )
              } else {
                cat2_table <- data.table(
                  cat2 = current_instruction[["cat2"]],
                  cat2_varname = secondary_byvar,
                  cat2_group = NA_character_
                )
              }

              # Create all logical combinations of cat1_table rows & cat2_table rows
                # This approach preserves the correct relationship between variable
                # names and their values. For example, if cat1_varname is 'chi_sex' then
                # cat1_group must only contain values like 'Male' or 'Female', not values
                # from other variables like 'Asian' or 'NHPI'.
                #
                # A simpler approach (like CJ with all columns directly) would create
                # invalid combinations such as cat1_varname='chi_sex' with cat1_group='Hispanic',
                # which is nonsensical.
                #
                # The nested lapply approach ensures that each category variable is only
                # paired with its own valid values while still creating all valid combinations
                # needed for the complete age-specific count table.

              all_combinations <- NULL
              if (nrow(cat2_table) > 0) {
                # For each row in cat1_table, combine with each row in cat2_table
                all_combinations <- rbindlist(lapply(1:nrow(cat1_table), function(i) {
                  rbindlist(lapply(1:nrow(cat2_table), function(j) {
                    data.table(
                      cat1 = cat1_table[i]$cat1,
                      cat1_varname = cat1_table[i]$cat1_varname,
                      cat1_group = cat1_table[i]$cat1_group,
                      cat2 = cat2_table[j]$cat2,
                      cat2_varname = cat2_table[j]$cat2_varname,
                      cat2_group = cat2_table[j]$cat2_group
                    )
                  }))
                }))
              } else {
                all_combinations <- cat1_table
                all_combinations[, `:=`(
                  cat2 = current_instruction[["cat2"]],
                  cat2_varname = secondary_byvar,
                  cat2_group = NA_character_
                )]
              }

              # Now add all ages to create the final cartesian product
              all_combinations_with_age <- CJ(
                combo_idx  = 1:nrow(all_combinations),
                chi_age = 0:100,
                unique = TRUE
              )

              all_combinations_with_age <- merge(
                all_combinations_with_age,
                all_combinations[, combo_idx  := .I],
                by = "combo_idx"
              )

              all_combinations_with_age[, combo_idx  := NULL]


            # Merge counts onto the reference table with all possible combinations ----
              complete_counts <- merge(
                all_combinations_with_age,
                age_counts,
                by = c("cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group", "chi_age"),
                all.x = TRUE
              )

              # Replace NA counts with 0 (needed for age-adjusted rates)
              complete_counts[is.na(count), count := 0]

              # Add remaining identifiers
              complete_counts[, indicator_key := current_instruction[["indicator_key"]]]
              complete_counts[, tab := current_instruction[["tab"]]]

              # Format year range appropriately
              if (current_instruction[["end"]] != current_instruction[["start"]]) {
                complete_counts[, year := paste0(current_instruction[["start"]], "-", current_instruction[["end"]])]
              } else {
                complete_counts[, year := as.character(current_instruction[["start"]])]
              }

              # Order and select final output columns
              complete_counts <- complete_counts[, list(
                indicator_key, year, tab,
                cat1, cat1_varname, cat1_group,
                cat2, cat2_varname, cat2_group,
                chi_age, count
              )]

              setorder(complete_counts, cat1_group, cat2_group, chi_age)
              return(complete_counts)
            } # close function within future_lapply
          ), # close future_lapply
          use.names = TRUE) # close rbindlist
      }) # close with_progress

    # Add source_date if provided
    if (!is.null(source_date)) {
      count_results[, source_date := source_date]
    } else {
      count_results[, source_date := as.Date(NA_character_)]
    }

  # Modify race3 / race3_hispanic ----
    # as noted above, these are two distinct variables that are presented as a single
    # variable in Tableau viz so the variable names need to be harmonized
      count_results[cat1_varname == 'race3_hispanic', cat1_varname := 'race3']
      count_results[cat2_varname == 'race3_hispanic', cat2_varname := 'race3']
      drop_race3_groups <- rads.data::misc_chi_byvars[varname == 'race3' & keepme == 'No']$group
      count_results <- count_results[!(cat1_varname == 'race3' & cat1_group %in% drop_race3_groups)]
      count_results <- count_results[is.na(cat2) | !(cat2_varname == 'race3' & cat2_group %in% drop_race3_groups)]

  # Return the final results ----
  return(count_results)
}
