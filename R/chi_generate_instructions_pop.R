#' Generate Population Instructions for CHI Analysis
#'
#' @description
#' Creates instructions for \code{\link{chi_get_proper_pop}} based on a table of count
#' data. These instructions configure appropriate demographic groupings,
#' geographic types, and time periods for retrieving population denominators used
#' in CHI rate calculations.
#'
#' @param mycount.data Input data.table produced by \code{\link{chi_count_by_age}},
#' containing the following columns:
#'  \itemize{
#'    \item indicator_key: indicator_key used by CHI
#'    \item year: Year range (e.g., "2019-2021" or single year)
#'    \item tab: Visualization tab type
#'    \item cat1, cat1_varname, cat1_group: Primary stratification variables
#'    \item cat2, cat2_varname, cat2_group: Secondary stratification variables
#'    \item chi_age: Single year age
#'    \item count: Count of events (births, death, hospitalizations, etc. )
#'  }
#' @param povgeo Geographic level for poverty analysis ('blk' or 'zip')
#'
#' @return A data.table containing population processing instructions with columns:
#'  \itemize{
#'    \item year: Original year range from input
#'    \item cat1, cat1_varname: Primary stratification details
#'    \item cat2, cat2_varname: Secondary stratification details
#'    \item tab: Visualization tab type
#'    \item start, stop: Start and end years parsed from the year range
#'    \item race_type: Race categorization type ('race', 'race_eth', or 'race_aic')
#'    \item geo_type: Geographic level for analysis ('kc', 'hra', 'region', 'blk', 'zip', 'wa')
#'    \item group_by1, group_by2: Demographic grouping specifications
#'  }
#'
#' @seealso
#' \code{\link{chi_count_by_age}} which generates the count data used as input
#' to this function
#'
#' \code{\link{chi_get_proper_pop}} which uses the output of this function
#'
#' @importFrom data.table copy `:=` setorder tstrsplit
#' @importFrom tools toTitleCase
#' @export
#'
chi_generate_instructions_pop <- function(mycount.data,
                                          povgeo = c('blk', 'zip')) {

  # Validation of inputs ----
    if (is.null(mycount.data)) {
      stop("\n\U1F6D1 mycount.data parameter is required")
    }

    if (!is.data.frame(mycount.data)) {
      stop("\n\U1F6D1 mycount.data must be a data.frame or data.table")
    } else {mycount.data <- setDT(copy(mycount.data))}

    povgeo <- match.arg(povgeo)

  # Initial data preparation ----
    # Create a template with only the necessary columns to avoid duplicates
    pop.template <- copy(mycount.data)
    pop.template <- unique(copy(pop.template)[, list(year, cat1, cat1_varname, cat2, cat2_varname, tab)])

  # Process year ranges ----
    # Split year ranges (e.g., "2019-2021") into start and stop years
    pop.template[, c("start", "stop") := tstrsplit(year, split = '-')]
    # For single years, set the stop year equal to the start year
    pop.template[is.na(stop), stop := start]

  # Set default demographic settings ----
    # Default race type includes ethnicity (Hispanic as race)
    pop.template[, race_type := 'race_eth']

  # Handle maternal data prefixes ----
    # Remove "Birthing person's" prefix to standardize maternal data categories
    pop.template[grepl("birthing person", cat1, ignore.case = TRUE),
                 cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
    pop.template[grepl("birthing person", cat2, ignore.case = TRUE),
                 cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

  # Process geographic types and demographic groupings ----
    # Define OMB AIC (alone or in combination)
    omb_aic <- c("chi_race_aic_aian", "chi_race_aic_asian", "chi_race_aic_black",
                 "chi_race_aic_his", "chi_race_aic_nhpi", "chi_race_aic_wht")

    # Process both primary (cat1) and secondary (cat2) stratification variables
    for(catnum in c("cat1", "cat2")) {
      catvarname <- paste0(catnum, "_varname")
      temp.groupby <- paste0("group_by", gsub('cat', '', catnum))

      # Set geographic type based on category
      pop.template[get(catnum) == "Cities/neighborhoods", geo_type := "hra"]

      # Set race_type and group_by based on race/ethnicity variable
      pop.template[get(catvarname) == "race3", c("race_type", temp.groupby) := 'race']
      pop.template[get(catvarname) == "race4", c("race_type", temp.groupby) := 'race_eth']
      pop.template[get(catvarname) %in% omb_aic, c("race_type", temp.groupby) := 'race_aic']

      # Filter out non-standard AIC race/ethnicity categories that don't have population data
      pop.template <- pop.template[!(grepl('_aic_', get(catvarname)) &
                                       !get(catvarname) %in% omb_aic)]

      # Set demographic grouping based on category label
      pop.template[get(catnum) == "Ethnicity", c("race_type", temp.groupby) := 'race_eth']
      pop.template[get(catnum) == "Gender", (temp.groupby) := 'genders']
      pop.template[get(catnum) %in% c("Race", "Race/ethnicity") & get(catvarname) == 'race4',
                   (temp.groupby) := 'race_eth']
      pop.template[(get(catnum) == "Race" & get(catvarname) == 'race3'),
                   (temp.groupby) := 'race']

      # Set geographic type based on regions
      pop.template[get(catnum) == "Regions" & (is.na(geo_type) | geo_type != 'hra'),
                   geo_type := "region"]
      pop.template[get(catnum) == "Big cities", geo_type := "hra"]
      pop.template[get(catnum) == "Washington State", geo_type := "wa"]
    }

  # Handle special geographic cases ----
    # Set geographic type for poverty analysis to block group level by default
    pop.template[grepl("poverty$", cat1, ignore.case = TRUE) |
                   grepl("poverty$", cat2, ignore.case = TRUE),
                 geo_type := "blk"]

    # Override with zip code level for poverty analysis if specified in povgeo parameter
    if(!is.na(povgeo) && povgeo == 'zip') {
      pop.template[grepl("poverty$", cat1, ignore.case = TRUE) |
                     grepl("poverty$", cat2, ignore.case = TRUE),
                   geo_type := "zip"]
    }

    # Special case for combined regions and cities/neighborhoods analysis
    pop.template[(cat1 == "Regions" & cat2 == "Cities/neighborhoods") |
                   (cat2 == "Regions" & cat1 == "Cities/neighborhoods"),
                 geo_type := "blk"]

    # Set missing or default values ----
    # Replace NA cat2 with "NA" string for consistent processing
    pop.template[is.na(cat2), cat2 := "NA"]

    # Set default geographic type as King County when not specified
    pop.template[is.na(geo_type), geo_type := 'kc']

  # Return final results ----
    # Remove duplicate rows to minimize calls to get_population for efficiency
    return(unique(pop.template))
}
