#' Generate Analysis Sets for CHI
#'
#' @description
#' Creates sets of indicator keys that share common analysis patterns within CHI
#' (Community Health Indicators) data. This function reads the most recent production
#' version of CHI estimates for a specified data source and groups indicators that
#' use the same combinations of \code{cat1}, \code{cat1_varname}, and \code{trends}
#' columns.
#'
#' @param data_source Character string (length 1) specifying the data source to analyze
#'   (e.g., \code{'birth'}, \code{'brfss'}, \code{'chars'}, \code{'death'}, \code{'hys'}). This corresponds to a table
#'   name in the PHExtractStore database.
#'
#' @return A data.table containing analysis sets with the following columns:
#'   \itemize{
#'     \item \code{set}: Integer identifying groups of indicators with identical analysis patterns
#'     \item \code{cat1}: Category name as expected in CHI TRO
#'     \item \code{cat1_varname}: Category variable name as expected in CHI TRO
#'     \item \code{_kingcounty}: \code{'x'} if analysis includes King County tab
#'     \item \code{_wastate}: \code{'x'} if analysis includes Washington State tab
#'     \item \code{demgroups}: \code{'x'} if analysis includes demographic groups
#'     \item \code{crosstabs}: \code{'x'} if analysis includes crosstabulations
#'     \item \code{trends}: \code{'x'} if analysis includes trends analysis
#'     \item \code{set_indicator_keys}: Comma-separated list of indicator keys sharing the pattern
#'   }
#'
#' @details
#' This function generates a table for \code{\link{chi_generate_tro_shell}}, providing the
#' structure required for generating analysis instructions. It connects to the
#' \code{[PHExtractStore]} database on \code{KCITSQLPRPHIP40} to retrieve the latest production data.
#' Users need appropriate database credentials - contact your manager if you need access.
#'
#' Typically, the analysis will not change from year to year. However, you should
#' compare the output of this function with the 'CHI-Variables_ByDataSource' worksheet
#' in
#' \href{https://kc1.sharepoint.com/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady\%20Output.xlsx}{
#' SharePoint > Community Health Indicators > CHI_vizes > CHI-Standards-TableauReady Output.xlsx}
#' and update it as necessary.
#'
#' The output structure directly informs \code{\link{chi_generate_tro_shell}} about which
#' indicators should be analyzed together based on their shared patterns of
#' categories and analysis types.
#'
#' @importFrom data.table setDT setorderv copy := .SD fifelse fsetdiff .GRP
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbGetQuery
#' @importFrom odbc odbc
#'
#' @export
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}} for generating TRO shell from these analysis sets
#'
#' @examples
#' \dontrun{
#' # Generate analysis sets for birth data
#' birth_sets <- chi_generate_analysis_set("birth")
#'
#' # Generate analysis sets for BRFSS data
#' brfss_sets <- chi_generate_analysis_set("brfss")
#' }
#'
chi_generate_analysis_set <- function(data_source = NULL) {
  # Input validation
    if (is.null(data_source)) {
      stop("\n\U1F6D1 data_source parameter must be provided")
    }
    if (!is.character(data_source) || length(data_source) != 1) {
      stop("\n\U1F6D1 data_source must be a single character string, e.g., 'birth'")
    }

  # Get data ----
    # Construct the full table name for error messages
      full_table_name <- paste0("[APDE].[", data_source, "_results]")
      server_info <- "KCITSQLPRPHIP40 > PHExtractStore"

    # try to make a database connection
      tryCatch({
        # Establish connection
        cnxn <- odbc::dbConnect(
          odbc::odbc(),
          Driver = "SQL Server",
          Server = "KCITSQLPRPHIP40",
          Database = "PHExtractStore"
        )
      }, error = function(e) {
        # Handle connection errors separately from table existence errors
        if (grepl("connection", tolower(e$message))) {
          stop(paste0("\n\U1F6D1 Failed to connect to ", server_info, ". Please check your network connection and credentials."))
        } else {
          # Re-throw the error with our custom message if it's already handled
          stop(e$message)
        }
      })


    # Check if table exists before attempting query
      table_exists <- DBI::dbExistsTable(
        conn = cnxn,
        name = paste0(data_source, "_results"),
        schema = "APDE"
      )

    # If table exists, load into memory
      if (!table_exists) {
        stop(paste0("\U1F6D1 You specified data_source = '", data_source,
                    "', which attempted to download ", full_table_name,
                    " from ", server_info, ". This table does not exist."))
      } else {
      tempdt <- data.table::setDT(DBI::dbGetQuery(
        conn = cnxn,
        statement = paste0("SELECT * FROM ", full_table_name)
      )) }

    # Close database connection
      if (exists("cnxn") && !is.null(cnxn)) {
        DBI::dbDisconnect(cnxn)
      }

  # Recodes for race3 & race4 ----
    # Necessary because they are wonky as heck due to how APDE decided to code/display them
      race3_remix1 <- tempdt[(grepl('race/ethnicity$', cat1, ignore.case = T) & cat1_varname == 'race3')]
      tempdt <- rbind(
        fsetdiff(tempdt, race3_remix1),
        copy(race3_remix1)[, cat1 := gsub('race/ethnicity$', 'race', cat1)][, cat1 := gsub('Race/ethnicity$', 'Race', cat1)],
        copy(race3_remix1)[, cat1 := gsub('race/ethnicity$', 'ethnicity', cat1)][, cat1 := gsub('Race/ethnicity$', 'Ethnicity', cat1)]
      )

      race3_remix2 <- tempdt[(grepl('race/ethnicity$', cat2, ignore.case = T) & cat2_varname == 'race3')]
      tempdt <- rbind(
        fsetdiff(tempdt, race3_remix2),
        copy(race3_remix2)[, cat2 := gsub('race/ethnicity$', 'race', cat2)][, cat2 := gsub('Race/ethnicity$', 'Race', cat2)],
        copy(race3_remix2)[, cat2 := gsub('race/ethnicity$', 'ethnicity', cat2)][, cat2 := gsub('Race/ethnicity$', 'Ethnicity', cat2)]
      )

      tempdt[cat1_varname == 'race4', cat1 := gsub('race$', 'race/ethnicity', cat1)]
      tempdt[cat1_varname == 'race4', cat1 := gsub('Race$', 'Race/ethnicity', cat1)]

      tempdt[cat2_varname == 'race4', cat2 := gsub('race$', 'race/ethnicity', cat2)]
      tempdt[cat2_varname == 'race4', cat2 := gsub('Race$', 'Race/ethnicity', cat2)]

  # Table of categories and tabs per indicator ----
    # For cat1 combinations
      tab_patterns <- tempdt[, list(
        `_kingcounty` = fifelse(any(tab == "_kingcounty"), "x", ""),
        `_wastate` = fifelse(any(tab == "_wastate"), "x", ""),
        demgroups = fifelse(any(tab == "demgroups"), "x", ""),
        crosstabs = fifelse(any(tab == "crosstabs"), "x", ""),
        trends = fifelse(any(tab == "trends"), "x", "")
      ), by = list(indicator_key, cat1, cat1_varname)]

    # For cat2 combinations
      tab_patterns2 <- tempdt[!is.na(cat2), list(
        `_kingcounty` = fifelse(any(tab == "_kingcounty"), "x", ""),
        `_wastate` = fifelse(any(tab == "_wastate"), "x", ""),
        demgroups = fifelse(any(tab == "demgroups"), "x", ""),
        crosstabs = fifelse(any(tab == "crosstabs"), "x", ""),
        trends = fifelse(any(tab == "trends"), "x", "")
      ), by = list(indicator_key, cat1 = cat2, cat1_varname = cat2_varname)]

    # Combine the patterns
      all_patterns <- merge(tab_patterns, tab_patterns2, by = c('indicator_key', 'cat1', 'cat1_varname'), all = T)
      all_patterns <- all_patterns[, list(indicator_key, cat1, cat1_varname,
                                       `_kingcounty` = fifelse(`_kingcounty.x` == 'x' | `_kingcounty.y` == 'x', 'x', NA_character_),
                                       `_wastate` = fifelse(`_wastate.x` == 'x' | `_wastate.y` == 'x', 'x', NA_character_),
                                       `demgroups` = fifelse(`demgroups.x` == 'x' | `demgroups.y` == 'x', 'x', NA_character_),
                                       `crosstabs` = fifelse(`crosstabs.x` == 'x' | `crosstabs.y` == 'x', 'x', NA_character_),
                                       `trends` = fifelse(`trends.x` == 'x' | `trends.y` == 'x', 'x', NA_character_) )]

    # Tidy
      all_patterns <- all_patterns[!(cat1 == 'Overall' & cat1_varname == 'overall')]

  # Generate analysis_sets ----
    # Function to convert a data.table's rows into a string
      rows_to_string <- function(mydt) {
        # Sort the data.table by all columns except 'indicator_key' to ensure consistent ordering
        dt_sorted <- setorderv(copy(mydt),
                               cols = setdiff(names(mydt), "indicator_key"))

        # Convert each row (excluding 'indicator_key') into a string, concatenating columns with "|||"
        row_strings <- apply(X = dt_sorted[, .SD, .SDcols = setdiff(names(mydt), "indicator_key")],
                             MARGIN = 1, # to iterate over rows
                             FUN = paste, collapse = "|||")

        # Combine the vector `row_strings` into a single string
        serialized_string <- paste(row_strings, collapse = "___")

        return(serialized_string)
      }

    # Generate the unique pattern for each indicator_key's rows
      pattern_groups <- all_patterns[, list(pattern = rows_to_string(.SD)),
                                     by = indicator_key]


    # Group the patterns to create the 'set' and 'set_indicator_keys' columns
      pattern_groups <- pattern_groups[, `:=`(set = .GRP,
                                              set_indicator_keys = paste(sort(indicator_key), collapse = ", ")),
                                       by = pattern]
      pattern_groups[, pattern := NULL]

    # Merge on original data
      result <- merge(all_patterns, pattern_groups, by = "indicator_key")

    # Keep unique rows
      result <- unique(result[, indicator_key := NULL])

  # return object ----
    return(result)
}


