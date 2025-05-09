#' Suppress data due to small numbers and create a caution flag due to reliability
#'
#' @description
#' This function applies primary data suppression based on the numerator and
#' denominator and secondary suppression based on the numerator. It also flags
#' low reliability. Suppressed data are noted by \code{suppression = '^'} and
#' unreliable data are noted by \code{caution = '!'}.
#'
#' @details
#' Data source specific suppression ranges can be found in
#' \href{https://kc1.sharepoint.com/teams/DPH-APDEData/Shared\%20Documents/General/data_presentation_algorithm/APDE_SmallNumberUpdate.xlsx}{APDE_SmallNumberUpdate.xlsx}
#'
#'
#' Please review the
#' \href{https://kc1.sharepoint.com/teams/DPH-APDEData/Shared\%20Documents/General/data_presentation_algorithm/APDEDataPresentationAlgorithm_2020_Approved.pptx}{APDE}
#' and \href{https://www.doh.wa.gov/Portals/1/Documents/1500/SmallNumbers.pdf}{DOH}
#' suppression guidelines for details on the logic used in this function.
#'
#' This function expects data that has already been formatted for CHI. However,
#' the user can modify the default parameter settings for use with most tables of
#' summary data.
#'
#' @param ph.data A data.table or data.frame. Must contain the data to be suppressed
#' with standard metric names,
#' i.e., mean, median, sum, rate, lower, upper, se, rse, numerator, denominator,
#' proportion
#' @param suppress_range Integer vector of length 2. They specify the minimum and
#' maximum range for suppression.
#' @param secondary Logical (\code{T}, \code{TRUE}, \code{F}, or \code{FALSE})
#' indicating whether secondary suppression should be run
#' @param secondary_ids Character vector of column names which are used to define
#' groups for secondary suppression.
#' Note, this should not include the most granular level. For example, if you wanted
#' secondary suppression for race/ethnicity within HRAs
#' where \code{cat1_varname == 'race4'} and \code{cat1_group == 'AIAN'},
#' \code{cat1_group == 'Asian'}, \code{cat1_group == 'Black'}, etc., you should
#' have \code{secondary_ids = c('hra20_name', 'cat1_varname')} rather than
#' \code{secondary_ids = c('hra20_name', 'cat1_varname', 'cat1_group')}
#' @param secondary_exclude An unquoted expression that evaluates to a logical vector
#' indicating which rows to include in secondary suppression analysis. Use this parameter
#' to exclude categories that are not mutually exclusive (e.g., overlapping demographic
#' groups). The expression should use column names from the dataset and evaluate to TRUE
#' for rows to include. For example: `secondary_exclude = cat1_varname != "race3"` would
#' exclude all rows where cat1_varname equals "race3" from secondary suppression.
#' @param flag_only Logical (\code{T}, \code{TRUE}, \code{F}, or \code{FALSE})
#' indicating whether data to be
#' suppressed should be flagged without setting estimates to NA
#' @param numerator_col Character string with the name of the numerator column. Default is "numerator".
#' @param denominator_col Character string with the name of the denominator column. Default is "denominator".
#' @param rse_col Character string with the name of the relative standard error column. Default is "rse".
#' @param columns_to_suppress Character vector of column names to be suppressed (set to NA) when
#' suppression is flagged. Default includes common result columns.
#'
#' @return A data.table with suppression applied to CHI standard columns.
#'
#' @export
#'
#' @keywords suppression
#'
#' @importFrom data.table ':=' data.table is.data.table setDT fsetdiff setorder setorderv copy
#'
#' @examples
#' \dontrun{
#' set.seed(98104)
#' dt <- data.table::data.table(
#'   chi_year = rep(2018, 100),
#'   result = rnorm(100, .25, .05),
#'   numerator = round(rnorm(100, 20, 9), 0),
#'   denominator = round(rnorm(100, 100, 30), 0),
#'   rse = sample(1:100, 100, replace = TRUE)
#' )
#'
#' # Basic suppression with default parameters
#' newdt <- chi_suppress_results(ph.data = dt,
#'                               suppress_range = c(1, 9),
#'                               secondary = FALSE)
#'
#' nrow(dt[numerator %in% 1:9 | denominator %in% 1:9]) # rows needed suppression
#' nrow(newdt[suppression=='^']) # rows suppressed
#' nrow(newdt[rse >= 30 | numerator == 0]) # rows needing caution
#' nrow(newdt[caution=='!']) # rows with caution
#'
#' # With secondary suppression
#' dt$region <- sample(c("North", "South", "East", "Seattle"), 100, replace = TRUE)
#' dt$category <- sample(c("A", "B", "C"), 100, replace = TRUE)
#'
#' newdt2 <- chi_suppress_results(ph.data = dt,
#'                                suppress_range = c(1, 9),
#'                                secondary = TRUE,
#'                                secondary_ids = c("region", "category"))
#'
#' nrow(newdt[suppression=='^']) # only primary suppression
#' nrow(newdt2[suppression=='^']) # with secondary suppression
#'
#' # Using custom column names
#' dt2 <- data.table::data.table(
#'   chi_year = rep(2018, 100),
#'   mean = rnorm(100, .25, .05),
#'   num = round(rnorm(100, 20, 9), 0),
#'   denom = round(rnorm(100, 100, 30), 0),
#'   rel_se = sample(1:100, 100, replace = TRUE)
#' )
#'
#' newdt3 <- chi_suppress_results(ph.data = dt2,
#'                                suppress_range = c(1, 9),
#'                                numerator_col = "num",
#'                                denominator_col = "denom",
#'                                rse_col = "rel_se",
#'                                columns_to_suppress = c("mean", "num", "denom"))
#'
#' nrow(dt2[num %in% 1:9 | denom %in% 1:9]) # rows need suppression
#' nrow(newdt3[suppression == '^']) # rows suppressed
#' }
#'

chi_suppress_results  <- function(ph.data = NULL,
                                  suppress_range = c(1, 9),
                                  secondary = FALSE,
                                  secondary_ids = c("tab", "indicator_key", "cat1", "cat2_group", "year"),
                                  secondary_exclude,
                                  flag_only = FALSE,
                                  numerator_col = "numerator",
                                  denominator_col = "denominator",
                                  rse_col = "rse",
                                  columns_to_suppress = c("result", "lower_bound", "upper_bound", "se", "rse",
                                                          "numerator", "denominator")){

  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
  numerator <- denominator <- suppression <- my.group <- my.order <- my.rowct <-
    suppressed.group <- my.flag <- rse <- caution <- rows.unsuppressed <-
    result <- lower_bound <- upper_bound <- se <- NULL

  # ---- Validate 'ph.data' ----
  if(is.null(ph.data)){
    stop("\n\U1F6D1 You must specify a dataset (i.e., 'ph.data' must be defined)")
  }

  if(!data.table::is.data.table(ph.data)){
    if(is.data.frame(ph.data)){
      data.table::setDT(ph.data)
    } else {
      stop(paste0("\n\U1F6D1 <{ph.data}> must be the name of a data.frame or data.table."))
    }
  }

  # ---- Validate 'suppress_range' ----
  if(is.null(suppress_range)){
    suppress_range <- c(0, 9)
  }

  if(!is.null(suppress_range) &
     (length(suppress_range) != 2 | suppress_range[1] %% 1 != 0 | suppress_range[2] %% 1 != 0 |
      suppress_range[1] < 0 | suppress_range[2] < 0)){
    stop("\n\U1F6D1 <suppress_range> must be comprised of two non-negative integers (i.e., 'c(0, 9)'")
  }

  # ---- Validate 'secondary' ----
  if(!is.logical(secondary)){
    stop("\n\U1F6D1 'secondary' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }

  # ---- Validate 'secondary_ids' ----
  if(secondary == TRUE & length(setdiff(secondary_ids, names(ph.data))) > 0){
    stop("\n\U1F6D1 At least one name in 'secondary_ids' is not found among the column names in 'ph.data'")
  }

  # ---- Validate 'secondary_exclude' ----
  if(!missing(secondary_exclude)){
    call = match.call()

    if(is.character(call[['secondary_exclude']])){
      where = str2lang(call[['secondary_exclude']])
      warning('\u26A0\ufe0f `secondary_exclude` is a string. It was converted so that it would work, but in the future, this might turn into an error.
              In the future, please pass unquoted commands that will resolve to a logical')
    } else {
      where = copy(call[['secondary_exclude']])
    }

    e <- substitute(expr = where) # get parse tree expression `where`
    myfilter <- eval(expr = e, envir = ph.data, enclos = parent.frame()) # evaluate
    stopifnot('`where` does not resolve to a logical' = is.logical(myfilter))
    if(nrow(ph.data[myfilter,]) < 1){
      stop(paste0("\n\U1F6D1 Your 'secondary_exclude' argument filters out all rows of data. Please revise and submit again"))
    }
  }

  # ---- Validate 'flag_only' ----
  if(!is.logical(flag_only)){
    stop("\n\U1F6D1 'flag_only' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }

  # ---- Validate 'numerator_col' ----
  if(!numerator_col %in% names(ph.data)){
    stop(paste0("\n\U1F6D1 Required column '", numerator_col, "' is missing from the dataset"))
  }

  # ---- Validate 'denominator_col' ----
  if(!denominator_col %in% names(ph.data)){
    warning(paste0("\u26A0\ufe0f Column '", denominator_col, "' is missing from the dataset. Only numerator-based suppression will be applied."))
  }

  # ---- Validate 'rse_col' ----
  if(!rse_col %in% names(ph.data)){
    warning(paste0("\u26A0\ufe0f Column '", rse_col, "', the value of `rse_col`, is missing from the dataset. `caution` flag for reliability will not be generated."))
  }

  # ---- Validate 'columns_to_suppress' ----
  missing_cols <- setdiff(columns_to_suppress, names(ph.data))
  if(length(missing_cols) > 0){
    warning(paste0("\u26A0\ufe0f The following columns specified in 'columns_to_suppress' are missing from the dataset and will be ignored: ",
                   paste(missing_cols, collapse = ", ")))
    columns_to_suppress <- intersect(columns_to_suppress, names(ph.data))
  }


  # ---- Copy ph.data to avoid changing the underlying data.table due to modification by references----
  temp.dt <- data.table::setDT(copy(ph.data))

  # ---- Check for existing suppression and caution columns ----
  if("suppression" %in% names(temp.dt)){
    warning("\u26A0\ufe0f Existing 'suppression' column will be overwritten")
    temp.dt[, suppression := NULL]  # Remove existing column
  }

  if("caution" %in% names(temp.dt)){
    warning("\u26A0\ufe0f Existing 'caution' column will be overwritten")
    temp.dt[, caution := NULL]  # Remove existing column
  }

  # ---- Identify primary suppression ----
  # Check both numerator and denominator for suppression
  temp.dt[, suppression := NA_character_]

  temp.dt[get(numerator_col) >= suppress_range[1] & get(numerator_col) <= suppress_range[2], suppression := "^"]

  if(denominator_col %in% names(temp.dt)){ # above used warning, not stop, if denominator_col didn't exist
    temp.dt[get(denominator_col) >= suppress_range[1] & get(denominator_col) <= suppress_range[2], suppression := "^"]
  }

  # ---- Identify secondary suppression (only based on numerator) ----
  if(isTRUE(secondary)){

    # Apply secondary_exclude argument
    if(!missing(secondary_exclude)){
      myfilter <- eval(expr = e, envir = temp.dt, enclos = parent.frame())
      temp.dt.aside <- data.table::fsetdiff(temp.dt, temp.dt[myfilter,])
      temp.dt <- temp.dt[myfilter,]
    }

    # Create group id for each set of secondary_ids
    temp.dt[, my.group := .GRP, by = secondary_ids]
    data.table::setorder(temp.dt, my.group)

    # Identify max number of rows per group defined by secondary_ids
    temp.dt[, my.rowct := .N, by = secondary_ids]

    # Identify groups that had initial suppression
    temp.dt[, suppressed.group := FALSE]
    temp.dt[my.group %in% unique(temp.dt[suppression == "^"]$my.group), suppressed.group := TRUE]

    # Within groups that had suppression, count the number of rows that were not suppressed
    temp.dt[my.group %in% unique(temp.dt[suppressed.group == TRUE]$my.group) & is.na(suppression),
            rows.unsuppressed := .N, by = secondary_ids]
    suppressWarnings(temp.dt[, rows.unsuppressed := max(rows.unsuppressed, na.rm = TRUE), by = my.group])

    # Identify when the number of un-suppressed rows (in groups that had suppression) is max rows minus 1
    # (these need secondary suppression)
    temp.dt[is.na(suppression) & rows.unsuppressed == my.rowct - 1,
            my.flag := "group needs secondary suppression"]

    # Sort table so the smallest numerator per group that needs secondary suppression is first
    data.table::setorderv(temp.dt, c('my.group', numerator_col), na.last = TRUE)

    # Suppress row with smallest numerator among groups needing secondary suppression
    if(nrow(temp.dt[my.flag == "group needs secondary suppression"]) > 0){
      temp.dt[my.flag == "group needs secondary suppression", my.order := 1:.N, by = my.group]
      temp.dt[my.order == 1, suppression := "^"]
    }

    # Drop all temporary variables
    temp.dt[, (intersect(c("my.group", "suppressed.group", "my.rowct", "my.flag", "my.order",
                           "rows.unsuppressed"), names(temp.dt))) := NULL]

    # Combine back with data filtered out by secondary_exclude
    if(exists("temp.dt.aside")){
      temp.dt <- rbind(temp.dt, temp.dt.aside)
      rm(temp.dt.aside)
    }
  }

  # ---- Apply suppression to columns_to_suppress unless flag_only = FALSE ----
  # Use validated columns_to_suppress
  if(isFALSE(flag_only)){
    temp.dt[suppression == "^", (columns_to_suppress) := NA]
  }

  # ---- Apply caution flag if possible ----
  if(rse_col %in% names(temp.dt)){
    temp.dt[get(rse_col) >= 30 | get(numerator_col) == 0, caution := "!"]
  }

  return(temp.dt)

}
