#' CHI Dev to Prod
#'
#' @description
#' Transfers CHI results and metadata from the development SQL Server
#' (`KCITSQLUATHIP40`, schema `APDE_WIP`) to the production SQL Server
#' (`KCITSQLPRPHIP40`, schema `APDE`). Because no linked server exists between
#' dev and prod, matching rows are pulled from dev into R and then written to
#' prod.
#'
#' @details
#' For each `indicator_key` that is **not** excluded (via `exclude_keys`), the
#' function:
#'
#' 1. Opens connections to both the dev and prod SQL Servers.
#' 2. Queries dev for all `indicator_key` values present in the results table,
#'    then applies `exclude_keys` filtering to determine which keys will be
#'    transferred.
#' 3. Optionally shows the user a confirmation table (row counts per
#'    `indicator_key` from dev) and waits for approval before writing to prod.
#' 4. Pulls matching rows from dev into R.
#' 5. Deletes those `indicator_key` rows from the corresponding prod tables.
#' 6. Appends the pulled rows to prod.
#' 7. Runs a post-copying row count check to confirm success.
#'
#' `indicator_key` values identified by `exclude_keys` are skipped entirely.
#' They are neither deleted from prod nor copied from dev. All other rows
#' already in prod for those excluded keys are left as-is.
#'
#' For details on server access and local configuration, please review:
#' SharePoint > DPH-KCCross-SectorData > Documents > References > SQL >
#' SQL Server Setup APDE.docx.
#'
#' @param table_name character. The root name of the SQL table to transfer,
#'   e.g., `'birth'`, `'brfss'`, `'death'`. The function operates on
#'   `[table_name]_results` and `[table_name]_metadata` in both dev and prod.
#'
#' @param exclude_keys character vector or `NULL`. One or more strings used to
#'   identify `indicator_key` values that should **NOT** be copied from dev to
#'   prod. Matching behavior is controlled by `exact_match`.
#'
#'   Default `exclude_keys = NULL`; meaning nothing is excluded, i.e., all dev
#'    `indicator_key` values are copied to prod.
#'
#' @param exact_match logical. Controls how `exclude_keys` are applied.
#'
#'   - `FALSE`: each string in `exclude_keys` is matched as a
#'     substring pattern using base R's [base::grepl()]. For example,
#'     `exclude_keys = 'uninsure'` will exclude any `indicator_key` containing
#'     the text `'uninsure'` (e.g., `'uninsured_adult'`, `'uninsured_child'`).
#'   - `TRUE`: each string in `exclude_keys` must match an `indicator_key`
#'     exactly. Use this when indicator keys do not share a common root.
#'
#'    Default `exact_match = FALSE`.
#'
#' @param confirm logical. If `TRUE`, the user is shown a summary table of
#'   `indicator_key` values and their dev row counts before any writing to prod.
#'   The user must type **`y`** to continue. Set to `FALSE` to
#'   skip the prompt.
#'
#'   Default `confirm = TRUE`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects: copying data
#'   from dev to prod.
#'
#' @examples
#' \dontrun{
#' # Transfer all birth indicators from dev to prod
#' chi_dev_to_prod(
#'   table_name = "birth"
#' )
#'
#' # Transfer acs indicators, excluding indicator_keys starting with 'uninsured'
#' chi_dev_to_prod(
#'   table_name   = "acs",
#'   exclude_keys = "^uninsure",
#'   exact_match  = FALSE
#' )
#'
#' # Exclude two specific indicator_key values by exact name, skip confirmation
#' chi_dev_to_prod(
#'   table_name   = "acs",
#'   exclude_keys = c("uninsured_adult", "uninsured_child"),
#'   exact_match  = TRUE,
#'   confirm      = FALSE
#' )
#' }
#'
#' @seealso
#' [chi_update_sql()] for pushing data from R to either the dev or prod server.
#'
#' [chi_qa_tro()] for validating data before upload.
#'
#' @keywords CHI, Tableau, Production, SQL
#'
#' @export
#'
chi_dev_to_prod <- function(table_name   = NULL,
                            exclude_keys = NULL,
                            exact_match  = FALSE,
                            confirm      = TRUE) {

  # Validate table_name ----
  if (is.null(table_name)) {
    stop("\n\U0001f47f `table_name` is required but is missing.")
  }
  if (!is.character(table_name) || length(table_name) != 1) {
    stop("\n\U0001f47f `table_name` must be a single character string, e.g., 'birth'.")
  }

  # Validate exclude_keys ----
  if (!is.null(exclude_keys)) {
    if (!is.character(exclude_keys)) {
      stop("\n\U0001f47f `exclude_keys` must be a character vector or NULL.")
    }
    if (any(is.na(exclude_keys)) || any(nchar(trimws(exclude_keys)) == 0)) {
      stop("\n\U0001f47f `exclude_keys` must not contain NA or empty strings.")
    }
  }

  # Validate exact_match ----
  if (!is.logical(exact_match) || length(exact_match) != 1) {
    stop("\n\U0001f47f `exact_match` must be a single logical value: TRUE or FALSE.")
  }

  # Validate confirm ----
  if (!is.logical(confirm) || length(confirm) != 1) {
    stop("\n\U0001f47f `confirm` must be a single logical value: TRUE or FALSE.")
  }

  # Open database connections ----
  # Server names and schema conventions mirror chi_update_sql()
  dev_con <- odbc::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "KCITSQLUATHIP40",
    Database = "PHExtractStore"
  )
  on.exit(try(odbc::dbDisconnect(dev_con), silent = TRUE), add = TRUE)

  prod_con <- odbc::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "KCITSQLPRPHIP40",
    Database = "PHExtractStore"
  )
  on.exit(try(odbc::dbDisconnect(prod_con), silent = TRUE), add = TRUE)

  dev_schema   <- "APDE_WIP"
  prod_schema  <- "APDE"
  results_tbl  <- paste0(table_name, "_results")
  metadata_tbl <- paste0(table_name, "_metadata")

  # Confirm dev tables exist ----
  if (!DBI::dbExistsTable(dev_con, DBI::Id(schema = dev_schema, table = results_tbl))) {
    stop(glue::glue(
      "\n\U0001f47f The dev results table ",
      "[PHExtractStore].[{dev_schema}].[{results_tbl}] doesn't exist. ",
      "Check `table_name` and confirm data have been pushed to dev."
    ))
  }

  if (!DBI::dbExistsTable(dev_con, DBI::Id(schema = dev_schema, table = metadata_tbl))) {
    stop(glue::glue(
      "\n\U0001f47f The dev metadata table ",
      "[PHExtractStore].[{dev_schema}].[{metadata_tbl}] doesn't exist. ",
      "Check `table_name` and confirm data have been pushed to dev."
    ))
  }

  # Get all indicator_keys from dev results ----
  all_dev_keys <- DBI::dbGetQuery(
    dev_con,
    glue::glue_sql(
      "SELECT DISTINCT indicator_key
       FROM [PHExtractStore].[{DBI::SQL(dev_schema)}].[{DBI::SQL(results_tbl)}]
       WHERE indicator_key IS NOT NULL",
      .con = dev_con
    )
  )$indicator_key

  if (length(all_dev_keys) == 0) {
    stop(glue::glue(
      "\n\U0001f47f No indicator_key values found in ",
      "[PHExtractStore].[{dev_schema}].[{results_tbl}]. Nothing to transfer."
    ))
  }

  # Determine excluded keys ----
  if (!is.null(exclude_keys)) {
    if (isFALSE(exact_match)) {
      pattern <- paste(exclude_keys, collapse = "|")
      excluded_keys <- all_dev_keys[grepl(pattern, all_dev_keys)]
    } else {
      # Exact IN matching
      excluded_keys <- intersect(exclude_keys, all_dev_keys)
    }

    if (length(excluded_keys) == 0) {
      stop(
        "\n\U1F6D1\U0001F92C\U1F6D1\n`exclude_keys` were provided but no matching ",
        "indicator_key values were found in dev.\n",
        "As a precaution, the function has stopped. Either correct your `exclude_keys` argument\n",
        "or remove it entirely and try again."
      )
    } else {
      message(glue::glue(
        "\U0001F4E3 The following {length(excluded_keys)} indicator_key value(s) ",
        "will be excluded from the transfer (left untouched in prod):\n",
        paste0("\u00A0\u00A0\u00A0\u00A0- ", sort(excluded_keys), collapse = "\n")
      ))
    }
  } else {
    excluded_keys <- character(0)
  }

  # Determine keys to transfer ----
  transfer_keys <- setdiff(all_dev_keys, excluded_keys)

  if (length(transfer_keys) == 0) {
    stop(
      "\n\U0001f47f After applying `exclude_keys`, no indicator_key values ",
      "remain to transfer. Nothing will be written to prod."
    )
  }

  # Flag indicator_keys that are new to prod (purely FYI) ----
  prod_results_exists <- DBI::dbExistsTable(
    prod_con, DBI::Id(schema = prod_schema, table = results_tbl)
  )

  if (prod_results_exists) {
    existing_prod_keys <- DBI::dbGetQuery(
      prod_con,
      glue::glue_sql(
        "SELECT DISTINCT indicator_key
         FROM [PHExtractStore].[{DBI::SQL(prod_schema)}].[{DBI::SQL(results_tbl)}]
         WHERE indicator_key IS NOT NULL",
        .con = prod_con
      )
    )$indicator_key

    new_to_prod <- setdiff(transfer_keys, existing_prod_keys)
    if (length(new_to_prod) > 0) {
      message(glue::glue(
        "\U0001f440 Heads up!!! {length(new_to_prod)} indicator_key value(s) are new to prod ",
        "(no prior rows to delete -- will be inserted fresh):\n",
        paste0("  - ", sort(new_to_prod), collapse = "\n")
      ))
    }
  }

  # Confirmation prompt ----
  if (isTRUE(confirm)) {
    count_tbl <- data.table::setDT(
      DBI::dbGetQuery(
        dev_con,
        glue::glue_sql(
          "SELECT indicator_key, COUNT(*) AS n_rows
           FROM [PHExtractStore].[{DBI::SQL(dev_schema)}].[{DBI::SQL(results_tbl)}]
           WHERE indicator_key IN ({transfer_keys*})
           GROUP BY indicator_key
           ORDER BY indicator_key",
          .con = dev_con
        )
      )
    )

    message(glue::glue(
      "\n\n\U0001F4E3 The following indicator_key values will be transferred from dev to prod.",
      "\n    Dev  : [KCITSQLUATHIP40].[PHExtractStore].[{dev_schema}].[{results_tbl}]",
      "\n    Prod : [KCITSQLPRPHIP40].[PHExtractStore].[{prod_schema}].[{results_tbl}]",
      "\n\n"
    ))
    print(count_tbl, row.names = FALSE)
    message("\nExisting prod rows for these indicator_key values will be replaced.\n\n")

    answer <- readline("Continue copying from dev to prod? (y/n): ")
    if (tolower(trimws(answer)) != "y") {
      stop("\n\U1F6D1 Copying cancelled by user. No changes were made to prod.")
    }
    message("Continuing...")
  }

  # Fetch filtered data from dev into R ----
  results_dev <- data.table::setDT(
    DBI::dbGetQuery(
      dev_con,
      glue::glue_sql(
        "SELECT *
         FROM [PHExtractStore].[{DBI::SQL(dev_schema)}].[{DBI::SQL(results_tbl)}]
         WHERE indicator_key IN ({transfer_keys*})",
        .con = dev_con
      )
    )
  )

  metadata_dev <- data.table::setDT(
    DBI::dbGetQuery(
      dev_con,
      glue::glue_sql(
        "SELECT *
         FROM [PHExtractStore].[{DBI::SQL(dev_schema)}].[{DBI::SQL(metadata_tbl)}]
         WHERE indicator_key IN ({transfer_keys*})",
        .con = dev_con
      )
    )
  )

  # Warn if any transfer_keys are absent from dev metadata ----
  missing_from_meta <- setdiff(transfer_keys, unique(metadata_dev$indicator_key))
  if (length(missing_from_meta) > 0) {
    stop(glue::glue(
      "\n\U1F6D1\U0001f47f\U1F6D1 {length(missing_from_meta)} indicator_key value(s) ",
      "exist in the dev results table but are missing from the dev metadata table. ",
      "Please fix add the following to the metadata table and try again.:\n",
      paste0("  - ", sort(missing_from_meta), collapse = "\n")
    ))
  }

  # Delete existing prod rows for transfer_keys ----
  if (prod_results_exists) {
    DBI::dbExecute(
      prod_con,
      glue::glue_sql(
        "DELETE FROM [PHExtractStore].[{DBI::SQL(prod_schema)}].[{DBI::SQL(results_tbl)}]
         WHERE indicator_key IN ({transfer_keys*})",
        .con = prod_con
      )
    )
  } else {
    warning(glue::glue(
      "\U00026A0\U0000fe0f  The prod results table [PHExtractStore].[{prod_schema}].[{results_tbl}] ",
      "does not yet exist. A new table will be created."
    ), immediate. = TRUE)
  }

  prod_metadata_exists <- DBI::dbExistsTable(
    prod_con, DBI::Id(schema = prod_schema, table = metadata_tbl)
  )
  if (prod_metadata_exists) {
    DBI::dbExecute(
      prod_con,
      glue::glue_sql(
        "DELETE FROM [PHExtractStore].[{DBI::SQL(prod_schema)}].[{DBI::SQL(metadata_tbl)}]
         WHERE indicator_key IN ({transfer_keys*})",
        .con = prod_con
      )
    )
  } else {
    warning(glue::glue(
      "\U00026A0\U0000fe0f  The prod metadata table [PHExtractStore].[{prod_schema}].[{metadata_tbl}] ",
      "does not yet exist. A new table will be created."
    ), immediate. = TRUE)
  }

  # Append dev data to prod ----
  DBI::dbWriteTable(
    conn      = prod_con,
    name      = DBI::Id(schema = prod_schema, table = results_tbl),
    value     = as.data.frame(results_dev),
    overwrite = FALSE,
    append    = TRUE
  )

  DBI::dbWriteTable(
    conn      = prod_con,
    name      = DBI::Id(schema = prod_schema, table = metadata_tbl),
    value     = as.data.frame(metadata_dev),
    overwrite = FALSE,
    append    = TRUE
  )

  # Post-transfer row count QA ----
  prod_count_results <- DBI::dbGetQuery(
    prod_con,
    glue::glue_sql(
      "SELECT COUNT(*) AS n
       FROM [PHExtractStore].[{DBI::SQL(prod_schema)}].[{DBI::SQL(results_tbl)}]
       WHERE indicator_key IN ({transfer_keys*})",
      .con = prod_con
    )
  )$n

  if (prod_count_results != nrow(results_dev)) {
    stop(glue::glue(
      "\n\U1F6D1\U0001f47f\U1F6D1 Row count mismatch in results after transfer! ",
      "Expected {nrow(results_dev)} rows in prod but found {prod_count_results}. ",
      "Please investigate before using prod data."
    ))
  }

  prod_count_metadata <- DBI::dbGetQuery(
    prod_con,
    glue::glue_sql(
      "SELECT COUNT(*) AS n
       FROM [PHExtractStore].[{DBI::SQL(prod_schema)}].[{DBI::SQL(metadata_tbl)}]
       WHERE indicator_key IN ({transfer_keys*})",
      .con = prod_con
    )
  )$n

  if (prod_count_metadata != nrow(metadata_dev)) {
    stop(glue::glue(
      "\n\U1F6D1\U0001f47f\U1F6D1 Row count mismatch in metadata after transfer! ",
      "Expected {nrow(metadata_dev)} rows in prod but found {prod_count_metadata}. ",
      "Please investigate before using prod data."
    ))
  }

  # Success message ----
  message(glue::glue(
    "\n\U0001f389 Congratulations! \U0001f973",
    "\n{length(transfer_keys)} indicator_key value(s) successfully transferred from dev to prod.",
    "\n  Results  ({prod_count_results} rows) : [KCITSQLPRPHIP40].[PHExtractStore].[{prod_schema}].[{results_tbl}]",
    "\n  Metadata ({prod_count_metadata} rows): [KCITSQLPRPHIP40].[PHExtractStore].[{prod_schema}].[{metadata_tbl}]"
  ))

  invisible(NULL)
}
