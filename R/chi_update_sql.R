#' CHI SQL Update
#'
#' @description
#' Function to update (or replace) results and metadata in APDE PHExtractStore
#' servers.
#'
#' @details
#' For details on server access, and to configure your local settings,
#' please review documentation:
#' \href{https://kc1.sharepoint.com/teams/DPH-KCCross-SectorData/Shared\%20Documents/References/SQL/SQL Server Setup APDE.docx}{
#' SharePoint > DPH-KCCross-SectorData > Documents > References > SQL > SQL Server Setup APDE.docx}.
#'
#' @param CHIestimates data.frame/data.table containing CHI analytic results
#' @param CHImetadata data.frame/data.table containing CHI metadata
#' @param table_name name of SQL Server table to update
#' @param server type of server (\code{'development'} for KCITSQLUATHIP40 and
#' \code{'production'} for KCITSQLPRPHIP40)
#'
#' Default \code{server = 'development'}
#' @param replace_table If TRUE, drop existing table and insert data, if FALSE
#' update matching rows and insert new data
#'
#' Default \code{replace_table = FALSE}
#'
#' @examples
#' \dontrun{
#' # Update development database
#' chi_update_sql(
#'   CHIestimates = final_estimates,
#'   CHImetadata = final_metadata,
#'   table_name = "birth",
#'   server = "development",
#'   replace_table = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{chi_qa_tro}} for validating data before upload
#'
#' \code{\link{chi_compare_estimates}} for comparing with existing data
#'
#' @return Returns a status message indicating success and location, or failure, of upload
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table setDT copy
#' @importFrom DBI dbConnect dbWriteTable dbGetQuery dbExistsTable Id
#' @importFrom odbc odbc
#' @importFrom glue glue_sql
#' @importFrom yaml yaml.load
#' @importFrom rads tsql_validate_field_types
#'
#' @export
#'
chi_update_sql <- function(CHIestimates = NULL,
                           CHImetadata = NULL,
                           table_name = NULL,
                           server = 'development', # options include c('development', 'production')
                           replace_table = FALSE # default is to update select rows rather than replace the entire table
){
  # load CHI yaml config file ----
  # check CHIestimates argument----
  if(is.null(CHIestimates)){stop("\n\U0001f47f The results table to push to SQL (CHIestimates) is missing ")}
  if( inherits(CHIestimates, "data.frame") == FALSE){stop("\n\U0001f47f CHIestimates must be a data.frame or a data.table.")}
  if( inherits(CHIestimates, "data.table") == FALSE){setDT(CHIestimates)}
  rads::tsql_validate_field_types(ph.data = CHIestimates, field_types = unlist(chi_get_yaml()$vars))

  # check CHImetadata argument----
  if(is.null(CHImetadata)){stop("\n\U0001f47f The metadata table to push to SQL (CHImetadata) is missing ")}
  if( inherits(CHImetadata, "data.frame") == FALSE){stop("\n\U0001f47f CHImetadata must be a data.frame or a data.table.")}
  if( inherits(CHImetadata, "data.table") == FALSE){setDT(CHImetadata)}
  rads::tsql_validate_field_types(ph.data = CHImetadata, field_types = unlist(chi_get_yaml()$metadata))

  # checkt table_name ----
  if(is.null(table_name)){stop("\n\U0001f47f The table_name argument is missing ")}
  if(length(table_name) != 1 | !is.character(table_name)){stop("\n\U0001f47f table_name must be a character vector of length 1")}

  # ensure indicator_key is consistent across estimates and metadata
  if(!identical(sort(as.character(unique(CHIestimates$indicator_key))), sort(as.character(CHImetadata$indicator_key)))){
    stop("\n\U0001f47f The indicator_key values in CHIestimates and CHImetadata are not identical ... but they should be!")
  }

  # check server argument----
  server = tolower(as.character(server))
  if(!server %in% c('development', 'production')){stop("\n\U0001f47f The server argument is limited to: 'development', 'production'")}
  if(length(server) != 1){stop("\n\U0001f47f The `server` argument must be of length 1")}

  # check replace argument----
  if(!is.logical(replace_table)){stop("\n\U0001f47f The `replace` argument must be a logical, i.e., TRUE | FALSE")}
  if(length(replace_table) != 1){stop("\n\U0001f47f The `server` argument must be of length 1")}

  # open database connection----
  if(server %in% c('development')){
    CHI_db_cxn <- odbc::dbConnect(odbc::odbc(),
                                  Driver = "SQL Server",
                                  Server = "KCITSQLUATHIP40",
                                  Database = "PHExtractStore")
    schema_suffix = '_WIP'
    complete_servername = 'KCITSQLUATHIP40'
  }
  if(server %in% c('production')){
    CHI_db_cxn <- odbc::dbConnect(odbc::odbc(),
                                  Driver = "SQL Server",
                                  Server = "KCITSQLPRPHIP40",
                                  Database = "PHExtractStore")
    schema_suffix = ''
    complete_servername = 'KCITSQLPRPHIP40'
  }

  # check if *_results and *_metadata tables already exist in the appropriate schema----
    if (!DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))) {
      warning(paste0("\U00026A0\U00026A0\U00026A0\U00026A0\nThe table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results]` does NOT currently exist. A NEW table will be created."))
      message("Continuing...")
    }

    if (!DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]", .con = CHI_db_cxn))) {
      warning(paste0("\U00026A0\U00026A0\U00026A0\U00026A0\nThe table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_metadata]` does NOT currently exist. A NEW table will be created."))
      message("Continuing...")
    }

  # if replace_table = F, delete existing data that will be replaced in SQL----
  if(isFALSE(replace_table)){
    # delete results
    if(DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))){
      DBI::dbGetQuery(
        conn = CHI_db_cxn,
        glue::glue_sql("DELETE FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                         WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))
    }


    # delete metadata
    if(DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]", .con = CHI_db_cxn))){
      DBI::dbGetQuery(
        conn = CHI_db_cxn,
        glue::glue_sql("DELETE FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]
                           WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))
    }

  }

  # if replace_table = T, check if there are indicators that would be lost----
  if(isTRUE(replace_table) &
     DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))){

    existing_indicators <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                           statement = glue::glue_sql("SELECT DISTINCT indicator_key FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                                                                            WHERE indicator_key IS NOT NULL"))$indicator_key
    new_indicators <- unique(CHIestimates[]$indicator_key)
    missing_indicators <- setdiff(existing_indicators, new_indicators)

    if(length(missing_indicators) > 0){
      tempquestion <- paste0("\U00026A0 You submitted the argument `replace_table = TRUE`, but your new data is missing ", length(missing_indicators),
                             "\nindicator_keys that are in the existing SQL table. These indicator_keys will be permanently lost! \nAre you sure you want to continue? (y/n) ")
      answer <- readline(tempquestion)
      if (answer == "y") {
        message("Continuing...")
      } else {
        stop(paste0("\n\U0001f47f Replacing [PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results] would have resulted in indicator_keys being permanently lost and you decided not to continue."))
      }

    }
  }

  # push data to SQL----
  if(isTRUE(replace_table)){
    # results
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_results')),
                      value = as.data.frame(copy(CHIestimates)),
                      overwrite = T,
                      append = F,
                      field.types = unlist(chi_get_yaml()$vars))

    # metadata
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_metadata')),
                      value = as.data.frame(copy(CHImetadata)),
                      overwrite = T,
                      append = F,
                      field.types = unlist(chi_get_yaml()$metadata))
  }

  if(isFALSE(replace_table)){
    # results
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_results')),
                      value = as.data.frame(copy(CHIestimates)),
                      overwrite = F,
                      append = T)

    # metadata
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_metadata')),
                      value = as.data.frame(copy(CHImetadata)),
                      overwrite = F,
                      append = T)
  }

  # quick QA (row counts) ----
  # results
  sqlcount_results <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                      glue::glue_sql("SELECT newcount = count(*) FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                         WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))$newcount
  if(sqlcount_results != nrow(CHIestimates)){
    stop("\n\U0001f47f For the indicator_keys in CHIestimates, the number of rows in SQL does not match the number of rows in CHIestimates. In other words, your upload failed ... try again")
  }

  # metadata
  sqlcount_metadata <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                       glue::glue_sql("SELECT newcount = count(*) FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]
                         WHERE indicator_key IN ({paste(unique(CHImetadata$indicator_key))*})", .con = CHI_db_cxn))$newcount
  if(sqlcount_metadata != nrow(CHImetadata)){
    stop("\n\U0001f47f For the indicator_keys in CHImetadata, the number of rows in SQL does not match the number of rows in CHImetadata. In other words, your upload failed ... try again")
  }

  # return----
  return(message(paste0("\n\U0001f389Congratulations!\U0001f973\nYour data were successfully pushed to the SQL server!\n",
                 "The result are here: [", complete_servername, "].[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results]\n",
                 "The metadata are here: [", complete_servername, "].[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_metadata]\n")))


}
