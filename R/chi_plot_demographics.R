#' Generate CHI-style demographic bar plots
#'
#' @description Pulls estimates from a CHI `*_results` table in
#'   `PHExtractStore.APDE` and produces one bar chart (PNG or JPG) per
#'   `indicator_key`, suitable for use CHNA reports. Plots follow the Tableau
#'   Style Guide colors and are based on `apde.graphs`'s [bar plot
#'   chi](https://github.com/PHSKC-APDE/apde.graphs/wiki/bar_plot_chi).
#'
#' @details Data are pulled from `[PHExtractStore].[APDE].[table_name]` where
#'   `tab IN ('demgroups', '_kingcounty')` (and `chi = 1` when `chi = TRUE`),
#'   using a SQL database connection created by
#'   [apde.etl::create_db_connection()].
#'
#'   Only the most recent `year` is kept, chosen once per `indicator_key` and
#'   applied to every row of that indicator, so all bars on a graph are from the
#'   same year regardless of `cat1` or `cat1_varname`. `year` can be a single
#'   year (e.g. `'2025'`) or a multi-year range (e.g. `'2022-2026'`), so the
#'   comparison uses the rightmost 4 characters (the ending year) as an integer.
#'   When there are ties in the end year (e.g., `'2026'` and `'2022-2026'` tie
#'   at 2026),
#' **the widest range wins**, i.e. `'2022-2026'` is plotted and `'2026'` is
#'   discarded. The rationale is that multi-year estimate is more stable. The
#'   same rule resolves a tie between two ranges. For example, given
#'   `'2022-2024'` and `'2022-2026'`, the 5-year `'2022-2026'` is kept.
#'
#'   Some indicators store the `cat1_group = 'King County'` on both the
#'   `'_kingcounty'` and `'demgroups'` tabs, which would draw that bar twice.
#'   The function deduplicates these rows. If a `cat1_group` is *still*
#'   duplicated for an indicator, the rows differ in something real (e.g. two
#'   different `result` values) and the function stops with an error naming the
#'   indicator and the problematic groups.
#'
#'   `cat1_group` bars are grouped by `cat1_varname`, and the **groups appear in
#'   the order the `cat1_varname` argument lists them**. So `cat1_varname =
#'   c('chi_geo_kc', 'race4', 'chi_geo_region')` puts King County at the top,
#'   then the race groups, then the regions. The one exception is King County,
#'   which is always the top bar whenever it is requested. Both `'chi_geo_kc'`
#'   and the older variant `'kingco'` are recognized as naming the county. When
#'   `cat1_varname = NULL` there is no user-supplied order to follow, so the
#'   groups fall back to alphabetical by `cat1` (still with King County on top).
#'
#'   Within a `cat1_varname`, values are generally ordered alphabetically. The
#'   exceptions are the sorting of numeric bands (e.g., age bins) that are
#'   sorted numerically and catch-all buckets (`'Other'`, `'Multiple'`) that
#'   are always pushed to the end of their block. For example, when graphing
#'   race, `'Multiple'` would come *after* `'White'`.
#'
#'   Output files are named
#'   `<table_name>_<indicator_key>_<Sys.Date()>.<image_type>`.
#'
#' @param table_name Character. Name of a single SQL table in
#'   `PHExtractStore.APDE` ending in `_results` (e.g. use `'acs'` for
#'   `'acs_results'`). Required, length 1.
#'
#' @param indicator_key Character vector of one or more `indicator_key` values
#'   to plot.
#'
#'   Default `indicator_key = NULL`, which plots every `indicator_key` available
#'   in `table_name`.
#'
#' @param cat1_varname Character vector of `cat1_varname` values to include.
#'   Passing `NULL` plots every `cat1_varname` available in `table_name` and
#'   issues a warning, since this is typically not what is used for CHNA.
#'
#'   Default `cat1_varname = c('chi_geo_kc', 'chi_geo_region', 'race4')`. If
#'   `'race4'` is requested but only `'race3'` is available in `table_name`,
#'   `'race3'` is used instead.
#'
#' @param chi Logical. If `TRUE`, the results table query is limited to
#'   official CHI indicators, i.e. those flagged `chi = 1` in SQL. If `FALSE`,
#'   every `indicator_key` in the table is plotted.
#'
#'   Note! Non-official indicators generally have no entry in
#'   `[PHExtractStore].[APDE].[indicators_titles]`, so there is no CHI title
#'   to draw on. Those graphs will use the bare `indicator_key` (e.g.
#'   `'houseGTE30_rent'`) as the title.
#'
#'   Default `chi = TRUE`.
#'
#' @param output_dir Character. Directory where PNG/JPG files are saved; created
#'   if it doesn't already exist.
#'
#'   Default `output_dir = "c:/temp/chi_graphics"`.
#'
#' @param image_type One of `"png"` or `"jpg"`.
#'
#'   Default `image_type = "png"`.
#'
#' @param width Numeric. Image width in inches.
#'
#'   Default `width = 11`.
#'
#' @param height Numeric. Image height in inches.
#'
#'   Default `height = 8.5`.
#'
#' @param dpi Numeric. Resolution in dots per inch.
#'
#'   Default `dpi = 600`.
#'
#' @param prod Logical. If `TRUE`, connect to the CHI production server; if
#'   `FALSE`, connect to the CHI development/WIP server.
#'
#'   Default `prod = TRUE`.
#'
#' @param show_title Logical. If `TRUE`, the plot is titled with the indicator's
#'   `title` from `[PHExtractStore].[APDE].[indicators_titles]` (falling back to
#'   the `indicator_key` if that table has no title for it). If `FALSE`, neither
#'   the title nor the `subtitle` is drawn and the space they would take is
#'   reclaimed by the plot.
#'
#'   Default `show_title = TRUE`.
#'
#' @param show_caption Logical. If `TRUE`, the footnotes explaining the `^`
#'   (suppressed) and `!` (interpret with caution) symbols are drawn beneath the
#'   plot. If `FALSE`, they are omitted and their space is reclaimed by the plot.
#'
#'   Default `show_caption = TRUE`.
#'
#' @param trim_margin Logical. If `TRUE`, the white margin between the plot and
#'   the edges of the image is reduced on all four sides, so the saved file is
#'   almost entirely filled by the plot. Nothing is cropped: the title, caption
#'   and axis labels keep their own space, they simply start at the edge of the
#'   image.
#'
#'   This only removes the margin *around* the plot. The spacing between the
#'   bars, and between the bars and the panel edges, is unchanged.
#'
#'   Default `trim_margin = TRUE`.
#'
#' @param title Character. A custom title for the plot, used instead of the
#'   indicator's title from `[PHExtractStore].[APDE].[indicators_titles]`. Useful
#'   for indicators that are not official CHI indicators (and so have no CHI
#'   title) or when you want to deviate from the CHI title. The same title is used
#'   for every `indicator_key` plotted in the call, so it should be used with a
#'   single `indicator_key`. Not drawn when `show_title = FALSE`.
#'
#'   Default `title = NULL`, which uses the indicator's CHI title (or the
#'   `indicator_key` if it has none).
#'
#' @param subtitle Character. A subtitle drawn beneath the title. The same
#'   subtitle is used for every `indicator_key` plotted in the call. Not drawn
#'   when `show_title = FALSE`.
#'
#'   Default `subtitle = NULL`, which writes no subtitle and reserves no space for
#'   one.
#'
#' @examples
#' \dontrun{
#' # every indicator_key in acs_results, default county / race / region grouping
#' chi_plot_demographics(table_name = "acs_results")
#'
#' # a single indicator, saved as jpg
#' chi_plot_demographics(
#'   table_name = "acs_results",
#'   indicator_key = "houseGTE30_rent",
#'   image_type = "jpg"
#' )
#'
#' # two indicators, county & region only (no race), written to a custom folder.
#' # No title, because the report numbers and captions its own figures
#' chi_plot_demographics(
#'   table_name = "birth_results",
#'   indicator_key = c("preterm", "lbw"),
#'   cat1_varname = c("chi_geo_kc", "chi_geo_region"),
#'   output_dir = "c:/temp/chna_2026/birth",
#'   show_title = FALSE,
#'   trim_margin = TRUE
#' )
#'
#' # custom title and subtitle in place of the CHI title
#' chi_plot_demographics(
#'   table_name = "brfss",
#'   indicator_key = "chi_no_pcp",
#'   title = "Adults without a primary care provider",
#'   subtitle = "King County, 2023"
#' )
#'
#' # smaller, lower resolution image (e.g., for a quick slide or a draft review),
#' # pulled from the CHI development/WIP server rather than production. No
#' # caption, because the '^' and '!' footnotes appear elsewhere on the slide
#' chi_plot_demographics(
#'   table_name = "death_results",
#'   indicator_key = "overdose",
#'   width = 8,
#'   height = 6,
#'   dpi = 150,
#'   prod = FALSE,
#'   show_caption = FALSE
#' )
#' }
#'
#' @return Invisibly returns a character vector of the file paths written, one per
#' `indicator_key` plotted.
#'
#' @export
chi_plot_demographics <- function(table_name,
                                  indicator_key = NULL,
                                  cat1_varname = c('chi_geo_kc', 'race4', 'chi_geo_region'),
                                  chi = TRUE,
                                  output_dir = 'c:/temp/chi_graphics',
                                  image_type = c('png', 'jpg'),
                                  width = 4,
                                  height = 6,
                                  dpi = 600,
                                  prod = TRUE,
                                  show_title = TRUE,
                                  show_caption = TRUE,
                                  trim_margin = TRUE,
                                  title = NULL,
                                  subtitle = NULL) {

  # - set Tableau color scheme ----
    tableau_colors <- c(
      "King County"            = "#79706E",
      "Age"                    = "#F16913",
      "Big cities"             = "#28A9C5",
      "Birthing person's age"  = "#F16913",
      "Birthing person's detailed race/ethnicity"  = "#30BCAD",
      "Birthing person's education" = "#D4A6C8",
      "Birthing person's ethnicity" = "#30BCAD",
      "Birthing person's race" = "#027B8E",
      "Children in household"  = "#993366",
      "Cities/neighborhoods"   = "#28A9C5",
      "Detailed Asian race/ethnicity" = "#30BCAD",
      "Disability"             = "#55AD56",
      "Education"              = "#D4A6C8",
      "Employment"             = "#D4A6C8",
      "English learner"        = "#8C2D04",
      "Ethnicity"              = "#30BCAD",
      "Foster care"            = "#F498B6",
      "Free lunch"             = "#77559E",
      "Gender"                 = "#F8B620",
      "Grade"                  = "#F16913",
      "Grade level"            = "#F16913",
      "Homeless"               = "#993366",
      "Household income"       = "#7A0177",
      "Migrant"                = "#30BCAD",
      "Military Service"       = "#A27099",
      "Nativity"               = "#95CECF",
      "Neighborhood poverty"   = "#7A0177",
      "NonSeattle"             = "#2C7BB6",
      "Poverty"                = "#77559E",
      "Race"                   = "#027B8E",
      "Race/ethnicity"         = "#027B8E",
      "Regions"                = "#2C7BB6",
      "School district"        = "#2C7BB6",
      "Sexual orientation"     = "#FFDA66",
      "Special education"      = "#55AD56",
      "Transgender"            = "#17BECF",
      "Zip code"               = "#28A9C5"
    )

  # - validate arguments ----
    if (missing(table_name) || !is.character(table_name) || length(table_name) != 1 || is.na(table_name)) {
      stop("\n\U1F6D1 `table_name` must be a single (length 1), non-NA character string.")
    } else {table_name <- gsub('_results', '', table_name)}

    if (!is.null(indicator_key) && (!is.character(indicator_key) || anyNA(indicator_key))) {
      stop("\n\U1F6D1 `indicator_key` must be NULL or a character vector with no missing values.")
    }

    if (is.null(cat1_varname)) {
      warning("\n\U26A0\UFE0F`cat1_varname` is NULL: \nthe graph will show ALL available cat1 data ",
              "rather than the typically expected subset (e.g., county, region, race).")
    } else if (!is.character(cat1_varname) || anyNA(cat1_varname)) {
      stop("\n\U1F6D1 `cat1_varname` must be NULL or a character vector with no missing values.")
    }

    if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
      stop("\n\U1F6D1 `output_dir` must be a single (length 1), non-NA character string.")
    }

    image_type <- match.arg(image_type)

    if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
      stop("\n\U1F6D1 `width` must be a single positive number.")
    }
    if (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0) {
      stop("\n\U1F6D1 `height` must be a single positive number.")
    }
    if (!is.numeric(dpi) || length(dpi) != 1 || is.na(dpi) || dpi <= 0) {
      stop("\n\U1F6D1 `dpi` must be a single positive number.")
    }
    if (!is.logical(prod) || length(prod) != 1 || is.na(prod)) {
      stop("\n\U1F6D1 `prod` must be a single logical value (TRUE or FALSE).")
    }
    if (!is.logical(chi) || length(chi) != 1 || is.na(chi)) {
      stop("\n\U1F6D1 `chi` must be a single logical value (TRUE or FALSE).")
    }
    if (!is.logical(show_title) || length(show_title) != 1 || is.na(show_title)) {
      stop("\n\U1F6D1 `show_title` must be a single logical value (TRUE or FALSE).")
    }
    if (!is.logical(show_caption) || length(show_caption) != 1 || is.na(show_caption)) {
      stop("\n\U1F6D1 `show_caption` must be a single logical value (TRUE or FALSE).")
    }
    if (!is.logical(trim_margin) || length(trim_margin) != 1 || is.na(trim_margin)) {
      stop("\n\U1F6D1 `trim_margin` must be a single logical value (TRUE or FALSE).")
    }

    if (!is.null(title) && (!is.character(title) || length(title) != 1 || is.na(title))) {
      stop("\n\U1F6D1 `title` must be NULL or a single (length 1), non-NA character string.")
    }
    if (!is.null(subtitle) && (!is.character(subtitle) || length(subtitle) != 1 || is.na(subtitle))) {
      stop("\n\U1F6D1 `subtitle` must be NULL or a single (length 1), non-NA character string.")
    }

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # - connect & pull data ----
    db_cxn <- apde.etl::create_db_connection(server = 'phextractstore', prod = prod)
    on.exit(DBI::dbDisconnect(db_cxn), add = TRUE)

    validTables <- setdiff(grep('_results$', DBI::dbListTables(db_cxn), value = TRUE), 'apcd_qa_results')
    if (!paste0(table_name, '_results') %in% validTables) {
      stop("\n\U1F6D1 `table_name` must be one of the tables in CHI Prod ending in '_results'.\n",
           "Valid table names include: \n", paste0(gsub('_results', '', validTables), collapse = ', '))
    }

    selectEst <- paste0(
      "SELECT indicator_key, year, cat1, cat1_varname, cat1_group, ",
      "result, lower_bound, upper_bound, suppression, caution ",
      "FROM APDE.", table_name, "_results",
      " WHERE tab IN ('demgroups', '_kingcounty')",
      if (chi) " AND chi = 1" else "")

    # get unique values because sometimes King County is in tab = '_kingcounty' AND tab = 'demgroups'
    CHIestimates <- unique(rads::string_clean(data.table::setDT(DBI::dbGetQuery(db_cxn, selectEst))))

    selectMeta <- paste0(
      "SELECT indicator_key, result_type ",
      "FROM APDE.", table_name, "_metadata")
    CHImetadata <- rads::string_clean(data.table::setDT(DBI::dbGetQuery(db_cxn, selectMeta)))

    selectTitle <- 'SELECT indicator_key, title FROM [PHExtractStore].[APDE].[indicators_titles]'
    CHItitle <- unique(rads::string_clean(data.table::setDT(DBI::dbGetQuery(db_cxn, selectTitle))))
    CHItitle[, title := gsub(', King County', '', title)]

    CHIestimates <- merge(CHIestimates, CHImetadata, by = 'indicator_key', all.x = T, all.y = F)

    CHIestimates <- merge(CHIestimates, CHItitle, by = 'indicator_key', all.x = T, all.y = F)

  # - subset / filter data ----
    ## - indicator_key----
    ik_requested <- indicator_key
    if (is.null(ik_requested)) {
      ik_requested <- unique(CHIestimates[['indicator_key']])
    } else if (!all(ik_requested %in% unique(CHIestimates[['indicator_key']]))) {
      stop("\n\U1F6D1 `indicator_key` value(s) not available in [PHExtractStore].[APDE].[", table_name, "]: ",
           paste0(setdiff(ik_requested, unique(CHIestimates[['indicator_key']])), collapse = ', '))
    } else {
      CHIestimates <- CHIestimates[indicator_key %in% ik_requested]
    }

    ## - cat1_varname ----
    # fall back from race4 to race3 if need be
    cv_requested <- cat1_varname
    if (!is.null(cv_requested)) {
      available_cv <- unique(CHIestimates[['cat1_varname']])
      if ('race4' %in% cv_requested & 'race4' %notin% available_cv & 'race3' %in% available_cv) {
        cv_requested <- gsub('race4', 'race3', cv_requested)
      }
      if (!all(cv_requested %in% available_cv)) {
        stop("\n\U1F6D1 `cat1_varname` value(s) not available in [PHExtractStore].[APDE].[", table_name, "]: ",
             paste0(setdiff(cv_requested, available_cv), collapse = ', '))
      }
      CHIestimates <- CHIestimates[cat1_varname %in% cv_requested]
    }

    if (nrow(CHIestimates) == 0) {
      stop("\n\U1F6D1 No rows remain in [PHExtractStore].[APDE].[", table_name, "] after filtering. ",
           "Check `indicator_key` and `cat1_varname`.")
    }

  # - bar label helpers ----
    # complicated! But critical so that all bar labels are visible, even when small in value
    # used AI to come up with this solution

    # font size (ggplot2 'size', i.e. mm) used for the value drawn on each bar
    bar_label_size <- 12 * 0.8 * 1.1 / ggplot2::.pt

    # width, in inches, that each label will occupy when drawn
    # geom_text()'s `size` is in mm, while grid wants points ... therefore we need conversion
    # greedy word wrap: breaks each line of `text` (existing '\n' are kept) so that none
    # is wider than `max_in` inches when drawn at `fontsize` points. A single word wider
    # than `max_in` is left on its own line.
    wrap_to_width <- function(text, max_in, fontsize) {
      width_of <- function(s) grid::convertWidth(
        grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontsize = fontsize))),
        "in", valueOnly = TRUE)
      wrap_line <- function(line) {
        words <- strsplit(line, ' ', fixed = TRUE)[[1]]
        out <- character(0)
        current <- ''
        for (w in words) {
          candidate <- if (nzchar(current)) paste(current, w) else w
          if (nzchar(current) && width_of(candidate) > max_in) {
            out <- c(out, current)
            current <- w
          } else {
            current <- candidate
          }
        }
        paste(c(out, current), collapse = '\n')
      }
      paste(vapply(strsplit(text, '\n', fixed = TRUE)[[1]], wrap_line, character(1), USE.NAMES = FALSE),
            collapse = '\n')
    }

    text_width_in <- function(labels, size) {
      fontsize <- size * (72.27 / 25.4)
      vapply(labels, function(lab) {
        if (is.na(lab)) return(NA_real_)
        grid::convertWidth(
          grid::grobWidth(grid::textGrob(lab, gp = grid::gpar(fontsize = fontsize))),
          "in", valueOnly = TRUE)
      }, numeric(1), USE.NAMES = FALSE)
    }

    # inches of the image actually available to the bars, i.e. the total width
    # less the y axis labels, margins and other fixed stuffs. Measured off the
    # generated plot (on a null device of the output's dimensions) so it stays
    # right if the theme changes; falls back to a rough share of `width` if the
    # measurement fails for any reason.
    panel_width_in <- function(plot, width, height) {
      fallback <- width * 0.7
      opened <- tryCatch({grDevices::pdf(NULL, width = width, height = height); TRUE},
                         error = function(e) FALSE)
      if (!opened) return(fallback)
      on.exit(grDevices::dev.off(), add = TRUE)
      # the panel is the one column measured in 'null' units, which converts to
      # zero inches, so whatever the absolute columns don't use is the panel
      # suppressWarnings(): this build is only a measurement, so it should not
      # repeat the warnings the real render will provide anyway
      measured <- tryCatch(suppressWarnings({
        gt <- ggplot2::ggplotGrob(plot)
        width - sum(grid::convertWidth(gt$widths, "in", valueOnly = TRUE))
      }), error = function(e) NA_real_)
      if (!is.finite(measured) || measured <= 0) fallback else measured
    }

    # gap left between the end of a bar and a label placed outside it
    label_gap_in <- 0.08

  # - cat1_group ordering helpers ----
    # Some cat1_group values are numeric bands: ages ('<18', '18-40', '41-60',
    # '61+'), neighborhood poverty ('<10%', '10-19.9%'), etc. Sorting those
    # as text is wrong because '<18' would be last since '<' sort after digits.
    # Similarly, standard sorting would put '5-9' after '10-14', because 5 > 1.
    #
    # When every cat1_group value can be parsed as a band, rank on the band's lower
    # bound. Then put open ended low bands (e.g., '<18') ahead of a band starting at
    # the same number. E.g., '<18' comes before '18-25'. Similarly, ensure that
    # open upper bands (e.g., '65+') are placed at the end.

    # Anything that isn't a band (e.g., region or race names) gets all zeros,
    # are then sorted alphabetically as a tie breaker.

    band_rank <- function(x) {
      named <- !is_catchall(x)             # catch-alls are ranked last regardless
      x_nocomma <- gsub('(?<=[0-9]),(?=[0-9]{3})', '', x, perl = TRUE) # thousands separators, so '50,000' is read as 50000 rather than 50 and 000
      nums <- regmatches(x_nocomma, gregexpr('[0-9]+\\.?[0-9]*', x_nocomma)) # extract all integer or decimals and return them as a list
      pick <- function(i) suppressWarnings(as.numeric(vapply(
        nums, function(n) if (length(n) >= i) n[i] else NA_character_, character(1)))) # pick the `i`th number from the list
      lower <- pick(1) # 1st number = the band's lower bound, the main sort key
      if (anyNA(lower[named])) return(rep(0, length(x))) # invalid named ranges → rank 0
      upper <- pick(2)
      upper[is.na(upper)] <- Inf # '65+' and bare numbers are open above
      lower[!named] <- Inf # Inf on both bounds parks catch-alls after every real band
      upper[!named] <- Inf
      open_below <- grepl('^\\s*(<|under\\b|less than)', x, ignore.case = TRUE) # starts below its number
      order(order(lower, !open_below, upper)) # not a typo! order() twice = ranks, aligned to input rows
    }

    # catch-all buckets ('Other', 'Other race', 'Unknown') belong after the named
    # categories no matter how they sort alphabetically. Otherwise, 'Other' would
    # land between 'NHPI' and 'White'. Kept separate from band_rank() so it applies to
    # band and non-band groups alike.
    is_catchall <- function(x) {
      grepl('^\\s*(other|unknown|multiple)\\b', x, ignore.case = TRUE)
    }

  # - build & save one plot per indicator ----
    saved_files <- character(0)

    for (ik in ik_requested) {

      dt_ik <- CHIestimates[indicator_key == ik]
      # most recent estimate only. One year is chosen for the whole indicator, so
      # every bar on a graph is from the same year regardless of tab or cat1.
      # `year` is a character column and can be a single year ('2023') or a
      # multi-year range ('2020-2024'), so compare on the rightmost 4 characters
      # (the ending year) converted to integer, rather than on `year` itself.
      dt_ik[, year_end := as.integer(rads::substrRight(year, 1, 4))]
      if (any(!is.na(dt_ik[['year_end']]))) {
        dt_ik <- dt_ik[year_end == max(year_end, na.rm = TRUE)]
      }
      # a single indicator can report both a 1-year and a multi-year estimate that
      # end in the same year (e.g. '2020' and '2016-2020'), which could leave two
      # estimates per cat1_group and mess up the graphs. Break the tie
      # on the number of years covered and keep the widest, i.e. prefer the 5-year
      # range, which is the more stable estimate.
      if (length(unique(dt_ik[['year']])) > 1) {
        # anything that isn't a plain 'NNNN-NNNN' range counts as a single year,
        # which also keeps an unexpected format from throwing a coercion warning
        dt_ik[, year_span := 1L]
        dt_ik[grepl('^[0-9]{4}-[0-9]{4}$', year),
              year_span := as.integer(substr(year, 6, 9)) - as.integer(substr(year, 1, 4)) + 1L]
        dt_ik <- dt_ik[year_span == max(year_span, na.rm = TRUE)]
        dt_ik[, year_span := NULL]
      }
      dt_ik[, year_end := NULL]

      if (nrow(dt_ik) == 0) next

      # at this point dt_ik should contain one row per cat1_group since identical rows
      # have been collapsed and a data for a single time period was just chosen.
      # If there are still duplicates, that means there is a problem in the underlying
      # SQL data and a human needs to figure out where things went off the rails
      dup_groups <- unique(dt_ik[['cat1_group']][duplicated(dt_ik[['cat1_group']])])
      if (length(dup_groups) > 0) {
        stop("\n\U1F6D1 `indicator_key` '", ik, "' has more than one estimate for the same ",
             "`cat1_group` in [PHExtractStore].[APDE].[", table_name, "_results], even after ",
             "collapsing identical rows, and keeping a single year (",
             paste0(unique(dt_ik[['year']]), collapse = ', '), ").\n",
             "Duplicated cat1_group: ", paste0(dup_groups, collapse = ', '), "\n",
             "These rows differ in something other than `tab`, so please inspect the SQL ",
             "table directly to figure out which estimate is correct.")
      }

      # bar labels: proportions are displayed as percents ('17.5%'), dollars as
      # whole dollars with a comma between thousands ('$123,456'), and everything
      # else (e.g. rates, counts) as a plain number with exactly one decimal place,
      # so that whole numbers keep their trailing zero ('17.0' rather than '17'),
      # with a comma between thousands ('1,234.5').
      # Rows with no result (e.g. suppressed) get no label; the suppression symbol
      # for those rows is drawn on its own below.
      dt_ik[, label := NA_character_]
      dt_ik[!is.na(result) & result_type %in% 'proportion',
            label := sprintf("%.1f%%", rads::round2(result * 100, 1))]
      dt_ik[!is.na(result) & result_type %in% 'dollars',
            label := paste0('$', formatC(rads::round2(result, 0), format = 'f', digits = 0, big.mark = ','))]
      dt_ik[!is.na(result) & !result_type %in% c('proportion', 'dollars'),
            label := formatC(result, format = 'f', digits = 1, big.mark = ',')]
      dt_ik[!is.na(label) & !is.na(suppression) & suppression != '', label := paste0(label, suppression)]
      dt_ik[!is.na(label) & !is.na(caution) & caution != '', label := paste0(label, caution)]

      # order cat1_group top-to-bottom, on four keys in this priority order:
      #
      #   1. King County first. FALSE sorts before TRUE, hence the negated test; if
      #      chi_geo_kc was not requested the term is constant across all rows and
      #      changes nothing.
      #   2. the user's submitted cat1_varname order, so groups appear in the sequence
      #      they were asked for. With cat1_varname = NULL there is no such order,
      #      so every row gets NA and move on to key 3.
      #   3. cat1 alphabetically.
      #   4. within a group: use helper function made above so that numeric bands
      #      are ordered by value (ages etc.), catch-all buckets ('Other') are ordered
      #      last, and plain alphabetical ordering for everything else.
      dt_ik[, band_order := band_rank(cat1_group), by = cat1_varname]
      dt_ik[, catchall_order := as.integer(is_catchall(cat1_group))]
      dt_ik[, group_priority := if (is.null(cv_requested)) NA_integer_
                                else match(cat1_varname, cv_requested)]

      # 'kingco' is the older HYS spelling of 'chi_geo_kc'; both name the county
      group_order <- dt_ik[order(cat1_varname %notin% c('chi_geo_kc', 'kingco'), group_priority, cat1,
                                 catchall_order, band_order, cat1_group)]
      top_to_bottom <- group_order[['cat1_group']]
      # coord_flip() puts the LAST factor level at the top of the graph, so the
      # levels vector must be the reverse of what we want from top to bottom
      dt_ik[, cat1_group := factor(cat1_group, levels = rev(top_to_bottom))]

      # horizontal separators between groups, where a group = a unique cat1_varname.
      # We later use coord_flip(), so here use geom_vline() which will then (after
      # the coordinate flip) be displayed as a horizontal line.
      n_groups <- length(top_to_bottom)
      varname_changes <- which(group_order[['cat1_varname']][-1] != group_order[['cat1_varname']][-n_groups])
      group_boundaries <- n_groups - varname_changes + 0.5

      # cat1 must match the standard Tableau Style Guide categories defined at the top
      cat1_values <- unique(dt_ik[['cat1']])
      if (!all(cat1_values %in% names(tableau_colors))) {
        stop("\n\U1F6D1 `cat1` value(s) in [PHExtractStore].[APDE].[", table_name, "] do not match ",
             "the standard Tableau Style Guide categories. Unrecognized cat1: ",
             paste0(setdiff(cat1_values, names(tableau_colors)), collapse = ', '),
             '\nYou will likely have to update `tableau_colors` at the top of `chi_plot_demographics()`.')
      }

      # title: the indicator's CHI title. Falls back to the indicator_key itself when
      # [APDE].[indicators_titles] has no title for it. A user-supplied `title`
      # replaces it.
      if (!is.null(title)) {
        plot_title <- title
      } else {
        plot_title <- unique(dt_ik[['title']])
        plot_title <- plot_title[!is.na(plot_title) & plot_title != '']
        plot_title <- if (length(plot_title) > 0) plot_title[1] else ik
      }
      plot_subtitle <- subtitle
      if (!show_title) {
        plot_title <- NULL
        plot_subtitle <- NULL
      }

      # caption: generic CHI caption
      # The caption is drawn from the left edge of the image (see plot.caption.position
      # below) and is word-wrapped to the width available between the plot margins, so
      # it is never cut off on a narrow image.
      plot_caption <- paste0(
        '^ = Data suppressed if too few cases to protect confidentiality and/or report reliable rates\n',
        '! = Interpret with caution; sample size is small so estimate is imprecise')
      if (show_caption) {
        margin_in <- if (trim_margin) 5 / 72.27 else 1 / 2.54 # must match plot.margin below
        plot_caption <- wrap_to_width(plot_caption, (width - 2 * margin_in) * 0.97, # 3% safety for font differences
                                      fontsize = 12 * 0.6) # apde theme base_size 12 x rel(0.6)
      } else {
        plot_caption <- NULL
      }

      # everything except the value axis and the bar labels, both of which depend
      # on how much room the labels need (see below). Layer order still puts the
      # labels on top, since they are added last.
      # each layer is handed only the rows it can actually draw. Suppressed rows
      # have no result and no bounds, and ggplot2 would drop them anyway -- but
      # noisily, with a 'Removed n rows containing missing values' warning per
      # layer, per graph. cat1_group is a factor carrying every level, so the
      # suppressed groups keep their slot on the axis regardless.
      base_plot <- ggplot2::ggplot(dt_ik, ggplot2::aes(x = cat1_group, y = result, fill = cat1)) +
        ggplot2::geom_bar(data = dt_ik[!is.na(result)], stat = "identity") +
        ggplot2::coord_flip(clip = 'off') + # clip off so the group separators can reach out over the y axis labels
        ggplot2::geom_point(data = dt_ik[!is.na(lower_bound)],
                            ggplot2::aes(y = lower_bound), shape = 16, size = 1, show.legend = FALSE) +
        ggplot2::geom_point(data = dt_ik[!is.na(upper_bound)],
                            ggplot2::aes(y = upper_bound), shape = 16, size = 1, show.legend = FALSE) +
        ggplot2::geom_segment(data = dt_ik[!is.na(lower_bound) & !is.na(upper_bound)],
                              ggplot2::aes(y = lower_bound, yend = upper_bound, x = cat1_group, xend = cat1_group)) +
        # drop = FALSE: the layers above are filtered to the rows they can draw,
        # so a fully suppressed cat1_group appears in no bar/point/segment layer.
        # Without this the scale would rebuild its order from the layers and
        # append that group at the end, i.e. the top of a flipped graph, instead
        # of honouring the factor levels set above.
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_fill_manual(values = tableau_colors) +
        ggplot2::labs(title = plot_title,
                      subtitle = plot_subtitle,
                      x = NULL,
                      y = NULL,
                      caption = plot_caption) +
        # APDE standard look: theme_minimal() plus the tweaks below. "sans" maps to
        # Arial on Windows
        ggplot2::theme_minimal(base_size = 12, base_family = 'sans') +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = ggplot2::rel(1.3), face = 'bold', hjust = 0.5,
                                             color = 'black', margin = ggplot2::margin(b = 10)),
          plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1), face = 'plain', hjust = 0.5,
                                                margin = ggplot2::margin(b = 10)),
          axis.title = ggplot2::element_text(size = ggplot2::rel(1), face = 'bold',
                                             margin = ggplot2::margin(t = 10, b = 10)),
          axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.title = ggplot2::element_text(size = ggplot2::rel(1), face = 'bold', color = 'black'),
          legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
          legend.position = 'right',
          plot.caption = ggplot2::element_text(size = ggplot2::rel(0.6), hjust = 0,
                                               margin = ggplot2::margin(t = 10)),
          plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = 'cm'),
          panel.spacing = grid::unit(0, 'lines'),
          strip.placement = 'outside',
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(face = 'bold', size = ggplot2::rel(1))
        ) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       legend.position = "none",
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.1)),
                       plot.caption = ggplot2::element_text(margin = ggplot2::margin(t = 2, unit = 'pt')), # increase to push caption further from bars
                       plot.caption.position = 'plot' # start at the left edge of the image rather than the panel
                      )

      # trimmed margins were decided via repeated testing
      if (trim_margin) {
        base_plot <- base_plot +
          ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 5, b= 5, l = 5, unit = 'pt'))
      }

      # ensure upper_bound point is never truncated by adding 5% buffer
      y_axis_values <- c(dt_ik[['result']], dt_ik[['upper_bound']])
      y_axis_values <- y_axis_values[is.finite(y_axis_values)]
      y_max <- if (length(y_axis_values) > 0) max(y_axis_values) * 1.05 else 1
      if (!is.finite(y_max) || y_max <= 0) y_max <- 1

      # a short bar can't fit its own label, so compare the bar's drawn length
      # against the label's drawn width. Labels that fit stay white and centered
      # inside the bar; the rest are drawn in black just past the end of the bar,
      # where they are legible against the panel background.
      panel_in <- panel_width_in(base_plot, width, height)
      dt_ik[, label_w_in := text_width_in(label, bar_label_size)]

      # outside labels need to start after the CI so doesn't sit on the error bar
      dt_ik[, label_anchor := pmax(result, upper_bound, na.rm = TRUE)] # check across result & upper_bound in case upper_bound is missing

      # moving a label outside its bar needs room to the right of that bar, which
      # can push out y_max, which shrinks every bar, which can push another label
      # outside -- so settle y_max and the in/out decision together. y_max only
      # ever grows here, so this converges. Limit to 5 iterations.
      for (pass in seq_len(5)) {
        dt_ik[, label_inside := !is.na(label) &
                (result / y_max) * panel_in >= label_w_in + 2 * label_gap_in] # bar inches vs label inches
        outside <- dt_ik[!is.na(label) & !label_inside] # the labels that must move out
        if (nrow(outside) == 0) break # everything fits, so y_max is final
        # each outside label needs anchor + (label_w + gap) inches worth of axis,
        # i.e. y_max >= anchor / (1 - (label_w + gap)/panel_in)
        room <- pmax(1 - (outside[['label_w_in']] + 2 * label_gap_in) / panel_in, 0.05) # axis share left for the bar
        y_needed <- max(outside[['label_anchor']] / room, na.rm = TRUE) # the neediest label sets the axis
        if (!is.finite(y_needed) || y_needed <= y_max * 1.001) break # no real growth → settled
        y_max <- y_needed # grow the axis, then re-test every label next pass
      }

      # group separators run from the right edge of the panel left across the y axis
      # labels. annotation_custom() is used because, unlike geom_segment(), it is not
      # censored by the y scale limits when it starts left of zero. The overhang is
      # the widest cat1_group label (drawn at the axis font size, 20 pt, which
      # text_width_in() wants in mm) plus a small allowance for the axis text margin,
      # converted from inches to y axis units.
      if (length(group_boundaries) > 0) {
        axis_label_w_in <- max(text_width_in(levels(dt_ik[['cat1_group']]), bar_label_size), na.rm = TRUE)
        sep_start <- -(axis_label_w_in + 0.03) / panel_in * y_max
        separators <- lapply(group_boundaries, function(b) {
          ggplot2::annotation_custom(
            grid::segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                               gp = grid::gpar(col = 'black', lwd = 1 * ggplot2::.pt)),
            xmin = b, xmax = b, ymin = sep_start, ymax = y_max)
        })
        base_plot <- base_plot + separators
      }

      myplot <- base_plot +
        ggplot2::scale_y_continuous(limits = c(0, y_max),
                                    expand = ggplot2::expansion(mult = c(0, 0))) +
        # interior label
        ggplot2::geom_text(data = dt_ik[label_inside == TRUE],
                           ggplot2::aes(label = label), hjust = 0.5,
                           position = ggplot2::position_stack(0.5),
                           color = 'white', size = bar_label_size) +
        # exterior label
        ggplot2::geom_text(data = dt_ik[!is.na(label) & label_inside == FALSE],
                           ggplot2::aes(y = label_anchor, label = label), hjust = 0,
                           nudge_y = y_max * (label_gap_in / panel_in),
                           color = 'black', size = bar_label_size) +
        # suppression label
        ggplot2::geom_text(data = dt_ik[!is.na(suppression) & suppression != ''],
                            ggplot2::aes(y = 0, label = suppression), hjust = 0)

      safe_ik <- gsub('[^A-Za-z0-9_-]+', '_', ik)
      filename <- paste0(table_name, "_", safe_ik, "_", Sys.Date(), ".", image_type)
      filepath <- file.path(output_dir, filename)

      ggplot2::ggsave(filepath, myplot, width = width, height = height, dpi = dpi, units = "in")
      saved_files <- c(saved_files, filepath)
    }

  # - return ----
    invisible(saved_files)
}
