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
#'   and `'kingco'` are recognized as naming the county. When
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
#'   Default `cat1_varname = c('chi_geo_kc', 'race4', 'chi_geo_region')`.
#'
#'   `'race3'` and `'race4'` are two different ways of categorizing race, with
#'   overlapping groups, and some indicators have both. They are handled for each
#'   `indicator_key` as follows:
#'   - `cat1_varname = NULL`: `'race4'` is plotted and `'race3'` is not, for
#'   indicators that have both.
#'   - `'race4'` requested, but an indicator only has `'race3'`: `'race3'` is
#'   used for that indicator, in the position given to `'race4'`, and a warning
#'   lists the affected `indicator_key` values.
#'   - `'race3'` requested: `'race3'` is used, never swapped.
#'   - Both requested: the function stops with an error, since graphing both
#'   would draw duplicate bars.
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
#' @param width Numeric. Image width in inches. Must be at least 4; narrower
#'   images leave too little room for the y axis labels, bars and caption.
#'
#'   Default `width = 4`.
#'
#' @param height Numeric. Image height in inches.
#'
#'   Default `height = NULL`, which calculates the height of each image so that
#'   every bar gets the same vertical space (0.4 inches, a value chosen for
#'   appearance) whether an indicator has few bars or many. The height is the sum
#'   of:
#'   * 0.4 inches for each bar. Bars include any suppressed `cat1_group`, which
#'   keeps its slot on the graph.
#'   * 1.2 bars' worth of padding that ggplot2 adds above the first and below the
#'   last bar (0.6 of a bar at each end).
#'   * The space for everything that is not a bar, which is measured from the plot
#'   rather than assumed: the margins, plus whichever of the title, subtitle and
#'   caption are drawn, including any extra lines when they wrap.
#'
#'   So the height is `0.4 * (number of bars + 1.2)` plus that measured space.
#'   Because the number of bars can differ by `indicator_key`, images from the
#'   same call may have different heights.
#'
#'   Supply a number to use the same fixed height for every image. The measured
#'   non-bar space is unchanged, so the bars share whatever height is left over
#'   and are thicker in a taller image and thinner in a shorter one. The function
#'   stops with an error if `height` is too small to leave any room for bars.
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
#' # lower resolution image (e.g., for a quick slide or a draft review),
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
                                  height = NULL,
                                  dpi = 600,
                                  prod = TRUE,
                                  show_title = TRUE,
                                  show_caption = TRUE,
                                  trim_margin = TRUE,
                                  title = NULL,
                                  subtitle = NULL) {

  # - read Tableau color scheme ----
    # one row per `cat1` (Tableau Style Guide category) with its hex color; to add a
    # category, add a row to inst/ref/tableau_colors.csv
    tableau_colors <- chi_plot_read_colors()

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

    if (all(c('race3', 'race4') %in% cat1_varname)) {
      stop("\n\U1F6D1 `cat1_varname` contains both 'race3' and 'race4'. These are two different ways of ",
           "categorizing race, with overlapping groups (e.g., both have 'Black'), so graphing them together ",
           "would draw duplicate bars. Please request only one of them.")
    }

    if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
      stop("\n\U1F6D1 `output_dir` must be a single (length 1), non-NA character string.")
    }

    image_type <- match.arg(image_type)

    if (!is.numeric(width) || length(width) != 1 || is.na(width) || width < 4) {
      stop("\n\U1F6D1 `width` must be a single number of at least 4 (inches). ",
           "Narrower images do not leave enough room for the y axis labels, bars and caption.")
    }
    if (!is.null(height) && (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0)) {
      stop("\n\U1F6D1 `height` must be NULL or a single positive number.")
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
    # 'race3' and 'race4' are two ways of categorizing race, and some indicators have both.
    # They share cat1_group values (e.g., 'Black'), so graphing both would draw duplicate
    # bars. Race is handled one indicator_key at a time:
    #   - cat1_varname = NULL: keep race4 and drop race3 for indicators that have both.
    #   - 'race4' requested but an indicator only has race3: use race3 for that indicator,
    #     and warn about it. The race3 rows are relabeled 'race4' so that they take the
    #     place in the graph that the user gave to race4.
    #   - 'race3' requested: used as is, never swapped.
    #   - both requested: already stopped with an error when the arguments were validated.
    cv_requested <- cat1_varname
    race <- chi_plot_resolve_race(CHIestimates, cv_requested)
    CHIestimates <- race$data
    race3_substituted <- race$substituted

    if (!is.null(cv_requested)) {
      available_cv <- unique(CHIestimates[['cat1_varname']])
      if (!all(cv_requested %in% available_cv)) {
        stop("\n\U1F6D1 `cat1_varname` value(s) not available in [PHExtractStore].[APDE].[", table_name, "]: ",
             paste0(setdiff(cv_requested, available_cv), collapse = ', '))
      }
      CHIestimates <- CHIestimates[cat1_varname %in% cv_requested]
    }

    if (length(race3_substituted) > 0) {
      shown <- race3_substituted[seq_len(min(10, length(race3_substituted)))]
      warning("\n\U26A0\UFE0F \U26A0\UFE0F \U26A0\UFE0F `cat1_varname` asked for 'race4', but 'race4' is not available ",
              "in [PHExtractStore].[APDE].[", table_name, "_results] for ", length(race3_substituted),
              " indicator_key(s). 'race3' was used for them instead:\n",
              paste0(shown, collapse = ', '),
              if (length(race3_substituted) > length(shown)) paste0(', ... (', length(race3_substituted) - length(shown), ' more)') else '',
              "\nThe race3 and race4 categories differ, so confirm that race3 is acceptable for these graphs.",
              call. = FALSE, immediate. = TRUE)
    }

    if (nrow(CHIestimates) == 0) {
      stop("\n\U1F6D1 No rows remain in [PHExtractStore].[APDE].[", table_name, "] after filtering. ",
           "Check `indicator_key` and `cat1_varname`.")
    }

    # Need to treat AIC chi_race as if they were one cat1_varname for the sake of plotting
    CHIestimates[grepl('^chi_race_aic_', cat1_varname), cat1_varname := 'chi_race_aic']
    cv_requested <- unique(sub("^chi_race_aic_.*", "chi_race_aic", cv_requested))

  # - sizes and spacing ----
    # complicated! But critical so that all bar labels are visible, even when small in value
    # used AI to come up with this solution

    # theme base font size, in points. Title, subtitle and caption are multiples of this.
    theme_base_size <- 12

    # title font size as a multiple of the base size (also used to wrap the title)
    title_rel <- 1.2

    # font size of the y axis labels, in points: the base size x 0.8 (the theme_minimal-style
    # scaling of axis text) x 1.1. Change the 1.1 to make the y axis labels bigger or smaller.
    axis_text_y_pt <- theme_base_size * 0.8 * 1.1

    # font size of the value drawn on each bar, set equal to the y axis labels. geom_text()
    # sizes are in mm rather than points, so divide by ggplot2::.pt (points per mm)
    bar_label_scalar <- 1.2 # how much bigger bar labels should be than axis labels
    bar_label_size <- bar_label_scalar * axis_text_y_pt / ggplot2::.pt

    # vertical space given to each bar, in inches, when `height` is not supplied. This
    # is NOT calculated: it was selected for aesthetics, by looking at graphs with 1 to
    # 21 bars. Make it bigger for taller, airier bars; smaller for more compact ones.
    bar_spacing_in <- 0.4

    # ggplot2 pads a discrete axis by 0.6 of a bar slot above the first bar and 0.6 below
    # the last (its default), so the bars' area is 1.2 slots taller than the bars alone
    axis_padding_slots <- 1.2

    # gap left between the end of a bar and a label placed outside it
    label_gap_in <- 0.08

  # - build & save one plot per indicator ----
    saved_files <- character(0)

    for (ik in ik_requested) {

      dt_ik <- CHIestimates[indicator_key == ik]
      # most recent estimate only. One year is chosen for the whole indicator, so every
      # bar on a graph is from the same year regardless of tab or cat1. See
      # chi_plot_latest_year().
      dt_ik <- chi_plot_latest_year(dt_ik)

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

      # bar labels (percent, dollars, or a plain number depending on result_type). Rows
      # with no result (e.g. suppressed) get no label; the suppression symbol for those
      # rows is drawn on its own below. See chi_plot_bar_labels().
      dt_ik[, label := chi_plot_bar_labels(result, result_type, caution)]

      # order cat1_group top-to-bottom: King County first, then the user's cat1_varname
      # order, then cat1, then (within a cat1_varname) catch-alls last, numeric bands by
      # value, and everything else alphabetically. See chi_plot_order_groups().
      group_order <- chi_plot_order_groups(dt_ik, cv_requested)
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
             '\nYou will likely have to add the new category to `inst/ref/tableau_colors.csv`.')
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

      # The title and caption are laid out across the whole image (see plot.title.position
      # and plot.caption.position below) and are word-wrapped to the width available
      # between the plot margins, so neither is cut off on a narrow image. The subtitle is
      # not wrapped: its wording is up to the user.
      text_max_in <- (width - 2 * if (trim_margin) 5 / 72.27 else 1 / 2.54) * 0.97 # margins must match plot.margin below; 3% safety for font differences

      if (!is.null(plot_title)) {
        plot_title <- chi_plot_wrap_text(plot_title, text_max_in,
                                    fontsize = theme_base_size * title_rel, fontface = 'bold')
      }

      # caption: generic CHI caption
      plot_caption <- paste0(
        '^ = Data suppressed if too few cases to protect confidentiality and/or report reliable rates\n',
        '! = Interpret with caution; sample size is small so estimate is imprecise')
      if (show_caption) {
        plot_caption <- chi_plot_wrap_text(plot_caption, text_max_in,
                                      fontsize = theme_base_size * 0.6) # matches plot.caption's rel(0.6)
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
        # APDE look: theme_minimal() plus the tweaks below. "sans" maps to Arial on Windows
        ggplot2::theme_minimal(base_size = theme_base_size, base_family = 'sans') +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = ggplot2::rel(title_rel), face = 'bold', hjust = 0.5,
                                             color = 'black', margin = ggplot2::margin(b = 10)),
          plot.title.position = 'plot', # start at the left edge of the image rather than the panel
          plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1), face = 'plain', hjust = 0.5,
                                                margin = ggplot2::margin(b = 10)),
          plot.caption = ggplot2::element_text(size = ggplot2::rel(0.6), hjust = 0,
                                               margin = ggplot2::margin(t = 2, unit = 'pt')), # increase to push caption further from bars
          plot.caption.position = 'plot', # start at the left edge of the image rather than the panel
          # trimmed margins were decided via repeated testing
          plot.margin = if (trim_margin) ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = 'pt')
                        else ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = 'cm'),
          axis.text.y = ggplot2::element_text(size = axis_text_y_pt),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = 'none'
        )

      # ensure upper_bound point is never truncated by adding 5% buffer
      y_axis_values <- c(dt_ik[['result']], dt_ik[['upper_bound']])
      y_axis_values <- y_axis_values[is.finite(y_axis_values)]
      y_max <- if (length(y_axis_values) > 0) max(y_axis_values) * 1.05 else 1
      if (!is.finite(y_max) || y_max <= 0) y_max <- 1

      # image height. Everything that is not the bars (margins, title, subtitle, caption) is
      # measured off the plot. When `height` was left NULL, the bars then get a fixed
      # `bar_spacing_in` each (n_groups counts every bar slot, including suppressed groups)
      # plus the axis padding:
      #   height = measured non-bar space + bar_spacing_in * (n_groups + axis_padding_slots)
      # When the user supplies `height`, the non-bar space is unchanged and the bars share
      # whatever is left, i.e. each bar's spacing = (height - non-bar space) / (n_groups + 1.2)
      non_panel_in <- chi_plot_non_panel_height_in(base_plot, width)
      if (is.null(height)) {
        plot_height <- non_panel_in + bar_spacing_in * (n_groups + axis_padding_slots)
      } else {
        plot_height <- height
        if (plot_height <= non_panel_in) {
          stop("\n\U1F6D1 `height` (", plot_height, " inches) is too small: the margins, title and ",
               "caption alone need about ", round(non_panel_in, 1), " inches, leaving no room for the bars.")
        }
      }

      # a short bar can't fit its own label, so compare the bar's drawn length
      # against the label's drawn width. Labels that fit stay white and centered
      # inside the bar; the rest are drawn in black just past the end of the bar,
      # where they are legible against the panel background.
      panel_in <- chi_plot_panel_width_in(base_plot, width, plot_height)
      dt_ik[, label_w_in := chi_plot_text_width_in(label, bar_label_size)]

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
      # the widest cat1_group label (drawn at the axis font size, which is the same as
      # bar_label_size, in the mm that chi_plot_text_width_in() wants) plus a small allowance for
      # the axis text margin, converted from inches to y axis units.
      if (length(group_boundaries) > 0) {
        axis_label_w_in <- max(chi_plot_text_width_in(levels(dt_ik[['cat1_group']]), bar_label_size), na.rm = TRUE)
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

      ggplot2::ggsave(filepath, myplot, width = width, height = plot_height, dpi = dpi, units = "in")
      saved_files <- c(saved_files, filepath)
    }

  # - return ----
    invisible(saved_files)
}
