# Internal helper functions for chi_plot_demographics()

# chi_plot_read_colors() ----
#' Read the Tableau Style Guide colors
#'
#' @description
#' Reads `inst/ref/tableau_colors.csv`, which holds one hex color for each `cat1`
#' (Tableau Style Guide category), and checks that the file is valid.
#'
#' @return A named character vector of hex colors, where the names are `cat1`
#' values.
#'
#' @keywords internal
chi_plot_read_colors <- function() {
  tableau_file <- system.file('ref', 'tableau_colors.csv', package = 'apde.chi.tools')
  if (tableau_file == '') {
    stop("\n\U1F6D1 Could not find `ref/tableau_colors.csv` in the apde.chi.tools package. ",
         "Try reinstalling the package.")
  }

  tableau_dt <- data.table::fread(tableau_file, colClasses = 'character', encoding = 'UTF-8')

  if (!identical(names(tableau_dt), c('cat1', 'hex')) || anyNA(tableau_dt[['cat1']]) ||
      anyDuplicated(tableau_dt[['cat1']]) > 0 ||
      !all(grepl('^#[0-9A-Fa-f]{6}$', tableau_dt[['hex']]))) {
    stop("\n\U1F6D1 `ref/tableau_colors.csv` must have exactly two columns, `cat1` and `hex`, ",
         "with no missing or duplicated `cat1` values and every `hex` formatted like '#79706E'.")
  }

  stats::setNames(tableau_dt[['hex']], tableau_dt[['cat1']])
}

# chi_plot_resolve_race() ----
#' Choose between race3 and race4 for each indicator
#'
#' @description
#' `race3` and `race4` are two ways of categorizing race, and some indicators have
#' both. They share `cat1_group` values (e.g., 'Black'), so plotting both would draw
#' duplicate bars. This function decides, one `indicator_key` at a time, which to keep.
#'
#' @details
#' * `cat1_varname = NULL`: `race4` is kept and `race3` is dropped for indicators
#'   that have both.
#' * `'race4'` requested, but an indicator only has `race3`: that indicator's `race3`
#'   rows are relabeled `race4`, so they take the place in the graph that the user
#'   gave to `race4`.
#' * `'race3'` requested: nothing is changed.
#'
#' @param dt A data.table with (at least) the columns `indicator_key` and
#' `cat1_varname`. It is not modified.
#' @param cat1_varname Character vector of the requested `cat1_varname` values, or
#' `NULL`.
#'
#' @return A list with two elements: `data`, the resulting data.table, and
#' `substituted`, a character vector of the `indicator_key` values whose `race3`
#' was used in place of a requested `race4`.
#'
#' @keywords internal
chi_plot_resolve_race <- function(dt, cat1_varname) {
  dt <- data.table::copy(dt)
  ik_with_race4 <- unique(dt[['indicator_key']][dt[['cat1_varname']] == 'race4'])
  substituted <- character(0)

  if (is.null(cat1_varname)) {
    dt <- dt[!(dt[['cat1_varname']] == 'race3' & dt[['indicator_key']] %in% ik_with_race4)]
  } else if ('race4' %in% cat1_varname) {
    ik_with_race3 <- unique(dt[['indicator_key']][dt[['cat1_varname']] == 'race3'])
    substituted <- setdiff(ik_with_race3, ik_with_race4)
    if (length(substituted) > 0) {
      to_relabel <- which(dt[['cat1_varname']] == 'race3' & dt[['indicator_key']] %in% substituted)
      data.table::set(dt, i = to_relabel, j = 'cat1_varname', value = 'race4')
    }
  }

  list(data = dt, substituted = substituted)
}

# chi_plot_latest_year() ----
#' Keep only the most recent year of estimates
#'
#' @description
#' Reduces the estimates for one `indicator_key` to a single time period, so that
#' all bars on a graph come from the same year.
#'
#' @details
#' `year` is a character column that can be a single year (e.g., '2023') or a
#' multi-year range (e.g., '2020-2024'). Rows are compared on the ending year, i.e.,
#' the rightmost 4 characters. If a single year and a range end in the same year
#' (e.g., '2020' and '2016-2020'), the widest range wins because it is the more
#' stable estimate. Anything that is not a plain 'NNNN-NNNN' range counts as a single
#' year.
#'
#' @param dt A data.table with a character column named `year`. It is not modified.
#'
#' @return A data.table with the rows for the chosen year. It has zero rows if `dt`
#' had none.
#'
#' @keywords internal
chi_plot_latest_year <- function(dt) {
  year_end <- as.integer(rads::substrRight(dt[['year']], 1, 4))
  if (any(!is.na(year_end))) {
    keep_rows <- which(year_end == max(year_end, na.rm = TRUE))
    dt <- dt[keep_rows]
  }

  if (length(unique(dt[['year']])) > 1) {
    is_range <- grepl('^[0-9]{4}-[0-9]{4}$', dt[['year']])
    year_span <- rep(1L, nrow(dt))
    year_span[is_range] <- as.integer(substr(dt[['year']][is_range], 6, 9)) -
      as.integer(substr(dt[['year']][is_range], 1, 4)) + 1L
    keep_rows <- which(year_span == max(year_span, na.rm = TRUE))
    dt <- dt[keep_rows]
  }

  dt
}

# chi_plot_bar_labels() ----
#' Make the text drawn on each bar
#'
#' @description
#' Formats estimates as bar labels. Proportions are shown as percents ('17.5%'),
#' dollars as whole dollars with a comma between thousands ('$123,456'), and all
#' other types (e.g., rates, counts) as a number with exactly one decimal place and a
#' comma between thousands ('1,234.5'). The caution symbol is then appended.
#'
#' @param result Numeric vector of estimates.
#' @param result_type Character vector, the same length as `result`, of result types
#' (e.g., 'proportion', 'dollars', 'count').
#' @param caution Character vector, the same length as `result`, with the caution
#' symbol (e.g., '!') or `NA` / `''`.
#'
#' @return A character vector the same length as `result`. Rows with no `result`
#' (e.g., suppressed) are `NA`.
#'
#' @examples
#' apde.chi.tools:::chi_plot_bar_labels(
#'   result = c(0.175, 123456.4, 17),
#'   result_type = c('proportion', 'dollars', 'count'),
#'   caution = c('!', NA, NA))
#'
#' @keywords internal
chi_plot_bar_labels <- function(result, result_type, caution) {
  label <- rep(NA_character_, length(result))

  has_result <- !is.na(result)
  is_prop <- has_result & result_type %in% 'proportion'
  is_dollar <- has_result & result_type %in% 'dollars'
  is_other <- has_result & !(result_type %in% c('proportion', 'dollars'))

  label[is_prop] <- sprintf("%.1f%%", rads::round2(result[is_prop] * 100, 1))
  label[is_dollar] <- paste0('$', formatC(rads::round2(result[is_dollar], 0),
                                          format = 'f', digits = 0, big.mark = ','))
  label[is_other] <- formatC(result[is_other], format = 'f', digits = 1, big.mark = ',')

  # no suppression symbol here: a suppressed estimate has no result (NULL in SQL, NA in R),
  # so it has no label to append to. Its '^' is drawn separately by chi_plot_demographics().
  add_caution <- !is.na(label) & !is.na(caution) & caution != ''
  label[add_caution] <- paste0(label[add_caution], caution[add_caution])

  label
}

# chi_plot_is_catchall() ----
#' Identify catch-all categories
#'
#' @description
#' Catch-all buckets (e.g., 'Other', 'Other race', 'Another race', 'Unknown',
#' 'Multiple') belong after the named categories no matter how they sort
#' alphabetically. Otherwise, 'Other' would land between 'NHPI' and 'White'.
#'
#' @param x Character vector of `cat1_group` values.
#'
#' @return A logical vector the same length as `x`.
#'
#' @examples
#' apde.chi.tools:::chi_plot_is_catchall(c('Other', 'Multiple race', 'White', 'Otherwise'))
#'
#' @keywords internal
chi_plot_is_catchall <- function(x) {
  grepl('^\\s*(other|another|unknown|multiple)\\b', x, ignore.case = TRUE)
}

# chi_plot_band_rank() ----
#' Rank numeric bands by value
#'
#' @description
#' Some `cat1_group` values are numeric bands: ages ('<18', '18-40', '61+'),
#' neighborhood poverty ('<10%', '10-19.9%'), incomes ('<$50,000',
#' '$50,000-$99,999'), etc. Sorting those as text is wrong because, for example,
#' '5-9' would come after '10-14'. This function ranks them by value instead.
#'
#' @details
#' Bands are ranked on their lower bound. Open-ended low bands (e.g., '<18') are
#' placed ahead of a band starting at the same number (e.g., '18-25'), and
#' open-ended upper bands (e.g., '65+') come last among bands. Thousands separators
#' are ignored, so '$1,000' is read as 1000. Catch-all categories (see
#' [chi_plot_is_catchall()]) are ranked after every band.
#'
#' If any value that is not a catch-all contains no number (e.g., region or race
#' names), the vector is not a set of bands and every value is ranked 0, which leaves
#' the order to the alphabetical tie breaker used by the caller.
#'
#' @param x Character vector of `cat1_group` values.
#'
#' @return An integer vector the same length as `x` with the rank of each value, or
#' all zeros if `x` is not a set of numeric bands.
#'
#' @examples
#' apde.chi.tools:::chi_plot_band_rank(c('61+', '<18', '18-40', '41-60'))
#' apde.chi.tools:::chi_plot_band_rank(c('$100,000 or more', '<$50,000', '$50,000-$99,999'))
#'
#' @keywords internal
chi_plot_band_rank <- function(x) {
  named <- !chi_plot_is_catchall(x) # catch-alls are ranked last regardless
  x_nocomma <- gsub('(?<=[0-9]),(?=[0-9]{3})', '', x, perl = TRUE) # so '50,000' is read as 50000, not 50 and 000
  nums <- regmatches(x_nocomma, gregexpr('[0-9]+\\.?[0-9]*', x_nocomma)) # every integer or decimal, as a list
  pick <- function(i) suppressWarnings(as.numeric(vapply(
    nums, function(n) if (length(n) >= i) n[i] else NA_character_, character(1)))) # the `i`th number of each value
  lower <- pick(1) # 1st number = the band's lower bound, the main sort key
  if (anyNA(lower[named])) return(rep(0, length(x))) # not bands, so rank 0
  upper <- pick(2)
  upper[is.na(upper)] <- Inf # '65+' and bare numbers are open above
  lower[!named] <- Inf # Inf on both bounds parks catch-alls after every real band
  upper[!named] <- Inf
  open_below <- grepl('^\\s*(<|under\\b|less than)', x, ignore.case = TRUE) # starts below its number
  order(order(lower, !open_below, upper)) # not a typo! order() twice = ranks, aligned to the input
}

# chi_plot_order_groups() ----
#' Order the bars from top to bottom
#'
#' @description
#' Sorts the rows of one indicator into the order the bars are drawn, using four keys
#' in this priority order:
#' 1. King County first.
#' 2. The order the user gave in `cat1_varname`. With `cat1_varname = NULL` there is
#'    no such order, so this key is skipped.
#' 3. `cat1`, alphabetically.
#' 4. Within a `cat1_varname`: catch-all categories last (see
#'    [chi_plot_is_catchall()]), numeric bands by value (see [chi_plot_band_rank()]),
#'    and plain alphabetical order for everything else.
#'
#' @details
#' 'kingco' is the older HYS spelling of 'chi_geo_kc', and both are treated as King
#' County.
#'
#' @param dt A data.table with the columns `cat1_varname`, `cat1`, and `cat1_group`.
#' It is not modified.
#' @param cat1_varname Character vector of the requested `cat1_varname` values, in the
#' order the user gave them, or `NULL`.
#'
#' @return `dt`, sorted from the top bar to the bottom bar.
#'
#' @examples
#' dt <- data.table::data.table(
#'   cat1_varname = c('race4', 'chi_geo_kc', 'race4', 'race4'),
#'   cat1 = c('Race', 'King County', 'Race', 'Race'),
#'   cat1_group = c('White', 'King County', 'Asian', 'Other'))
#' apde.chi.tools:::chi_plot_order_groups(dt, c('race4', 'chi_geo_kc'))
#'
#' @keywords internal
chi_plot_order_groups <- function(dt, cat1_varname) {
  varname <- dt[['cat1_varname']]
  group <- dt[['cat1_group']]

  band_order <- integer(length(group))
  for (v in unique(varname)) {
    in_v <- varname == v
    band_order[in_v] <- chi_plot_band_rank(group[in_v])
  }

  group_priority <- if (is.null(cat1_varname)) rep(NA_integer_, length(group)) else match(varname, cat1_varname)

  row_order <- order(!(varname %in% c('chi_geo_kc', 'kingco')), # FALSE sorts before TRUE, hence the negation
                     group_priority,
                     dt[['cat1']],
                     as.integer(chi_plot_is_catchall(group)),
                     band_order,
                     group)

  dt[row_order]
}

# chi_plot_text_width_in() ----
#' Width of text when drawn on a bar
#'
#' @description
#' Measures how many inches wide each label will be when drawn by
#' [ggplot2::geom_text()] at a given size. `geom_text()` sizes are in mm whereas grid
#' wants points, so this converts between the two.
#'
#' @param labels Character vector of labels. `NA` returns `NA`.
#' @param size Font size in mm, as used by `geom_text()`.
#'
#' @return A numeric vector the same length as `labels`, in inches.
#'
#' @keywords internal
chi_plot_text_width_in <- function(labels, size) {
  fontsize <- size * (72.27 / 25.4)
  vapply(labels, function(lab) {
    if (is.na(lab)) return(NA_real_)
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(lab, gp = grid::gpar(fontsize = fontsize))),
      "in", valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

# chi_plot_wrap_text() ----
#' Word wrap text to fit a width
#'
#' @description
#' Greedy word wrap. Breaks each line of `text` (existing line breaks are kept) so
#' that none is wider than `max_in` inches when drawn. A single word wider than
#' `max_in` is left on its own line.
#'
#' @param text A single character string.
#' @param max_in Maximum width of a line, in inches.
#' @param fontsize Font size, in points.
#' @param fontface Font face, e.g., 'plain' or 'bold'.
#'
#' @return A single character string with `\\n` where lines should break.
#'
#' @keywords internal
chi_plot_wrap_text <- function(text, max_in, fontsize, fontface = 'plain') {
  width_of <- function(s) grid::convertWidth(
    grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontsize = fontsize, fontface = fontface))),
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

# chi_plot_panel_width_in() ----
#' Width of the bars' area
#'
#' @description
#' Inches of the image actually available to the bars, i.e., the total width less the
#' y axis labels, margins, and other fixed elements. It is measured off the plot (on a
#' null device of the output's dimensions) so it stays right if the theme changes.
#'
#' @details
#' The panel is the one column measured in 'null' units, which converts to zero
#' inches, so whatever the absolute columns do not use is the panel.
#'
#' @param plot A ggplot object.
#' @param width Image width, in inches.
#' @param height Image height, in inches.
#'
#' @return The panel width in inches. Falls back to 70% of `width` if the measurement
#' fails for any reason.
#'
#' @keywords internal
chi_plot_panel_width_in <- function(plot, width, height) {
  fallback <- width * 0.7
  opened <- tryCatch({grDevices::pdf(NULL, width = width, height = height); TRUE},
                     error = function(e) FALSE)
  if (!opened) return(fallback)
  on.exit(grDevices::dev.off(), add = TRUE)

  # suppressWarnings(): this build is only a measurement, so it should not repeat the
  # warnings that the real render will provide anyway
  measured <- tryCatch(suppressWarnings({
    gt <- ggplot2::ggplotGrob(plot)
    width - sum(grid::convertWidth(gt$widths, "in", valueOnly = TRUE))
  }), error = function(e) NA_real_)

  if (!is.finite(measured) || measured <= 0) fallback else measured
}

# chi_plot_non_panel_height_in() ----
#' Height not used by the bars
#'
#' @description
#' Inches of the image height used by everything that is not the bars, i.e., the plot
#' margins and whichever of the title, subtitle, and caption are drawn (at however many
#' lines they wrap to).
#'
#' @details
#' Measured the same way as [chi_plot_panel_width_in()]: the panel is the one row
#' measured in 'null' units, which converts to zero inches, so the sum of the absolute
#' rows is everything else. The height of the measuring device does not matter.
#'
#' @param plot A ggplot object.
#' @param width Image width, in inches. It matters because text wraps based on it.
#'
#' @return The non-panel height in inches. Falls back to 1 inch if the measurement
#' fails for any reason.
#'
#' @keywords internal
chi_plot_non_panel_height_in <- function(plot, width) {
  fallback <- 1
  opened <- tryCatch({grDevices::pdf(NULL, width = width, height = 6); TRUE},
                     error = function(e) FALSE)
  if (!opened) return(fallback)
  on.exit(grDevices::dev.off(), add = TRUE)

  measured <- tryCatch(suppressWarnings({
    gt <- ggplot2::ggplotGrob(plot)
    sum(grid::convertHeight(gt$heights, "in", valueOnly = TRUE))
  }), error = function(e) NA_real_)

  if (!is.finite(measured) || measured <= 0) fallback else measured
}
