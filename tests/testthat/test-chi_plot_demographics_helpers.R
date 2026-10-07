# Tests for the internal helpers of chi_plot_demographics(). None of these need a
# database connection.

# grid needs a graphics device to measure text; use a null one so no Rplots.pdf is created
with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

# chi_plot_read_colors() ----
test_that("chi_plot_read_colors() returns a named vector of valid hex colors", {
  colors <- chi_plot_read_colors()

  expect_type(colors, "character")
  expect_false(is.null(names(colors)))
  expect_false(anyDuplicated(names(colors)) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  expect_equal(unname(colors["King County"]), "#79706E")
})

# chi_plot_is_catchall() ----
test_that("chi_plot_is_catchall() flags catch-all categories only", {
  expect_equal(
    chi_plot_is_catchall(c("Other", "Other race", "Another race", "Unknown", "Multiple", "multiple race",
                           "White", "Otherwise", "Asian")),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

# chi_plot_band_rank() ----
test_that("chi_plot_band_rank() orders age bands by value, not alphabetically", {
  x <- c("61+", "<18", "41-60", "18-40")
  expect_equal(x[order(chi_plot_band_rank(x))], c("<18", "18-40", "41-60", "61+"))

  x <- c("10-14", "5-9", "15+", "<5")
  expect_equal(x[order(chi_plot_band_rank(x))], c("<5", "5-9", "10-14", "15+"))
})

test_that("chi_plot_band_rank() puts an open low band before a band starting at the same number", {
  r <- chi_plot_band_rank(c("18-25", "<18"))
  expect_lt(r[2], r[1])
  r <- chi_plot_band_rank(c("10-19.9%", "<10%", "20-29.9%", "30%+"))
  expect_equal(order(r), c(2, 1, 3, 4))
})

test_that("chi_plot_band_rank() orders dollar bands, including thousands separators", {
  x <- c(">$100,000", "$50,000-100,000", "<$50,000")
  expect_equal(x[order(chi_plot_band_rank(x))], c("<$50,000", "$50,000-100,000", ">$100,000"))

  x <- c("$200,000+", "<$15,000", "$100,000-$149,999", "$25,000-$34,999", "$15,000-$24,999")
  expect_equal(x[order(chi_plot_band_rank(x))],
               c("<$15,000", "$15,000-$24,999", "$25,000-$34,999", "$100,000-$149,999", "$200,000+"))

  # different magnitudes would be misordered if '$1,000,000' were read as 1
  x <- c("$1,000,000+", "$950-$999", "$1,000-$9,999", "<$950", "$100,000-$999,999", "$10,000-$99,999")
  expect_equal(x[order(chi_plot_band_rank(x))],
               c("<$950", "$950-$999", "$1,000-$9,999", "$10,000-$99,999", "$100,000-$999,999", "$1,000,000+"))

  # decimals
  x <- c("$2,000+", "<$1,000.50", "$1,000.50-$1,999.99")
  expect_equal(x[order(chi_plot_band_rank(x))], c("<$1,000.50", "$1,000.50-$1,999.99", "$2,000+"))
})

test_that("chi_plot_band_rank() ranks catch-alls after every band", {
  x <- c("Other", "$100,000 or more", "<$50,000", "Unknown", "$50,000-$99,999")
  expect_equal(x[order(chi_plot_band_rank(x))],
               c("<$50,000", "$50,000-$99,999", "$100,000 or more", "Other", "Unknown"))
})

test_that("chi_plot_band_rank() gives all zeros when the values are not bands", {
  expect_equal(chi_plot_band_rank(c("White", "Asian", "Black")), c(0, 0, 0))
  # one non-band value (that is not a catch-all) means the set is not bands
  expect_equal(chi_plot_band_rank(c("18-40", "<18", "Seattle")), c(0, 0, 0))
  expect_equal(chi_plot_band_rank(character(0)), integer(0))
})

# chi_plot_bar_labels() ----
test_that("chi_plot_bar_labels() formats by result_type", {
  expect_equal(
    chi_plot_bar_labels(result = c(0.175, 0.05, 123456.4, 1234.5, 17),
                        result_type = c("proportion", "proportion", "dollars", "count", "rate"),
                        suppression = rep(NA_character_, 5),
                        caution = rep(NA_character_, 5)),
    c("17.5%", "5.0%", "$123,456", "1,234.5", "17.0"))
})

test_that("chi_plot_bar_labels() appends suppression and caution symbols", {
  expect_equal(
    chi_plot_bar_labels(result = c(0.1, 0.2, 0.3, 0.4),
                        result_type = "proportion",
                        suppression = c("^", NA, "", NA),
                        caution = c(NA, "!", "", "!")),
    c("10.0%^", "20.0%!", "30.0%", "40.0%!"))
})

test_that("chi_plot_bar_labels() leaves rows with no result unlabeled", {
  expect_equal(
    chi_plot_bar_labels(result = c(NA, 0.5), result_type = c("proportion", "proportion"),
                        suppression = c("^", NA), caution = c(NA, NA)),
    c(NA, "50.0%"))
})

test_that("chi_plot_bar_labels() treats anything but proportion and dollars as a plain number", {
  expect_equal(
    chi_plot_bar_labels(result = c(5, 5), result_type = c("count", NA),
                        suppression = c(NA, NA), caution = c(NA, NA)),
    c("5.0", "5.0"))
})

# chi_plot_latest_year() ----
test_that("chi_plot_latest_year() keeps the most recent ending year", {
  dt <- data.table::data.table(year = c("2020", "2021", "2022", "2022"), x = 1:4)
  out <- chi_plot_latest_year(dt)
  expect_equal(out$x, 3:4)
  expect_equal(nrow(dt), 4) # input is not modified
})

test_that("chi_plot_latest_year() compares multi-year ranges on the ending year", {
  dt <- data.table::data.table(year = c("2018-2022", "2023", "2019-2021"), x = 1:3)
  expect_equal(chi_plot_latest_year(dt)$x, 2)
})

test_that("chi_plot_latest_year() prefers the widest range when years end the same year", {
  dt <- data.table::data.table(year = c("2026", "2022-2026", "2024-2026"), x = 1:3)
  expect_equal(chi_plot_latest_year(dt)$x, 2)

  dt <- data.table::data.table(year = c("2022-2024", "2022-2026"), x = 1:2)
  expect_equal(chi_plot_latest_year(dt)$x, 2)
})

test_that("chi_plot_latest_year() handles a single year and zero rows", {
  dt <- data.table::data.table(year = c("2023", "2023"), x = 1:2)
  expect_equal(chi_plot_latest_year(dt)$x, 1:2)

  expect_equal(nrow(chi_plot_latest_year(dt[0])), 0)
})

# chi_plot_resolve_race() ----
race_dt <- function() {
  data.table::data.table(
    indicator_key = c("both", "both", "both", "only3", "only3", "only4", "only4"),
    cat1_varname  = c("chi_geo_kc", "race3", "race4", "chi_geo_kc", "race3", "chi_geo_kc", "race4"),
    cat1_group    = c("King County", "Black", "Black", "King County", "Black", "King County", "Black"))
}

test_that("chi_plot_resolve_race() keeps race4 and drops race3 when cat1_varname is NULL", {
  out <- chi_plot_resolve_race(race_dt(), NULL)

  expect_equal(out$substituted, character(0))
  expect_equal(out$data[indicator_key == "both"]$cat1_varname, c("chi_geo_kc", "race4"))
  expect_equal(out$data[indicator_key == "only3"]$cat1_varname, c("chi_geo_kc", "race3")) # only has race3
  expect_equal(out$data[indicator_key == "only4"]$cat1_varname, c("chi_geo_kc", "race4"))
})

test_that("chi_plot_resolve_race() substitutes race3 for a requested race4 only when race4 is missing", {
  out <- chi_plot_resolve_race(race_dt(), c("chi_geo_kc", "race4"))

  expect_equal(out$substituted, "only3")
  expect_equal(out$data[indicator_key == "only3"]$cat1_varname, c("chi_geo_kc", "race4"))
  # an indicator that has both is left alone, and filtering on race4 later drops its race3
  expect_equal(out$data[indicator_key == "both"]$cat1_varname, c("chi_geo_kc", "race3", "race4"))
})

test_that("chi_plot_resolve_race() never swaps when race3 is requested", {
  out <- chi_plot_resolve_race(race_dt(), c("chi_geo_kc", "race3"))

  expect_equal(out$substituted, character(0))
  expect_equal(out$data, race_dt())
})

test_that("chi_plot_resolve_race() does not modify its input", {
  dt <- race_dt()
  chi_plot_resolve_race(dt, c("chi_geo_kc", "race4"))
  expect_equal(dt, race_dt())
})

# chi_plot_order_groups() ----
order_dt <- function() {
  data.table::data.table(
    cat1_varname = c("chi_geo_region", "race4", "chi_geo_kc", "race4", "race4", "age6", "age6", "age6"),
    cat1         = c("Regions", "Race", "King County", "Race", "Race", "Age", "Age", "Age"),
    cat1_group   = c("South", "White", "King County", "Other", "Asian", "10-14", "5-9", "<5"))
}

test_that("chi_plot_order_groups() puts King County first and follows the requested order", {
  out <- chi_plot_order_groups(order_dt(), c("age6", "race4", "chi_geo_region", "chi_geo_kc"))

  expect_equal(out$cat1_group[1], "King County")
  expect_equal(out$cat1_varname, c("chi_geo_kc", "age6", "age6", "age6", "race4", "race4", "race4", "chi_geo_region"))
})

test_that("chi_plot_order_groups() sorts bands by value and catch-alls last within a cat1_varname", {
  out <- chi_plot_order_groups(order_dt(), c("age6", "race4", "chi_geo_region", "chi_geo_kc"))

  expect_equal(out[cat1_varname == "age6"]$cat1_group, c("<5", "5-9", "10-14"))
  expect_equal(out[cat1_varname == "race4"]$cat1_group, c("Asian", "White", "Other"))
})

test_that("chi_plot_order_groups() falls back to alphabetical cat1 when cat1_varname is NULL", {
  out <- chi_plot_order_groups(order_dt(), NULL)

  expect_equal(out$cat1_group[1], "King County")
  expect_equal(unique(out$cat1)[-1], c("Age", "Race", "Regions")) # alphabetical
})

test_that("chi_plot_order_groups() treats the older 'kingco' spelling as King County", {
  dt <- data.table::data.table(
    cat1_varname = c("ccreg", "kingco"),
    cat1 = c("Regions", "King County"),
    cat1_group = c("South", "King County"))
  expect_equal(chi_plot_order_groups(dt, c("kingco", "ccreg"))$cat1_group, c("King County", "South"))
  expect_equal(chi_plot_order_groups(dt, c("ccreg", "kingco"))$cat1_group, c("King County", "South"))
})

test_that("chi_plot_order_groups() does not modify its input", {
  dt <- order_dt()
  chi_plot_order_groups(dt, NULL)
  expect_equal(dt, order_dt())
})

# chi_plot_text_width_in() ----
test_that("chi_plot_text_width_in() measures text and grows with length and size", {
  w <- with_null_device(chi_plot_text_width_in(c("1", "1000000"), size = 4))
  expect_true(all(w > 0))
  expect_gt(w[2], w[1])

  small <- with_null_device(chi_plot_text_width_in("17.5%", size = 3))
  big <- with_null_device(chi_plot_text_width_in("17.5%", size = 6))
  expect_gt(big, small)
})

test_that("chi_plot_text_width_in() returns NA for NA", {
  expect_equal(with_null_device(chi_plot_text_width_in(NA_character_, size = 4)), NA_real_)
})

# chi_plot_wrap_text() ----
test_that("chi_plot_wrap_text() leaves text alone when it already fits", {
  out <- with_null_device(chi_plot_wrap_text("A short title", max_in = 10, fontsize = 12))
  expect_equal(out, "A short title")
})

test_that("chi_plot_wrap_text() breaks long text into lines that each fit", {
  txt <- paste(rep("Adults without a primary care provider", 3), collapse = " ")
  out <- with_null_device(chi_plot_wrap_text(txt, max_in = 3, fontsize = 12))
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]

  expect_gt(length(lines), 1)
  expect_equal(paste(lines, collapse = " "), txt) # nothing lost or reordered
  widths <- with_null_device(vapply(lines, function(s) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontsize = 12))), "in", valueOnly = TRUE)
  }, numeric(1)))
  expect_true(all(widths <= 3))
})

test_that("chi_plot_wrap_text() keeps existing line breaks", {
  out <- with_null_device(chi_plot_wrap_text("first line\nsecond line", max_in = 10, fontsize = 12))
  expect_equal(out, "first line\nsecond line")
})

test_that("chi_plot_wrap_text() leaves a word that is too wide on its own line", {
  out <- with_null_device(chi_plot_wrap_text("a Supercalifragilisticexpialidocious b", max_in = 0.3, fontsize = 12))
  expect_equal(out, "a\nSupercalifragilisticexpialidocious\nb")
})

test_that("chi_plot_wrap_text() measures bold text wider than plain text", {
  txt <- "Adults without a primary care provider in the past twelve months"
  plain <- with_null_device(chi_plot_wrap_text(txt, max_in = 3, fontsize = 12, fontface = "plain"))
  bold <- with_null_device(chi_plot_wrap_text(txt, max_in = 3, fontsize = 12, fontface = "bold"))
  expect_gte(lengths(strsplit(bold, "\n", fixed = TRUE)), lengths(strsplit(plain, "\n", fixed = TRUE)))
})

# chi_plot_panel_width_in() and chi_plot_non_panel_height_in() ----
test_that("chi_plot_panel_width_in() and chi_plot_non_panel_height_in() measure a plot", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(g = factor(c("a", "b", "c")), v = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5, "pt"))

  # width: the panel is narrower than the image, and narrower still with longer axis labels
  w <- chi_plot_panel_width_in(p, width = 6, height = 4)
  expect_gt(w, 0)
  expect_lt(w, 6)

  p_long <- ggplot2::ggplot(transform(d, g = factor(c("a very long label", "b", "c"))),
                            ggplot2::aes(g, v)) +
    ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5, "pt"))
  expect_lt(chi_plot_panel_width_in(p_long, width = 6, height = 4), w)

  # height: a title and a two line caption need more room than a bare plot
  bare <- chi_plot_non_panel_height_in(p, width = 6)
  with_title <- chi_plot_non_panel_height_in(p + ggplot2::labs(title = "A title"), width = 6)
  with_caption <- chi_plot_non_panel_height_in(
    p + ggplot2::labs(title = "A title", caption = "line 1\nline 2"), width = 6)
  expect_gt(bare, 0)
  expect_gt(with_title, bare)
  expect_gt(with_caption, with_title)
})

test_that("chi_plot_non_panel_height_in() does not depend on how many bars there are", {
  skip_if_not_installed("ggplot2")
  make <- function(n) {
    d <- data.frame(g = factor(letters[seq_len(n)], levels = letters[seq_len(n)]), v = seq_len(n))
    ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::theme_minimal() + ggplot2::labs(title = "A title")
  }
  expect_equal(chi_plot_non_panel_height_in(make(3), width = 6),
               chi_plot_non_panel_height_in(make(15), width = 6))
})
