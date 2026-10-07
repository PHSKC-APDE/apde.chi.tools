# Tests for chi_plot_demographics(). The helpers it uses are tested in
# test-chi_plot_demographics_helpers.R.

# argument validation ----
# These stop before any database connection is made, so they run anywhere.
  test_that("chi_plot_demographics() rejects bad arguments", {
    out_dir <- tempfile("chi_plot_test_")

    expect_error(chi_plot_demographics(), "table_name")
    expect_error(chi_plot_demographics(table_name = c("brfss", "acs")), "table_name")
    expect_error(chi_plot_demographics(table_name = NA_character_), "table_name")
    expect_error(chi_plot_demographics("brfss", indicator_key = 1), "indicator_key")
    expect_error(chi_plot_demographics("brfss", cat1_varname = 1), "cat1_varname")
    expect_error(chi_plot_demographics("brfss", output_dir = c("a", "b")), "output_dir")
    expect_error(chi_plot_demographics("brfss", image_type = "gif"))
    expect_error(chi_plot_demographics("brfss", width = "wide"), "width")
    expect_error(chi_plot_demographics("brfss", width = c(5, 6)), "width")
    expect_error(chi_plot_demographics("brfss", height = 0), "height")
    expect_error(chi_plot_demographics("brfss", height = "tall"), "height")
    expect_error(chi_plot_demographics("brfss", dpi = -1), "dpi")
    expect_error(chi_plot_demographics("brfss", prod = "yes"), "prod")
    expect_error(chi_plot_demographics("brfss", chi = NA), "chi")
    expect_error(chi_plot_demographics("brfss", show_title = NULL), "show_title")
    expect_error(chi_plot_demographics("brfss", show_caption = 1), "show_caption")
    expect_error(chi_plot_demographics("brfss", trim_margin = "TRUE"), "trim_margin")
    expect_error(chi_plot_demographics("brfss", title = c("a", "b")), "title")
    expect_error(chi_plot_demographics("brfss", title = 5), "title")
    expect_error(chi_plot_demographics("brfss", subtitle = NA_character_), "subtitle")

    expect_false(dir.exists(out_dir)) # nothing is created when validation fails
  })

  test_that("chi_plot_demographics() requires a width of at least 4 inches", {
    expect_error(chi_plot_demographics("brfss", width = 3.9), "at least 4")
    expect_error(chi_plot_demographics("brfss", width = 0), "at least 4")
  })

  test_that("chi_plot_demographics() stops when both race3 and race4 are requested", {
    expect_error(
      chi_plot_demographics("brfss", cat1_varname = c("chi_geo_kc", "race3", "race4")),
      "both 'race3' and 'race4'")
  })

# database tests ----
# These need access to PHExtractStore, so they are skipped on machines without it.
  chi_db_available <- tryCatch({
    cxn <- apde.etl::create_db_connection(server = "phextractstore", prod = TRUE)
    DBI::dbDisconnect(cxn)
    TRUE
  }, error = function(e) FALSE)

  skip_if_no_chi_db <- function() {
    testthat::skip_if_not(chi_db_available, "Cannot connect to PHExtractStore")
  }

  test_that("chi_plot_demographics() writes one png per indicator and returns the path", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    files <- chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                                   output_dir = out_dir, dpi = 100)

    expect_length(files, 1)
    expect_true(file.exists(files))
    # normalizePath() so that slash direction and short Windows folder names do not matter
    expect_equal(normalizePath(dirname(files), winslash = "/"), normalizePath(out_dir, winslash = "/"))
    expect_equal(basename(files), paste0("brfss_chi_no_pcp_", Sys.Date(), ".png"))
  })

  test_that("chi_plot_demographics() accepts a table name with or without '_results' and can write jpg", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    files <- chi_plot_demographics(table_name = "brfss_results", indicator_key = "chi_no_pcp",
                                   image_type = "jpg", output_dir = out_dir, dpi = 100)

    expect_equal(basename(files), paste0("brfss_chi_no_pcp_", Sys.Date(), ".jpg"))
    expect_true(file.exists(files))
  })

  test_that("chi_plot_demographics() honors a user supplied width, height and dpi", {
    skip_if_no_chi_db()
    skip_if_not_installed("png")
    out_dir <- withr::local_tempdir()

    files <- chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                                   output_dir = out_dir, width = 5, height = 4, dpi = 100)

    dims <- dim(png::readPNG(files)) # rows (height), columns (width), channels
    expect_equal(dims[1], 400)
    expect_equal(dims[2], 500)
  })

  test_that("chi_plot_demographics() makes the default height taller when there are more bars", {
    skip_if_no_chi_db()
    skip_if_not_installed("png")
    out_dir <- withr::local_tempdir()

    few <- chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                                 cat1_varname = c("chi_geo_kc", "chi_geo_region"),
                                 output_dir = file.path(out_dir, "few"), width = 5, dpi = 100)
    many <- chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                                  cat1_varname = c("chi_geo_kc", "race4", "chi_geo_region"),
                                  output_dir = file.path(out_dir, "many"), width = 5, dpi = 100)

    expect_gt(dim(png::readPNG(many))[1], dim(png::readPNG(few))[1])
  })

  test_that("chi_plot_demographics() runs with optional text turned off or customized", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    expect_no_error(chi_plot_demographics(
      table_name = "brfss", indicator_key = "chi_no_pcp", output_dir = file.path(out_dir, "none"),
      show_title = FALSE, show_caption = FALSE, trim_margin = FALSE, dpi = 100))

    expect_no_error(chi_plot_demographics(
      table_name = "brfss", indicator_key = "chi_no_pcp", output_dir = file.path(out_dir, "custom"),
      title = "A custom title that is long enough that it will have to wrap onto a second line",
      subtitle = "A subtitle", dpi = 100))
  })

  test_that("chi_plot_demographics() runs with cat1_varname = NULL and warns about it", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    # includes tables where an indicator has both race3 and race4, which must not draw duplicate bars
    expect_warning(
      files <- chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                                     cat1_varname = NULL, output_dir = out_dir, dpi = 100),
      "is NULL")
    expect_true(file.exists(files))
  })

  test_that("chi_plot_demographics() gives informative errors for unavailable requests", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    expect_error(
      chi_plot_demographics(table_name = "not_a_table", output_dir = out_dir),
      "must be one of the tables")
    expect_error(
      chi_plot_demographics(table_name = "brfss", indicator_key = "not_an_indicator", output_dir = out_dir),
      "not_an_indicator")
    expect_error(
      chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                            cat1_varname = c("chi_geo_kc", "not_a_varname"), output_dir = out_dir),
      "not_a_varname")
  })

  test_that("chi_plot_demographics() stops when a supplied height leaves no room for the bars", {
    skip_if_no_chi_db()
    out_dir <- withr::local_tempdir()

    expect_error(
      chi_plot_demographics(table_name = "brfss", indicator_key = "chi_no_pcp",
                            output_dir = out_dir, height = 0.5),
      "too small")
  })
