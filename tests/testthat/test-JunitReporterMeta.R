library(xml2)

#' @meta author Moritz Lang
#' @meta reviewer Mario Annau
#' @meta review_date 2026-05-22
#' @meta description Test JunitReporterMeta transforms roxygen2 meta tags
#'   into JUnit XML properties. Verifies meta tags are embedded as property
#'   elements in the XML output, and that tests without matching @meta blocks
#'   do not crash the reporter.
test_that("Meta tags transformed into properties", {
  test_text <- "
  #' @meta author Mario
  #' @meta reviewer Moritz
  #' @meta review_date 2024-06-10
  #' @meta description Test to evaluate that adding two integer numbers works
  #' as expected. This description can optionally be quite detailed, extending
  #' over several lines if necessary, to provide a comprehensive understanding of
  #' the test case.
  test_that(\"Adding numbers works\", {
    expect_equal(1+2, 3)
  })
  "

  test_file <- tempfile(fileext = ".R")
  writeLines(test_text, con = test_file)

  out <- capture.output(test_file(test_file, reporter = JunitReporterMeta))
  out_xml <- read_xml(paste(out, collapse = "\n"))
  meta_author <- out_xml |>
    xml_find_all("//testcase//properties/property[@name = 'author']") |>
    xml_attr("value")
  expect_equal(meta_author, "Mario")

  meta_reviewer <- out_xml |>
    xml_find_all("//testcase//properties/property[@name = 'reviewer']") |>
    xml_attr("value")
  expect_equal(meta_reviewer, "Moritz")

  meta_review_date <- out_xml |>
    xml_find_all("//testcase//properties/property[@name = 'review_date']") |>
    xml_attr("value")
  expect_equal(meta_review_date, "2024-06-10")

  meta_description <- out_xml |>
    xml_find_all("//testcase//properties/property[@name = 'description']") |>
    xml_attr("value")
  expect_equal(meta_description, "Test to evaluate that adding two integer numbers works as expected. This description can optionally be quite detailed, extending over several lines if necessary, to provide a comprehensive understanding of the test case.")

  # No matching @meta block does not crash under covr ----
  # covr (dgkf/covr@dev) instruments [[ so that list[[NA]] errors instead of
  # returning NULL. Simulate this by replacing roxy_blocks with a strict_list
  # that rejects NA subscripts, matching covr's behavior.
  strict_list <- function(x) structure(x, class = c("strict_list", "list"))
  s3_method <- function(x, i, ...) {
    if (length(i) == 1 && is.na(i)) {
      stop("attempt to select less than one element in get1index")
    }
    NextMethod()
  }
  registerS3method("[[", "strict_list", s3_method)

  mock_result <- structure(
    list(message = "ok", srcref = NULL),
    class = c("expectation_success", "expectation", "condition")
  )

  # Empty roxy_blocks: file with no roxygen blocks at all
  reporter <- JunitReporterMeta$new()
  reporter$start_reporter()
  tf <- tempfile(fileext = ".R")
  writeLines("test_that(\"no meta\", { expect_true(TRUE) })", tf)
  reporter$start_file(tf)
  reporter$roxy_blocks <- strict_list(reporter$roxy_blocks)
  reporter$start_context("ctx")
  expect_no_error(reporter$add_result("ctx", "no meta", mock_result))

  # Non-empty roxy_blocks: mixed file where only some tests have @meta
  reporter2 <- JunitReporterMeta$new()
  reporter2$start_reporter()
  tf2 <- tempfile(fileext = ".R")
  writeLines(c(
    "#' @meta author Mario",
    "test_that(\"has meta\", { expect_true(TRUE) })",
    "",
    "test_that(\"no meta\", { expect_equal(1, 1) })"
  ), tf2)
  reporter2$start_file(tf2)
  reporter2$roxy_blocks <- strict_list(reporter2$roxy_blocks)
  reporter2$start_context("ctx2")
  expect_no_error(reporter2$add_result("ctx2", "has meta", mock_result))
  expect_no_error(reporter2$add_result("ctx2", "no meta", mock_result))
})
