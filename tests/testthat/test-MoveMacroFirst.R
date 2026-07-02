#' @meta author Moritz Lang
#' @meta reviewer Mario Annau
#' @meta review_date 2026-05-22
#' @meta description Test move_macro_first repositions USERMACRO elements
#'   in parsed Rd objects to appear after a specified tag.
test_that("MoveMacroFirst", {
  # Helper to create a parsed Rd object ----
  make_rd_element <- function(text, tag) {
    el <- list(text)
    attr(el, "Rd_tag") <- tag
    el
  }

  # Default: move USERMACRO after \\title + offset ----
  rd <- list(
    make_rd_element("preamble", "COMMENT"),
    make_rd_element("My Title", "\\title"),
    make_rd_element("title text", "TEXT"),
    make_rd_element("some args", "\\arguments"),
    make_rd_element("description", "\\description"),
    make_rd_element("meta info", "USERMACRO")
  )
  class(rd) <- "Rd"

  result <- move_macro_first(rd)
  tags <- sapply(result, attr, "Rd_tag")
  macro_pos <- which(tags == "USERMACRO")
  title_pos <- which(tags == "\\title")
  # USERMACRO should be right after title + offset (position 3)
  expect_equal(macro_pos, title_pos + 2L)
  # Result should still be class Rd
  expect_s3_class(result, "Rd")
  # All elements preserved
  expect_equal(length(result), length(rd))

  # Custom rd_tag and offset ----
  result2 <- move_macro_first(rd, rd_tag = "\\arguments", offset = 0L)
  tags2 <- sapply(result2, attr, "Rd_tag")
  args_pos <- which(tags2 == "\\arguments")
  macro_pos2 <- which(tags2 == "USERMACRO")
  # USERMACRO should be right after \\arguments (offset 0 means +1 position)
  expect_equal(macro_pos2, args_pos + 1L)
  expect_equal(length(result2), length(rd))
})
