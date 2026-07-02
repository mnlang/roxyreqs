#' @meta author Moritz Lang
#' @meta reviewer Mario Annau
#' @meta review_date 2026-05-22
#' @meta description Test format.rd_section_meta correctly formats meta tags,
#'   computes Test Scope from the scopemap, and reorders tags.
test_that("FormatRdSectionMeta", {
  # Helper to create rd_section_meta objects ----
  make_section <- function(types, details) {
    roxygen2::rd_section("meta", list(type = types, details = details))
  }

  # Formats basic meta tags ----
  section <- make_section(
    c("id", "compliance_risk", "implementation_complexity"),
    c("URS01.FS001", "low", "low")
  )
  out <- format(section)
  expect_true(grepl("Id: URS01.FS001", out))
  expect_true(grepl("Compliance Risk: low", out))
  expect_true(grepl("Implementation Complexity: low", out))

  # Auto-computes Test Scope from scopemap ----
  # low + low -> low
  expect_true(grepl("Test Scope: low", out))

  # low + high -> high
  section2 <- make_section(
    c("id", "compliance_risk", "implementation_complexity"),
    c("FS002", "low", "high")
  )
  out2 <- format(section2)
  expect_true(grepl("Test Scope: high", out2))

  # medium + medium -> medium
  section3 <- make_section(
    c("id", "compliance_risk", "implementation_complexity"),
    c("FS003", "medium", "medium")
  )
  out3 <- format(section3)
  expect_true(grepl("Test Scope: medium", out3))

  # high + high -> high
  section4 <- make_section(
    c("id", "compliance_risk", "implementation_complexity"),
    c("FS004", "high", "high")
  )
  out4 <- format(section4)
  expect_true(grepl("Test Scope: high", out4))

  # Tag ordering: Id comes first ----
  lines <- strsplit(out, "\n")[[1]]
  lines <- lines[lines != "" & !grepl("^\\\\meta", lines) & lines != "}"]
  expect_equal(lines[1], "Id: URS01.FS001")

  # Output wraps in \\meta{} ----
  expect_true(grepl("^\\\\meta\\{", out))
  expect_true(grepl("\\}$", out))

  # User-defined custom tags are included ----
  section_custom <- make_section(
    c("id", "compliance_risk", "implementation_complexity", "jira_ticket", "category"),
    c("FS005", "low", "low", "PROJ-123", "regression")
  )
  out_custom <- format(section_custom)
  expect_true(grepl("Jira Ticket: PROJ-123", out_custom))
  expect_true(grepl("Category: regression", out_custom))
  # Standard tags still present
  expect_true(grepl("Id: FS005", out_custom))
  expect_true(grepl("Test Scope: low", out_custom))

  # Custom tags appear after standard tags ----
  lines_custom <- strsplit(out_custom, "\n")[[1]]
  lines_custom <- lines_custom[lines_custom != "" &
    !grepl("^\\\\meta", lines_custom) & lines_custom != "}"]
  id_pos <- which(grepl("^Id:", lines_custom))
  jira_pos <- which(grepl("^Jira Ticket:", lines_custom))
  expect_true(jira_pos > id_pos)
})
