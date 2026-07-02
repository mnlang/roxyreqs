# `roxyreqs`

Structured metadata for R test cases and function documentation, in the
style of roxygen2.

R documents functions well via roxygen2, but has no equivalent for test
cases. Who wrote a test? Who reviewed it? What requirement does it verify?
`roxyreqs` extends roxygen2 with `@meta` tags above `test_that()` blocks and
in function documentation, a JUnit reporter that exports the metadata, and
validation functions that ensure required tags are present.

This is especially useful in regulated industries such as pharma and
finance, where traceability between requirements and tests is mandatory.

## Installation

```r
# CRAN
install.packages("roxyreqs")

# development version
# install.packages("pak")
pak::pak("mnlang/roxyreqs")
```

## Quick start

Add `@meta` tags to a `test_that()` block:

```r
#' @meta author Alice
#' @meta reviewer Bob
#' @meta review_date 2025-01-15
#' @meta description Validates input parsing for edge cases.
test_that("parse_input handles edge cases", {
  expect_equal(parse_input("a,b"), c("a", "b"))
})
```

Run the suite with the metadata-aware JUnit reporter to emit the tags as
XML properties, then validate that every test carries the required tags:

```r
testthat::test_local(".", reporter = roxyreqs::JunitReporterMeta)
roxyreqs::check_meta_test()
```

## Function documentation metadata

`@meta` tags also work in function documentation, adding a metadata section
to the generated help file. Enable the macro in your `DESCRIPTION`:

```
RdMacros: roxyreqs
Imports:
    roxyreqs
```

```r
#' Add two numbers.
#'
#' @param x number
#' @param y number
#' @return number
#'
#' @meta id URS01.FS001
#' @meta compliance_risk low
#' @export
add_number <- function(x, y) x + y
```

After `devtools::document()`, the generated `.Rd` contains a `\meta{}`
block with the parsed tags.

## Validation

`@meta` tags are free-form (`@meta key value`). Require and validate a
specific set to fail a CI build when metadata is missing or duplicated:

| Function | Purpose |
| --- | --- |
| `check_meta()` | Run all checks and return a combined result. |
| `check_meta_test()` | Required tags present on each `test_that()` block. |
| `check_meta_undocumented_test()` | Every `test_that()` block has an `@meta` block. |
| `check_meta_rd()` | Required tags present in `.Rd` help files. |
| `check_meta_unique_ids()` | The `Id` tag is unique across help files. |

```r
roxyreqs::check_meta(print_error = TRUE)
```

Helpers `parse_rd_meta()` (read `@meta` tags into a data frame) and
`move_macro_first()` (macro ordering) support downstream reporting.

## Contributing

See `CONTRIBUTING.md`. Feedback and contributions are welcome.

## License

GPL-3.
