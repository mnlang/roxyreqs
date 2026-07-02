#' Check Metadata Documentation
#'
#' @description
#' A suite of functions to validate metadata documentation in Rd and test files.
#'
#' Meta tags follow the format `@meta key value` where `key` can be any
#' user-defined identifier. Built-in tags for function documentation include
#' `id`, `compliance_risk`, and `implementation_complexity`. For test files,
#' the default required tags are `author`, `reviewer`, `review_date`, and
#' `description`. Custom tags can be added and validated by passing them to
#' the `check_rd_tag_exist` or `check_test_tag_exist` parameters.
#'
#' This function combines multiple checks to ensure metadata in your Rd and test files meets
#' required standards for completeness, uniqueness, and documentation quality. The following
#' checks are included:
#'
#' - [check_meta_rd()]: Checks for missing metadata tags in Rd files.
#' - [check_meta_unique_ids()]: Ensures the 'Id' metadata tag in Rd files is unique.
#' - [check_meta_undocumented_test()]: Validates that all `test_that` test cases
#'   are properly documented.
#' - [check_meta_test()]: Verifies that test case documentation contains all required
#'   metadata tags.
#'
#' @param files_rd character; A vector of paths to `.Rd` files to be checked.
#' @param files_test character; A vector of paths to test files to be checked.
#' @param check_rd_tag_exist character; A vector of metadata tag names in Rd files to check
#'   for completeness.
#' @param check_test_tag_exist character; A vector of metadata tag names in test files to check
#'   for completeness.
#' @param check_rd_tag_unique character; A single metadata tag in Rd files to check for uniqueness.
#'   Default is `"Id"`.
#' @param exclude_package_rd logical; Whether to exclude package-level Rd files (i.e., files ending
#'   with `"-package.Rd"`). Default is `TRUE`.
#' @param concise logical; Whether to print only failures instead of detailed output. Default is
#'   `TRUE`.
#' @param print_error logical; Whether to print error messages. This is useful for CI/CD pipelines
#'   to produce errors when issues are found. Default is `FALSE`.
#' @return
#' - `check_meta()`: A named list with results from each sub-check
#'   (invisibly).
#'
#' @meta id URS01.FS001
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @rdname checkmeta
#' @export
check_meta <- function(files_rd = list.files("man", pattern = "*.Rd", full.names = TRUE),
                       files_test = list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE),
                       check_rd_tag_exist = c("Id", "Compliance Risk", "Implementation Complexity", "Test Scope"),
                       check_test_tag_exist = c("author", "reviewer", "review_date", "description"),
                       check_rd_tag_unique = "Id",
                       exclude_package_rd = TRUE,
                       concise = TRUE,
                       print_error = FALSE) {
  # Initialize storage for error and warning events
  error_events <- list()
  rval <- list()

  # Helper function to handle error and warning collection
  collect_errors <- function(expr, label) {
    tryCatch(
      expr,
      warning = function(w) {
        error_events[[length(error_events) + 1]] <<- list(type = "warning", label = label, value = w$message)
      },
      error = function(e) {
        error_events[[length(error_events) + 1]] <<- list(type = "error", label = label, value = e$message)
      }
    )
  }

  # Run all checks and collect errors/warnings
  collect_errors(
    rval[["meta_rd"]] <- check_meta_rd(
      files_rd = files_rd,
      check_rd_tag_exist = check_rd_tag_exist,
      exclude_package_rd = exclude_package_rd,
      print_error = print_error,
      concise = concise
    ),
    label = "Missing meta tags in Rd files"
  )

  collect_errors(
    rval[["meta_unique_ids"]] <- check_meta_unique_ids(
      files_rd = files_rd,
      check_rd_tag_unique = check_rd_tag_unique,
      exclude_package_rd = exclude_package_rd,
      print_error = print_error,
      concise = concise
    ),
    label = "Duplicated meta tags in Rd files"
  )

  collect_errors(
    rval[["meta_undocumented_test"]] <- check_meta_undocumented_test(
      files_test = files_test,
      print_error = print_error,
      concise = concise
    ),
    label = "Undocumented test_that blocks in test files"
  )

  collect_errors(
    rval[["meta_test"]] <- check_meta_test(
      files_test = files_test,
      check_test_tag_exist = check_test_tag_exist,
      print_error = print_error,
      concise = concise
    ),
    label = "Missing meta tags in test files"
  )

  # Combine all collected errors and warnings
  if (length(error_events) > 0) {
    error_df <- do.call(rbind.data.frame, lapply(error_events, as.data.frame))
    colnames(error_df) <- c("type", "label", "message")

    # Separate errors and warnings for reporting
    errors <- error_df[error_df$type == "error", ]
    warnings <- error_df[error_df$type == "warning", ]

    # Print warnings if any
    if (nrow(warnings) > 0) {
      cli::cli_h2("Warnings")
      for (i in seq_len(nrow(warnings))) {
        cli::cli_alert_warning("{.strong [From: {warnings$label[i]}]} {.field {warnings$message[i]}}")
      }
    }

    # Stop if there are errors
    if (nrow(errors) > 0) {
      cli::cli_h2("Errors")
      for (i in seq_len(nrow(errors))) {
        cli::cli_alert_danger("{.strong [From: {errors$label[i]}]} {.field {errors$message[i]}}")
      }
      stop("One or more errors occurred. See details above.")
    }
  }

  # If no errors or warnings, print success message
  if (all(sapply(rval[c("meta_undocumented_test", "meta_test")], function(x) length(x) == 0L)) &&
    all(sapply(rval[c("meta_test", "meta_unique_ids")], function(x) NROW(x) == 0L))) {
    cli::cli_h2("Summary running all checks")
    cli::cli_alert_success("All meta checks passed successfully.")
  } else {
    cli::cli_h2("Summary running all checks")
    cli::cli_alert_danger("One or more meta checks failed. See details above.")
  }

  # Return invisible
  invisible(rval)
}

#' - `check_meta_test()`: A named list of data.frames with missing meta
#'   tags per file (invisibly). Empty if all tags are present.
#'
#' @meta id URS01.FS002
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @rdname checkmeta
#' @export
check_meta_test <- function(files_test = list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE),
                            check_test_tag_exist = c("author", "reviewer", "review_date", "description"),
                            concise = FALSE,
                            print_error = FALSE) {
  # Parse Roxygen tags from test files
  roc <- do.call(c, lapply(files_test, roxygen2::parse_file, env = NULL))

  # Extract metadata from Roxygen blocks
  roc <- lapply(roc, function(x) {
    res_meta <- list()
    for (y in x$tags) {
      if (y$tag == "meta") {
        res_meta[[y$val$type]] <- y$val$details
      }
    }
    as.data.frame(c(file = x$file, line = x$line, res_meta),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  all_cols <- unique(unlist(lapply(roc, names)))
  roc <- lapply(roc, function(x) {
    missing <- setdiff(all_cols, names(x))
    x[missing] <- NA
    x
  })
  roc <- do.call(rbind, roc)

  # Ensure all required columns are present
  required_cols <- c("file", "line", check_test_tag_exist)
  new_col <- setdiff(required_cols, names(roc))
  roc[new_col] <- NA

  # Initialize counters
  passes <- 0
  failures <- 0
  missing_meta_tags <- list()

  # Print header
  cli::cli_h1("Checking test files for missing meta tags")

  # Create status
  status <- cli::cli_status("{.file Checking files...}")

  # Iterate over each file and check for missing meta tags
  for (file in unique(roc$file)) {
    # Update status
    cli::cli_status_update(status, "{.file Checking file: {file}}")

    file_meta <- roc[roc$file == file, ]
    file_missing <- file_meta[!stats::complete.cases(file_meta), ]

    if (nrow(file_missing) > 0) {
      # Record missing meta tags
      failures <- failures + 1
      missing_meta_tags[[file]] <- file_missing
      for (i_row in seq_len(NROW(file_missing))) {
        cli::cli_alert_danger("File '{.file {file}}' is missing in line {.field {file_missing[i_row, 'line']}} the following meta tags: {.field {paste(names(file_missing)[is.na(file_missing[i_row, ])] , collapse = ', ')}}")
      }
    } else {
      passes <- passes + 1
      if (!isTRUE(concise)) cli::cli_alert_success("File '{.file {file}}' has all Roxygen blocks properly documented.")
    }
  }

  # Clear status
  cli::cli_status_clear(status)

  # Print summary
  if (!isTRUE(concise)) {
    cli::cli_h2("Summary")
    cli::cli_alert_success("Passed: {passes}")
    cli::cli_alert_danger("Failed: {failures}")
  }

  # Handle errors in CI/CD mode
  if (failures > 0) {
    if (isTRUE(print_error)) {
      stop(sprintf("Error: %d test files contain Roxygen blocks with missing meta tags. See details above.", failures))
    }
  } else {
    cli::cli_alert_success("All Roxygen blocks have the required meta tags.")
  }

  # Return invisible
  invisible(missing_meta_tags)
}


#' - `check_meta_undocumented_test()`: A named list of integer vectors
#'   with line numbers of undocumented `test_that` blocks per file
#'   (invisibly). Empty if all blocks are documented.
#'
#' @meta id URS01.FS003
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @rdname checkmeta
#' @export
check_meta_undocumented_test <- function(files_test = list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE),
                                         concise = FALSE,
                                         print_error = FALSE) {
  # Initialize counters
  passes <- 0
  failures <- 0
  undocumented_test <- list()

  # Print header
  cli::cli_h1("Checking test files for undocumented `test_that` blocks")

  # Create status
  status <- cli::cli_status("{.file Checking files...}")

  # Iterate through each test file
  for (file in files_test) {
    # Update status
    cli::cli_status_update(status, "{.file Checking file: {file}}")

    # Read the file's lines
    lines <- readLines(file, warn = FALSE)

    # Use R parser to find real test_that calls (avoids false positives
    # from test_that appearing inside string literals)
    exprs <- tryCatch(
      parse(file, keep.source = TRUE),
      error = function(e) NULL
    )
    if (is.null(exprs)) {
      test_lines <- integer(0)
    } else {
      srcrefs <- attr(exprs, "srcref")
      test_lines <- vapply(seq_along(exprs), function(i) {
        e <- exprs[[i]]
        if (is_test_that_call(e)) srcrefs[[i]][1] else NA_integer_
      }, integer(1))
      test_lines <- test_lines[!is.na(test_lines)]
    }

    # Check for missing roxygen comments above `test_that` blocks
    missing_docs <- c()
    for (line_num in test_lines) {
      # Check up to 3 lines above for roxygen comments
      doc_found <- FALSE
      for (i in 1:3) {
        if ((line_num - i) > 0 && grepl("^#'", lines[line_num - i])) {
          doc_found <- TRUE
          break
        }
      }
      if (!doc_found) {
        missing_docs <- c(missing_docs, line_num)
      }
    }

    # Record results for the current file
    if (length(missing_docs) > 0) {
      failures <- failures + 1
      undocumented_test[[file]] <- missing_docs
      cli::cli_alert_danger("File '{.file {file}}' has undocumented `test_that` blocks at lines: {.field {paste(missing_docs, collapse = ', ')}}")
    } else {
      passes <- passes + 1
      if (!isTRUE(concise)) cli::cli_alert_success("File '{.file {file}}' has all `test_that` blocks properly documented.")
    }
  }

  # Clear status
  cli::cli_status_clear(status)

  # Print summary
  if (!isTRUE(concise)) {
    cli::cli_h2("Summary")
    cli::cli_alert_success("Passed: {passes}")
    cli::cli_alert_danger("Failed: {failures}")
  }

  # Handle errors in CI/CD mode
  if (failures > 0) {
    if (isTRUE(print_error)) {
      stop(sprintf("Error: %d test files contain undocumented `test_that` blocks. See details above.", failures))
    }
  } else {
    cli::cli_alert_success("All test files are properly documented.")
  }

  # Return invisible
  invisible(undocumented_test)
}

#' - `check_meta_rd()`: A data.frame of Rd files with missing meta tags
#'   (invisibly). Zero rows if all tags are present.
#'
#' @meta id URS01.FS004
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @rdname checkmeta
#' @export
check_meta_rd <- function(files_rd = list.files("man", pattern = "*.Rd", full.names = TRUE),
                          check_rd_tag_exist = c("Id", "Compliance Risk", "Implementation Complexity", "Test Scope"),
                          exclude_package_rd = TRUE,
                          concise = FALSE,
                          print_error = FALSE) {
  # Filter out package-level Rd files if requested
  if (exclude_package_rd) {
    files_rd <- files_rd[!grepl("-package\\.Rd$", basename(files_rd))]
  }

  # Parse metadata
  rd <- parse_rd_meta(filepath = files_rd)

  # Ensure all required columns are present in the parsed metadata
  required_cols <- c("filepath", check_rd_tag_exist)
  new_col <- setdiff(required_cols, names(rd))
  rd[new_col] <- NA
  rd <- rd[, required_cols, drop = FALSE]

  # Initialize counters
  passes <- 0
  failures <- 0

  # Print header
  cli::cli_h1("Checking Rd files for missing meta tags")

  # Create status
  status <- cli::cli_status("{.file Checking files...}")

  # Check for missing metadata in each Rd file
  for (file in unique(rd$filepath)) {
    # Update status
    cli::cli_status_update(status, "{.file Checking file: {file}}")

    # Extract metadata for the current file
    file_metadata <- rd[rd$filepath == file, , drop = FALSE]

    # Collect missing tags for the current file
    missing_tags <- check_rd_tag_exist[is.na(file_metadata[check_rd_tag_exist])]

    if (length(missing_tags) > 0) {
      # Increment failure count
      failures <- failures + 1
      cli::cli_alert_danger("File '{.file {file}}' is missing the following meta tags: {.field {paste(missing_tags, collapse = ', ')}}")
    } else {
      # Increment pass count
      passes <- passes + 1
      if (!isTRUE(concise)) cli::cli_alert_success("File '{.file {file}}' has all required meta tags.")
    }
  }

  # Clear status
  cli::cli_status_clear(status)

  # Print summary
  if (!isTRUE(concise)) {
    cli::cli_h2("Summary")
    cli::cli_alert_success("Passed: {passes}")
    cli::cli_alert_danger("Failed: {failures}")
  }

  # Handle errors for e.g. CI/CD mode
  if (failures > 0) {
    if (isTRUE(print_error)) {
      stop(sprintf("Error: %d Rd files are missing required meta tags. See details above.", failures))
    }
  } else {
    cli::cli_alert_success("All Rd files have the required meta tags.")
  }

  # Return invisible
  invisible(rd[!stats::complete.cases(rd), ])
}

#' - `check_meta_unique_ids()`: A data.frame of Rd files with duplicated
#'   tag values (invisibly). Zero rows if all values are unique.
#'
#' @meta id URS01.FS005
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @rdname checkmeta
#' @export
check_meta_unique_ids <- function(files_rd = list.files("man", pattern = "*.Rd", full.names = TRUE),
                                  check_rd_tag_unique = "Id",
                                  exclude_package_rd = TRUE,
                                  concise = FALSE,
                                  print_error = FALSE) {
  # Filter out package-level Rd files if requested
  if (exclude_package_rd) {
    files_rd <- files_rd[!grepl("-package\\.Rd$", basename(files_rd))]
  }

  # Parse metadata
  rd <- parse_rd_meta(filepath = files_rd)

  # Ensure the required column (e.g., "Id") exists
  new_col <- setdiff(check_rd_tag_unique, names(rd))
  rd[new_col] <- NA

  # Filter out rows where the check_rd_tag_unique column is missing
  rd <- rd[!is.na(rd[[check_rd_tag_unique]]), ]

  # Identify duplicates in the specified column
  duplicates <- rd[duplicated(rd[[check_rd_tag_unique]]), ]

  # Initialize counters
  passes <- 0
  failures <- 0

  # Print header
  cli::cli_h1("Checking Rd files for unique meta tag '{check_rd_tag_unique}'")

  # Create status
  status <- cli::cli_status("{.file Checking files...}")

  # Process duplicates dynamically
  for (file in unique(rd$filepath)) {
    # Update status
    cli::cli_status_update(status, "{.file Checking file: {file}}")

    file_duplicates <- duplicates[duplicates$filepath == file, ]

    if (nrow(file_duplicates) > 0) {
      # Increment failure count
      failures <- failures + 1
      cli::cli_alert_danger("File '{.file {file}}' contains duplicated '{.field {check_rd_tag_unique}}'")
    } else {
      # Increment pass count
      passes <- passes + 1
      if (!isTRUE(concise)) cli::cli_alert_success("File '{.file {file}}' has unique '{.field {check_rd_tag_unique}}'")
    }
  }

  # Clear status
  cli::cli_status_clear(status)

  # Print summary
  if (!isTRUE(concise)) {
    cli::cli_h2("Summary")
    cli::cli_alert_success("Passed: {passes}")
    cli::cli_alert_danger("Failed: {failures}")
  }

  # Handle errors for e.g. CI/CD mode
  if (failures > 0) {
    if (isTRUE(print_error)) {
      stop(sprintf("Error: %d Rd files contain duplicated '%s'. See details above.", failures, check_rd_tag_unique))
    }
  } else {
    cli::cli_alert_success("All '{check_rd_tag_unique}' meta tags are unique across Rd files.")
  }

  # Return invisible
  invisible(duplicates)
}
