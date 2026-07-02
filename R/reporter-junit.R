#' Test reporter: summary of errors in JUnit XML format.
#'
#' @description
#' This reporter includes detailed results about each test and summaries,
#' written to a file (or stdout) in JUnit XML format. This can be read by
#' the Jenkins Continuous Integration System to report on a dashboard etc.
#' Requires the _xml2_ package.
#'
#' @details
#' To fit into the JUnit structure, context() becomes the `<testsuite>`
#' name as well as the base of the `<testcase> classname`. The
#' test_that() name becomes the rest of the `<testcase> classname`.
#' The deparsed expect_that() call becomes the `<testcase>` name.
#' On failure, the message goes into the `<failure>` node message
#' argument (first line only) and into its text content (full message).
#'
#' Execution time and some other details are also recorded.
#'
#' Tests without `@meta` roxygen blocks produce test cases with an
#' empty `<properties/>` node. Use [check_meta_test()] to validate
#' that all required tags are present.
#'
#' References for the JUnit XML format:
#' \url{http://llg.cubic.org/docs/junit/}
#'
#' @meta id URS01.FS007
#' @meta compliance_risk low
#' @meta implementation_complexity medium
#'
#' @export
#' @importFrom testthat JunitReporter
#' @examples
#' \dontrun{
#' testthat::test_local(path = "inst/example.pkg", reporter = JunitReporterMeta)
#' }
#' @family reporters
JunitReporterMeta <- R6::R6Class("JunitReporterMeta",
  inherit = JunitReporter,
  public = list(
    #' @field results Test results.
    results = NULL,
    #' @field timer Process time tracker.
    timer = NULL,
    #' @field doc XML document object.
    doc = NULL,
    #' @field errors Error count for current suite.
    errors = NULL,
    #' @field failures Failure count for current suite.
    failures = NULL,
    #' @field skipped Skip count for current suite.
    skipped = NULL,
    #' @field tests Test count for current suite.
    tests = NULL,
    #' @field root XML root node (`<testsuites>`).
    root = NULL,
    #' @field suite Current XML suite node (`<testsuite>`).
    suite = NULL,
    #' @field suite_time Elapsed time for current suite.
    suite_time = NULL,
    #' @field file_name Current test file path.
    file_name = NULL,
    #' @field roxy_blocks Parsed roxygen2 blocks for current file.
    roxy_blocks = NULL,

    #' @description Return elapsed time since last call and reset timer.
    #' @return Elapsed time in seconds.
    elapsed_time = function() {
      time <- (private$proctime() - self$timer)[["elapsed"]]
      self$timer <- private$proctime()
      time
    },
    #' @description Reset suite counters to zero.
    reset_suite = function() {
      self$errors <- 0
      self$failures <- 0
      self$skipped <- 0
      self$tests <- 0
      self$suite_time <- 0
    },
    #' @description Initialize the reporter, create XML document.
    start_reporter = function() {
      rlang::check_installed("xml2", "to use JunitReporter")

      self$timer <- private$proctime()
      self$doc <- xml2::xml_new_document()
      self$root <- xml2::xml_add_child(self$doc, "testsuites")
      self$reset_suite()
    },
    #' @description Start processing a test file. Parses roxygen2 blocks.
    #' @param file Path to the test file.
    start_file = function(file) {
      self$file_name <- file
      self$roxy_blocks <- roxygen2::parse_file(file, env = NULL)
    },
    #' @description Called at the start of each test.
    #' @param context Test context name.
    #' @param test Test name.
    start_test = function(context, test) {
      if (is.null(context)) {
        context_start_file(self$file_name)
      }
    },
    #' @description Start a new test suite context.
    #' @param context Context name.
    start_context = function(context) {
      self$suite <- xml2::xml_add_child(
        self$root,
        "testsuite",
        name      = context,
        timestamp = private$timestamp(),
        hostname  = private$hostname()
      )
    },
    #' @description Finalize a test suite context. Writes counts and time.
    #' @param context Context name.
    end_context = function(context) {
      # Always uses . as decimal place in output regardless of options set
      # in test
      withr::local_options(list(OutDec = "."))
      xml2::xml_attr(self$suite, "tests") <- as.character(self$tests)
      xml2::xml_attr(self$suite, "skipped") <- as.character(self$skipped)
      xml2::xml_attr(self$suite, "failures") <- as.character(self$failures)
      xml2::xml_attr(self$suite, "errors") <- as.character(self$errors)
      # jenkins junit plugin requires time has at most 3 digits
      xml2::xml_attr(self$suite, "time") <-
        as.character(round(self$suite_time, 3))

      self$reset_suite()
    },
    #' @description Add a test result. Matches `@meta` tags from roxygen2
    #'   blocks and writes them as XML properties.
    #' @param context Context name.
    #' @param test Test name.
    #' @param result A testthat expectation object.
    add_result = function(context, test, result) {
      withr::local_options(list(OutDec = "."))
      self$tests <- self$tests + 1

      time <- self$elapsed_time()
      self$suite_time <- self$suite_time + time

      # XML node for test case
      name <- if (is.null(test)) "(unnamed)" else test
      testcase <- xml2::xml_add_child(
        self$suite, "testcase",
        time = toString(time),
        classname = testthat:::classnameOK(context),
        name = testthat:::classnameOK(name)
      )
      ### Add properties here
      roxy_block_names <- sapply(self$roxy_blocks, function(x) {
        match.call(definition = test_that, x$call)$desc
      })
      # Match roxy block to test case
      mid <- match(test, roxy_block_names)
      if (is.na(mid)) {
        tags_selected <- list()
      } else {
        tags_selected <- self$roxy_blocks[[mid]]$tags
      }
      # Add property children to test case
      properties <- xml2::xml_add_child(testcase, "properties")
      meta_tags <- tags_selected[sapply(
        tags_selected,
        function(x) x$tag
      ) == "meta"]
      lapply(meta_tags, function(mt) {
        xml2::xml_add_child(
          properties, "property",
          name = mt$val$type, value = mt$val$details
        )
      })
      first_line <- function(x) {
        loc <- testthat:::expectation_location(x, " (", ")")
        paste0(strsplit(x$message, split = "\n")[[1]][1], loc)
      }

      # add an extra XML child node if not a success
      if (testthat:::expectation_error(result)) {
        # "type" in Java is the exception class
        error <- xml2::xml_add_child(testcase, "error",
          type = "error",
          message = first_line(result)
        )
        xml2::xml_text(error) <- cli::ansi_strip(format(result))
        self$errors <- self$errors + 1
      } else if (testthat:::expectation_failure(result)) {
        # "type" in Java is the type of assertion that failed
        failure <- xml2::xml_add_child(testcase, "failure",
          type = "failure",
          message = first_line(result)
        )
        xml2::xml_text(failure) <- cli::ansi_strip(format(result))
        self$failures <- self$failures + 1
      } else if (testthat:::expectation_skip(result)) {
        xml2::xml_add_child(testcase, "skipped", message = first_line(result))
        self$skipped <- self$skipped + 1
      }
    },
    #' @description Write the XML document to the output destination.
    end_reporter = function() {
      if (is.character(self$out)) {
        xml2::write_xml(self$doc, self$out, format = TRUE)
      } else if (inherits(self$out, "connection")) {
        file <- withr::local_tempfile()
        xml2::write_xml(self$doc, file, format = TRUE)
        cat(brio::read_file(file), file = self$out)
      } else {
        stop("unsupported output type: ", toString(self$out))
      }
    } # end_reporter
  ), # public

  private = list(
    proctime = function() {
      proc.time()
    },
    timestamp = function() {
      strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    },
    hostname = function() {
      Sys.info()[["nodename"]]
    }
  ) # private
)
