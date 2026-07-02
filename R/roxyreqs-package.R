#' @keywords internal
"_PACKAGE"

# Packages used in R6 class bodies (reporter-junit.R) are not detected
# by R CMD check's namespace analysis. These references ensure they are
# recognized as used.
ignore_unused_imports <- function() {
  brio::read_file
  R6::R6Class
  rlang::check_installed
  withr::local_options
}
