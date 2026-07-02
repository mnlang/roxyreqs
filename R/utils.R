#' Extract Pattern Capture Groups
#'
#' @param pattern A (perl) regular expression to parse
#' @param x A string to parse from
#' @noRd
recapture <- function(pattern, x) {
  m <- regexpr(pattern, x, perl = TRUE)
  mapply(substr,
    as.data.frame(s <- attr(m, "capture.start")),
    x = x,
    stop = s + attr(m, "capture.length") - 1L
  )
}


#' Check if an expression is a test_that call

#' @param expr An R expression to check
#' @noRd
is_test_that_call <- function(expr) {
  is.call(expr) &&
    (expr[[1]] == quote(test_that) ||
      expr[[1]] == quote(testthat::test_that))
}

#' Move macro (meta) fragment right after arguments
#'
#' @param x An Rd object as returned by \code{tools::parse_Rd()}
#' @param rd_tag character; Rd tag name after which macro section shall be moved
#' @param offset integer; Offset index, so that macro block is moved after
#' rd_tag plus following text.
#' @returns transformed Rd object
#'
#' @meta id URS01.FS012
#' @meta compliance_risk low
#' @meta implementation_complexity low
#' @export
move_macro_first <- function(x, rd_tag = "\\title", offset = 1L) {
  macroidx <- which(sapply(x, attr, "Rd_tag") == "USERMACRO")[1]
  headeridx <- which(sapply(x, attr, "Rd_tag") == rd_tag) + offset
  xidx <- seq_along(x)
  out <- c(x[1:headeridx], x[xidx >= macroidx], x[xidx > headeridx & xidx < macroidx])
  class(out) <- "Rd"
  out
}
