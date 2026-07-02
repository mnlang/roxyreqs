#' Parse and format roxygen2 meta tags
#'
#' S3 methods that extend roxygen2 to support `@meta` tags in function and
#' test documentation. Tags follow the format `@meta key value` where `key`
#' can be any user-defined identifier (underscores become spaces, title-cased
#' in output).
#'
#' `format.rd_section_meta` automatically computes a `Test Scope` value from
#' `Compliance Risk` and `Implementation Complexity` using an internal
#' scopemap matrix. Tags are reordered with `Id`, `Compliance Risk`,
#' `Implementation Complexity`, and `Test Scope` first, followed by any
#' custom tags.
#'
#' @param x A roxygen2 tag, rd_section, or Rd object depending on the method.
#' @param base_path Base path of the package (used by roxygen2 dispatch).
#' @param env Package environment (used by roxygen2 dispatch).
#' @param y A second rd_section_meta object to merge with `x`.
#' @param ... Additional arguments passed to methods.
#'
#' @name meta_tags
#' @meta id URS01.FS008
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_meta <- function(x) {
  re <- "(?<type>[^\\s]+)\\s+(?<details>.*)"
  x$raw <- gsub("\n", " ", x$raw, fixed = TRUE)
  x$raw <- gsub("\\s+", " ", x$raw, perl = TRUE)
  fields <- recapture(re, x$raw)
  x$val <- lapply(fields, trimws)
  x
}

#' @rdname meta_tags
#' @meta id URS01.FS009
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @importFrom roxygen2 roxy_tag_rd
#' @export
roxy_tag_rd.roxy_tag_meta <- function(x, base_path, env) {
  roxygen2::rd_section("meta", x$val)
}

#' @rdname meta_tags
#' @meta id URS01.FS010
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @export
merge.rd_section_meta <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  roxygen2::rd_section(x$type, Map(c, x$value, y$value))
}

#' Lookup matrix mapping Compliance Risk x Implementation Complexity to
#' Test Scope. Used by format.rd_section_meta to auto-compute Test Scope.
#' @noRd
scopemap <- data.frame(
  `Compliance Risk` = c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high"),
  `Implementation Complexity` = c("low", "low", "low", "medium", "medium", "medium", "high", "high", "high"),
  `Test Scope` = c("low", "medium", "high", "medium", "medium", "high", "high", "high", "high"),
  check.names = FALSE
)

#' @rdname meta_tags
#' @meta id URS01.FS011
#' @meta compliance_risk low
#' @meta implementation_complexity low
#'
#' @export
format.rd_section_meta <- function(x, ...) {
  tmp_key_value <- list()
  for (i in seq_along(x$value$type)) {
    tmp_key_value[[x$value$type[i]]] <- x$value$details[i]
  }

  names(tmp_key_value) <- stringr::str_to_title(gsub("_", " ", names(tmp_key_value)))

  if (all(names(scopemap)[1:2] %in% names(tmp_key_value))) {
    scopeval <- scopemap[scopemap$`Compliance Risk` == tmp_key_value$`Compliance Risk` &
      scopemap$`Implementation Complexity` == tmp_key_value$`Implementation Complexity`, 3]
    if (length(scopeval) > 0) {
      tmp_key_value[[names(scopemap)[3]]] <- scopeval
    }
  }

  # Re-order Meta Tags
  pre_order <- c("Id", names(scopemap))

  midx <- match(pre_order, names(tmp_key_value))
  append_idx <- setdiff(seq_along(tmp_key_value), midx)
  all_idx <- stats::na.omit(c(midx, append_idx))

  tmp_key_value <- tmp_key_value[all_idx]

  values <- paste(names(tmp_key_value),
    unlist(tmp_key_value, use.names = FALSE),
    sep = ": ", collapse = "\n\n"
  )
  paste0("\\", "meta", paste0("{\n", values, "\n}", collapse = ""))
}
