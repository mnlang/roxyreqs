#' Parse Metadata from Rd Files
#'
#' @param filepath Character array specifying paths to Rd-files.
#' @return data.frame containing one column for each meta tag and one row
#' per filepath.
#'
#' @meta id URS01.FS006
#' @meta compliance_risk low
#' @meta implementation_complexity low
#' @export
parse_rd_meta <- function(filepath) {
  stopifnot(is.character(filepath))

  df <- list()
  for (fp in filepath) {
    rd_text <- paste(readLines(fp), collapse = "\n")
    out <- list(filepath = fp)
    if (grepl("\\meta", rd_text, fixed = TRUE)) {
      rd_text_meta <- gsub("^.*meta\\{\\s*|\\s*\\}.*$", "", rd_text)
      meta_chunks <- strsplit(rd_text_meta, "\n", fixed = TRUE)[[1]]
      meta_chunks <- meta_chunks[meta_chunks != ""]
      for (x in meta_chunks) {
        meta_split <- strsplit(x, "\\s*:\\s*")[[1]]
        out[[meta_split[1]]] <- meta_split[2]
      }
    }

    df[[fp]] <- out
  }
  # Combine list of data.frames with potentially different columns
  all_cols <- unique(unlist(lapply(df, names)))
  df <- lapply(df, function(x) {
    missing <- setdiff(all_cols, names(x))
    x[missing] <- NA
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  })
  df <- do.call(rbind, df)
  rownames(df) <- NULL
  df
}
