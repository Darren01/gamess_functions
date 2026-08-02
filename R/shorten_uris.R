#' Strip namespace prefixes from a SPARQL result, for readability
#'
#' Optional - wrap any sparql_query() result in this to see short,
#' readable names (e.g. "exp_caa001b", "VibrationalAnalysis",
#' "angstrom") instead of full URIs. Leaves anything that isn't a URI -
#' numbers, free-text comments, anything without "http(s)://" at the
#' start - completely untouched, including text with punctuation or
#' apostrophes.
#'
#' @param df A data.frame, typically the output of sparql_query().
#' @return The same data.frame, with every character column's URI-like
#'   values shortened to their final path segment.
#' @export
shorten_uris <- function(df) {
  df[] <- lapply(df, function(col) {
    if (!is.character(col)) return(col)
    is_uri <- grepl("^https?://", col)
    col[is_uri] <- sub("^.*[/#]", "", col[is_uri])
    col
  })
  df
}
