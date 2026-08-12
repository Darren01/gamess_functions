#' Fetch real bibliographic metadata for a DOI via content negotiation
#'
#' Rebuilt from scratch - the original version of this function was
#' tested working in an earlier session, but a check of the real,
#' current repo found it was never actually committed (confirmed via
#' git clone, not assumed from memory) - this is a fresh, careful
#' rebuild, not a recovery of the lost original.
#'
#' Uses the standard doi.org/CSL-JSON content negotiation approach:
#' requesting https://doi.org/{doi} with an
#' Accept: application/vnd.citationstyles.csl+json header returns
#' structured bibliographic metadata (title, authors, journal, volume,
#' pages, etc.) as JSON, regardless of publisher - this is a real,
#' documented DOI.org/Crossref standard, not something invented for
#' this project.
#'
#' Tries base R's url() with a custom header first (works via libcurl
#' on most systems); falls back to a system curl call if that fails
#' (e.g. behind some corporate proxies where R's own networking is
#' restricted but a system curl install works - the same class of
#' issue this project has hit before with corporate SSL inspection).
#'
#' NOTE: not testable in the environment this was written in (no
#' network access to doi.org) - please test against a real DOI before
#' trusting this, same as everything else built this way today.
#'
#' @param doi A DOI, with or without a leading "10." check - either
#'   "10.1021/ja00249a034" or a full "https://doi.org/..." URL works.
#' @return A list: title, authors (character vector, "Family, Given"),
#'   year, journal, volume, issue, page_start, page_end, doi. Any field
#'   not present in the source metadata is NA, not an error - not every
#'   DOI's metadata has every field.
#' @export
fetch_doi_metadata <- function(doi) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with: ",
         "install.packages('jsonlite')")
  }

  doi <- sub("^https?://doi\\.org/", "", doi)
  url_str <- paste0("https://doi.org/", doi)

  raw_json <- tryCatch({
    con <- url(url_str, headers = c(Accept = "application/vnd.citationstyles.csl+json"))
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }, error = function(e) NULL)

  if (is.null(raw_json) || !nzchar(raw_json)) {
    raw_json <- tryCatch({
      system2("curl", c("-sL", "-H", "'Accept: application/vnd.citationstyles.csl+json'", shQuote(url_str)),
              stdout = TRUE, stderr = FALSE)
    }, error = function(e) NULL)
    raw_json <- paste(raw_json, collapse = "\n")
  }

  if (is.null(raw_json) || !nzchar(raw_json)) {
    stop("Could not fetch metadata for DOI ", doi,
         " - both url() and curl fallback failed. Check network access to doi.org.")
  }

  parsed <- tryCatch(jsonlite::fromJSON(raw_json, simplifyVector = FALSE),
                      error = function(e) {
                        stop("Fetched a response for ", doi,
                             " but it wasn't valid JSON - possibly blocked, redirected ",
                             "to an HTML page, or rate-limited. Raw response starts: ",
                             substr(raw_json, 1, 200))
                      })

  authors <- NA_character_
  if (!is.null(parsed$author)) {
    authors <- vapply(parsed$author, function(a) {
      family <- if (!is.null(a$family)) a$family else ""
      given  <- if (!is.null(a$given)) a$given else ""
      trimws(paste0(family, if (nzchar(family) && nzchar(given)) ", " else "", given))
    }, character(1))
  }

  year <- NA_character_
  if (!is.null(parsed$issued$`date-parts`)) {
    dp <- parsed$issued$`date-parts`[[1]]
    if (length(dp) >= 1) year <- as.character(dp[[1]])
  }

  page_start <- NA_character_
  page_end <- NA_character_
  if (!is.null(parsed$page)) {
    parts <- strsplit(parsed$page, "[-\u2013]")[[1]]
    if (length(parts) >= 1) page_start <- trimws(parts[1])
    if (length(parts) >= 2) page_end <- trimws(parts[2])
  }

  list(
    title      = if (!is.null(parsed$title)) parsed$title else NA_character_,
    authors    = authors,
    year       = year,
    journal    = if (!is.null(parsed$`container-title`)) parsed$`container-title` else NA_character_,
    volume     = if (!is.null(parsed$volume)) parsed$volume else NA_character_,
    issue      = if (!is.null(parsed$issue)) parsed$issue else NA_character_,
    page_start = page_start,
    page_end   = page_end,
    doi        = doi
  )
}
