#' Run a SPARQL SELECT query against a local ontology/graph file
#'
#' Thin wrapper around `robot query`, since ROBOT is already the standard
#' tool used to build and update the ont_mm graphs (see builds/README.md).
#' Writes the query to a temp file, shells out to
#' `robot query --input <graph_file> --query <query_file> <output_file>`,
#' and reads the result back in as a data.frame.
#'
#' PREFIX declarations for ex:, gc:, prov:, rdf:, rdfs:, owl:, dcterms:,
#' skos:, and bibo: are added automatically unless the query text already
#' declares them itself, so day-to-day queries can omit the boilerplate.
#'
#' @param graph_file Path to the ontology/graph file to query
#'   (e.g. "builds/gc_core.ttl").
#' @param query SPARQL query text (SELECT).
#' @param robot_cmd Command used to invoke robot (default "robot", assumes
#'   it is on PATH). Pass e.g. "java -jar /path/to/robot.jar" if not.
#' @param format Output format robot should write - "csv" (default) or "tsv".
#' @param prefixes Named character vector of additional/override prefixes,
#'   merged with the defaults (user-supplied values win).
#' @return A data.frame of query results. Returns a zero-row data.frame
#'   with a warning if the query has no results.
#' @export
sparql_query <- function(graph_file,
                          query,
                          robot_cmd = "robot",
                          format = c("csv", "tsv"),
                          prefixes = character(0)) {

  format <- match.arg(format)

  if (!file.exists(graph_file)) {
    stop("Graph file not found: ", graph_file)
  }

  default_prefixes <- c(
    ex      = "http://example.org/",
    gc      = "http://purl.org/gc/",
    prov    = "http://www.w3.org/ns/prov#",
    rdf     = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs    = "http://www.w3.org/2000/01/rdf-schema#",
    owl     = "http://www.w3.org/2002/07/owl#",
    dcterms = "http://purl.org/dc/terms/",
    skos    = "http://www.w3.org/2004/02/skos/core#",
    bibo    = "http://purl.org/ontology/bibo/"
  )

  # user-supplied prefixes override defaults with the same name
  all_prefixes <- default_prefixes
  all_prefixes[names(prefixes)] <- prefixes

  # only add prefixes the query hasn't already declared itself
  already_declared <- vapply(names(all_prefixes), function(p) {
    grepl(paste0("PREFIX\\s+", p, ":"), query, ignore.case = TRUE)
  }, logical(1))

  needed <- names(all_prefixes)[!already_declared]

  if (length(needed) > 0) {
    preamble <- paste0("PREFIX ", needed, ": <", all_prefixes[needed], ">",
                        collapse = "\n")
    query <- paste(preamble, query, sep = "\n")
  }

  query_file  <- tempfile(fileext = ".sparql")
  output_file <- tempfile(fileext = paste0(".", format))
  on.exit(unlink(c(query_file, output_file)), add = TRUE)

  writeLines(query, query_file)

  cmd_parts <- strsplit(robot_cmd, "\\s+")[[1]]
  cmd  <- cmd_parts[1]
  args <- c(
    cmd_parts[-1],
    "query",
    "--input", path.expand(graph_file),
    "--query", query_file, output_file
  )

  result <- suppressWarnings(
    system2(cmd, args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(result, "status")

  if (!is.null(status) && status != 0) {
    stop("robot query failed:\n", paste(result, collapse = "\n"))
  }

  if (!file.exists(output_file) || file.size(output_file) == 0) {
    warning("Query returned no results")
    return(data.frame())
  }

  reader <- if (format == "csv") utils::read.csv else utils::read.delim
  reader(output_file, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Resolve a file:// URL (as stored in ex:fileURL) to a local filesystem path
#'
#' @param url Character vector of file:// URLs (or plain paths, passed through).
#' @return Character vector of local paths. NA for anything that isn't a
#'   recognisable file:// URL.
#' @export
resolve_file_url <- function(url) {

  out <- rep(NA_character_, length(url))

  is_file_url <- grepl("^file:/{2,3}", url)
  out[is_file_url] <- sub("^file:/{2,3}", "/", url[is_file_url])
  out[is_file_url] <- utils::URLdecode(out[is_file_url])

  # anything with no scheme at all is treated as a plain local path already
  no_scheme <- !is_file_url & !grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", url)
  out[no_scheme] <- url[no_scheme]

  path.expand(out)
}


#' Resolve and check a set of file:// URLs, reporting what's missing
#'
#' Meant to sit directly after sparql_query(): take whatever column of
#' ex:fileURL values the query returned, resolve each to a local path, and
#' flag anything that doesn't exist on this machine (e.g. because the
#' ontology was built on a different host, or the file has since moved).
#'
#' @param urls Character vector of ex:fileURL values.
#' @return A data.frame with columns: url, path, exists.
#' @export
batch_resolve <- function(urls) {

  paths  <- resolve_file_url(urls)
  exists <- file.exists(paths)
  exists[is.na(paths)] <- NA

  df <- data.frame(
    url    = urls,
    path   = paths,
    exists = exists,
    stringsAsFactors = FALSE
  )

  missing <- df$url[!is.na(df$exists) & !df$exists]
  if (length(missing) > 0) {
    warning(
      "Some resolved files do not exist on this machine (",
      length(missing), " of ", length(urls), "):\n",
      paste(" -", missing, collapse = "\n")
    )
  }

  df
}
