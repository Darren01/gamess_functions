#' Turn a doi_notes.tsv into full paper_template_instances.tsv rows
#'
#' Mirrors notes_to_annotations()/write_annotations()'s pattern exactly:
#' you write a simple TSV of DOIs and why they matter, this fetches the
#' real bibliographic detail and builds the full template rows - you
#' never hand-type a citation.
#'
#' Same safety property as write_annotations(): regenerates the output
#' file fresh each time, rather than appending - safe to edit
#' doi_notes.tsv and re-run at any time as your project grows, without
#' worrying about duplicating already-processed papers.
#'
#' @param doi_notes_file Path to a TSV with columns DOI, Note,
#'   RelatesTo (RelatesTo is optional - a pipe-separated list of
#'   ex:exp_* IDs the paper is relevant to, e.g.
#'   "ex:exp_caa004a|ex:exp_caa004m" - leave blank if not yet decided).
#' @param output_file Path to write the instances TSV (overwritten
#'   completely each time).
#' @export
doi_notes_to_templates <- function(doi_notes_file, output_file) {

  notes <- read.delim(doi_notes_file, stringsAsFactors = FALSE, colClasses = "character")

  required_cols <- c("DOI", "Note")
  missing_cols <- setdiff(required_cols, names(notes))
  if (length(missing_cols) > 0) {
    stop("doi_notes_file is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (!"RelatesTo" %in% names(notes)) notes$RelatesTo <- ""

  header <- c("ID", "Label", "Type", "Title", "Creator", "Year", "DOI",
              "Volume", "Issue", "PageStart", "PageEnd", "Citation", "RelatesTo")
  type_row <- c("ID", "LABEL", "TYPE", "A dcterms:title",
                "A dcterms:creator SPLIT=|", "AT dcterms:date^^xsd:gYear",
                "A dcterms:identifier", "A bibo:volume", "A bibo:issue",
                "A bibo:pageStart", "A bibo:pageEnd",
                "A dcterms:bibliographicCitation", "I dcterms:relation SPLIT=|")

  rows <- character(0)
  failed <- character(0)

  for (i in seq_len(nrow(notes))) {
    doi <- trimws(notes$DOI[i])
    if (!nzchar(doi)) next

    meta <- tryCatch(fetch_doi_metadata(doi), error = function(e) {
      warning("Could not fetch metadata for ", doi, ": ", conditionMessage(e))
      NULL
    })
    if (is.null(meta)) {
      failed <- c(failed, doi)
      next
    }

    first_author_surname <- if (length(meta$authors) > 0 && !is.na(meta$authors[1])) {
      tolower(gsub("[^A-Za-z]", "", strsplit(meta$authors[1], ",")[[1]][1]))
    } else {
      "unknown"
    }
    # ID is the real, resolvable DOI URL itself - not a derived
    # surname+year label. Deliberate choice: aimed at a generic reader
    # who knows nothing about this project's own ex: conventions but
    # would recognise and could click a real URL. Also solves a real
    # chicken-and-egg problem: run_notes.tsv (written before any
    # metadata is fetched) can only ever know the raw DOI, never a
    # surname+year label that depends on a successful fetch - using the
    # DOI URL directly means both sides can always agree on the same ID
    # without needing to coordinate.
    id <- paste0("<https://doi.org/", doi, ">")
    label <- paste0(first_author_surname, " ", if (!is.na(meta$year)) meta$year else "")

    creator_str <- if (length(meta$authors) > 0) paste(meta$authors, collapse = "|") else ""

    citation <- paste0(
      if (length(meta$authors) > 0) paste(meta$authors, collapse = "; ") else "",
      " ", if (!is.na(meta$journal)) meta$journal else "",
      " ", if (!is.na(meta$year)) meta$year else "",
      ", ", if (!is.na(meta$volume)) meta$volume else "",
      if (!is.na(meta$issue)) paste0(" (", meta$issue, ")") else "",
      ", ", if (!is.na(meta$page_start)) meta$page_start else "",
      if (!is.na(meta$page_end)) paste0("-", meta$page_end) else "", "."
    )

    row <- c(id, label, "bibo:AcademicArticle",
             if (!is.na(meta$title)) meta$title else "",
             creator_str,
             if (!is.na(meta$year)) meta$year else "",
             doi,
             if (!is.na(meta$volume)) meta$volume else "",
             if (!is.na(meta$issue)) meta$issue else "",
             if (!is.na(meta$page_start)) meta$page_start else "",
             if (!is.na(meta$page_end)) meta$page_end else "",
             citation,
             trimws(notes$RelatesTo[i]))

    rows <- c(rows, paste(row, collapse = "\t"))
  }

  writeLines(paste(header, collapse = "\t"), output_file)
  write(paste(type_row, collapse = "\t"), output_file, append = TRUE)
  for (r in rows) write(r, output_file, append = TRUE)

  cat("Wrote", length(rows), "paper(s) to", output_file,
      "(file regenerated fresh, not appended)\n")
  if (length(failed) > 0) {
    cat("Failed to fetch:", paste(failed, collapse = ", "), "\n")
  }

  invisible(list(written = length(rows), failed = failed))
}
