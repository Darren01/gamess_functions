#' Convert run_notes.tsv into rows for the annotation template
#'
#' run_notes.tsv is a simple, manually-maintained tab-separated file:
#' one line per note, "<filename>\t<comment>", with an optional third
#' field, "<filename>\t<comment>\t<doi_list>", where doi_list is one or
#' more DOIs (pipe-separated for more than one) for papers this
#' specific run is based on or being compared against - e.g. a scan
#' whose constraint distance came from a literature bond length.
#' Every note maps to the experiment individual (ex:exp_<stem>)
#' regardless of whether the filename referenced is the .inp or .log -
#' a note naming a specific file is still fundamentally a comment about
#' that run as a whole, not about the file as a data artifact.
#'
#' Genuinely generic, not specific to any one project - any project
#' using run_notes.tsv can add DOIs to any line, regardless of what the
#' experiment or the literature is actually about.
#'
#' A line must have EXACTLY 2 or EXACTLY 3 tab-separated fields - not
#' "2 or more, with everything after the first tab treated as the
#' comment" as an earlier version of this function did. That earlier,
#' more permissive design can't be kept alongside a real third field:
#' there would be no reliable way to tell "a comment with 3 tab-
#' separated parts, no DOI" apart from "a comment plus a real DOI
#' field" if both were accepted length-flexibly. A comment
#' containing a literal tab character is now correctly flagged as
#' ambiguous rather than silently absorbed - genuinely unlikely in
#' practice for hand-typed free text, and predictable is better than a
#' fragile heuristic here.
#'
#' The DOI(s), if present, are linked using the exact same resolvable
#' URL form used for the paper's own ID in doi_notes_to_templates() -
#' <https://doi.org/...> - so a query joining on dcterms:relation
#' always matches correctly on both sides, without either side needing
#' to know how the other generates its IDs.
#'
#' Type is deliberately set to gc:MolecularComputation (the same safe,
#' general ancestor used by every other "reopen an existing experiment"
#' writer this project) - not because this template needs to assert
#' anything about the experiment's specific type, but because leaving
#' Type blank would let ROBOT default the individual to a bare
#' owl:Class in this template's own standalone build (each template
#' builds independently against the bare release, before the final
#' merge combines everything) - the exact bug already found and fixed
#' in constraints_to_templates.R.
#'
#' @param notes_file Path to a run_notes.tsv file.
#' @return A data.frame: ID, Type, Comment, RelatesTo - one row per
#'   note. RelatesTo is an empty string for notes with no DOI, not NA.
#' @export
notes_to_annotations <- function(notes_file) {

  if (!file.exists(notes_file)) {
    stop("File not found: ", notes_file)
  }

  lines <- readLines(notes_file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0) {
    stop("No notes found in ", notes_file)
  }

  normalise_doi_url <- function(doi) {
    doi <- trimws(doi)
    doi <- sub("^<|>$", "", doi)
    doi <- sub("^https?://doi\\.org/", "", doi)
    paste0("https://doi.org/", doi)
  }

  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]

    if (!length(parts) %in% c(2, 3)) {
      warning("Skipping malformed line (expected exactly 'filename<TAB>comment' or ",
              "'filename<TAB>comment<TAB>doi_list'): '", line, "'")
      return(NULL)
    }

    filename <- trimws(parts[1])
    comment <- trimws(parts[2])
    stem <- sub("\\.(inp|log|dat|rst)$", "", filename)
    exp_id <- paste0("ex:exp_", stem)

    relates_to <- ""
    if (length(parts) == 3 && nzchar(trimws(parts[3]))) {
      dois <- strsplit(trimws(parts[3]), "|", fixed = TRUE)[[1]]
      relates_to <- paste(vapply(dois, normalise_doi_url, character(1)), collapse = "|")
    }

    data.frame(ID = exp_id, Type = "gc:MolecularComputation", Comment = comment,
               RelatesTo = relates_to, stringsAsFactors = FALSE)
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    stop("Found lines in ", notes_file, " but none were correctly formatted")
  }

  do.call(rbind, rows)
}


#' Write annotation rows to an instances file
#'
#' Always regenerates the file fresh, rather than appending - this is
#' the correct behaviour specifically for annotations, unlike the
#' shared float_value/spectra_result templates elsewhere in this
#' project (which genuinely need append-across-calls, since different
#' writers each contribute rows nobody else has visibility into).
#' notes_to_annotations() always reads run_notes.tsv in full and
#' returns every note, so appending here would duplicate every
#' already-processed experiment's annotation each time run_notes.tsv
#' is edited and this is re-run - found before it caused a real
#' problem, not after.
#'
#' @param rows Output of notes_to_annotations().
#' @param output_file Path to write the instances TSV (overwritten
#'   completely each time).
#' @export
write_annotations <- function(rows, output_file) {
  header <- c("ID", "Type", "Comment", "RelatesTo")
  type_row <- c("ID", "TYPE", "A skos:editorialNote", "I dcterms:relation SPLIT=|")

  writeLines(paste(header, collapse = "\t"), output_file)
  write(paste(type_row, collapse = "\t"), output_file, append = TRUE)
  write.table(rows, output_file, sep = "\t", row.names = FALSE,
              col.names = FALSE, quote = FALSE, append = TRUE)

  cat("Wrote", nrow(rows), "annotation(s) to", output_file,
      "(file regenerated fresh, not appended)\n")
}
