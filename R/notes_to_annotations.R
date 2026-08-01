#' Convert run_notes.tsv into rows for the annotation template
#'
#' run_notes.tsv is a simple, manually-maintained tab-separated file:
#' one line per note, "<filename>\t<comment>". Every note maps to the
#' experiment individual (ex:exp_<stem>) regardless of whether the
#' filename referenced is the .inp or .log - a note naming a specific
#' file is still fundamentally a comment about that run as a whole, not
#' about the file as a data artifact.
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
#' @return A data.frame: ID, Type, Comment - one row per note.
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

  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      warning("Skipping malformed line (expected 'filename<TAB>comment'): '", line, "'")
      return(NULL)
    }

    filename <- trimws(parts[1])
    comment <- trimws(paste(parts[-1], collapse = "\t"))
    stem <- sub("\\.(inp|log|dat|rst)$", "", filename)
    exp_id <- paste0("ex:exp_", stem)

    data.frame(ID = exp_id, Type = "gc:MolecularComputation", Comment = comment,
               stringsAsFactors = FALSE)
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    stop("Found lines in ", notes_file, " but none were correctly formatted")
  }

  do.call(rbind, rows)
}


#' Write annotation rows to an instances file
#'
#' @param rows Output of notes_to_annotations().
#' @param output_file Path to write (or append to) the instances TSV.
#' @export
write_annotations <- function(rows, output_file) {
  header <- c("ID", "Type", "Comment")
  type_row <- c("ID", "TYPE", "A rdfs:comment")

  if (file.exists(output_file)) {
    write.table(rows, output_file, sep = "\t", row.names = FALSE,
                col.names = FALSE, quote = FALSE, append = TRUE)
  } else {
    writeLines(paste(header, collapse = "\t"), output_file)
    write(paste(type_row, collapse = "\t"), output_file, append = TRUE)
    write.table(rows, output_file, sep = "\t", row.names = FALSE,
                col.names = FALSE, quote = FALSE, append = TRUE)
  }

  cat("Wrote", nrow(rows), "annotation(s) to", output_file, "\n")
}
