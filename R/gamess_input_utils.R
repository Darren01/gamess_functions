#' Strip GAMESS's "INPUT CARD>" echo prefix from a line, if present
#'
#' GAMESS echoes the input deck verbatim into the .log file, prefixed with
#' "INPUT CARD>". Raw .inp files have no such prefix. Stripping it (as a
#' no-op when absent) lets every other input-parsing function work
#' identically on either file type, rather than each one needing its own
#' notion of "which file format am I looking at".
#'
#' @param lines character vector of file lines
#' @return character vector, same length, with any leading INPUT CARD>
#'   removed from each line
#' @export
strip_input_card_prefix <- function(lines) {
  sub("^\\s*INPUT CARD>\\s*", "", lines, ignore.case = TRUE)
}


#' Extract a $BLOCKNAME ... $END span from GAMESS input lines
#'
#' Works on either a raw .inp file or a .log file (via
#' strip_input_card_prefix()). Matches by line position, not a single-line
#' regex, so it's robust to a block being split across multiple physical
#' lines - which GAMESS allows and which occurs in real input decks.
#'
#' @param lines character vector of file lines (already prefix-stripped)
#' @param block_name e.g. "CONTRL", "BASIS", "STATPT", "SCF" (no $ needed)
#' @return character vector of the matched lines (inclusive of the
#'   $BLOCKNAME and $END lines), or NULL if not found
#' @export
get_gamess_block <- function(lines, block_name) {
  start <- grep(paste0("^\\s*\\$", block_name, "\\b"), lines, ignore.case = TRUE)
  end   <- grep("\\$END\\b", lines, ignore.case = TRUE)

  if (length(start) == 0) return(NULL)

  end <- end[end >= start[1]]
  if (length(end) == 0) return(NULL)

  lines[start[1]:end[1]]
}


#' Parse "KEY=VALUE" pairs out of a GAMESS input block
#'
#' @param block_lines character vector from get_gamess_block(), or NULL
#' @return a named list (lowercase keys), empty list if block_lines is NULL
#' @export
parse_gamess_block <- function(block_lines) {
  if (is.null(block_lines) || length(block_lines) == 0) return(list())

  txt <- paste(block_lines, collapse = " ")
  txt <- gsub("\\$[A-Z]+", "", txt, ignore.case = TRUE)

  tokens <- unlist(strsplit(txt, "\\s+"))

  res <- list()
  for (tok in tokens) {
    if (grepl("=", tok)) {
      parts <- strsplit(tok, "=")[[1]]
      key <- tolower(trimws(parts[1]))
      val <- trimws(parts[2])
      res[[key]] <- val
    }
  }
  res
}
