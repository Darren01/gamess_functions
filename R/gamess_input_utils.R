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
#' Returns only the FIRST occurrence - correct for blocks that appear at
#' most once per file ($CONTRL, $STATPT, $SCF, $BASIS). For blocks that
#' can legitimately appear more than once (e.g. $ZMAT), use
#' get_gamess_blocks() (plural) instead.
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


#' Parse GAMESS frequency values from FREQUENCY: lines
#'
#' GAMESS marks imaginary frequencies with a trailing "I" rather than a
#' negative sign (e.g. "139.50" vs "45.20I"). This returns imaginary
#' modes as negative numbers, a convention used throughout this package
#' (has_imaginary checks, imaginary flags, etc.).
#'
#' Was previously duplicated near-identically in extract_ir_spectrum.R
#' and extract_ir_diagnostics.R - consolidated here so the two can't
#' silently drift apart, same reasoning as get_gamess_block()/
#' get_gamess_blocks().
#'
#' @param freq_lines character vector of raw "FREQUENCY: ..." lines
#' @return numeric vector of frequencies (cm-1), imaginary modes negative
#' @export
parse_gamess_frequencies <- function(freq_lines) {
  parse_one_line <- function(line) {
    m <- gregexpr("-?\\d+\\.\\d+\\s*I?", line, perl = TRUE)
    tokens <- regmatches(line, m)[[1]]
    vapply(tokens, function(t) {
      val <- as.numeric(gsub("I", "", t))
      if (grepl("I", t)) -abs(val) else val
    }, numeric(1))
  }
  unlist(lapply(freq_lines, parse_one_line), use.names = FALSE)
}
#'
#' Same matching logic as get_gamess_block(), but returns ALL
#' non-overlapping occurrences rather than just the first - for blocks
#' that can legitimately appear more than once in the same file (e.g.
#' $ZMAT, which real input decks have been observed to repeat).
#'
#' @param lines character vector of file lines (already prefix-stripped)
#' @param block_name e.g. "ZMAT" (no $ needed)
#' @return a list of character vectors, one per occurrence (each
#'   inclusive of its $BLOCKNAME and $END lines), or an empty list if
#'   none found
#' @export
get_gamess_blocks <- function(lines, block_name) {
  start <- grep(paste0("^\\s*\\$", block_name, "\\b"), lines, ignore.case = TRUE)
  end   <- grep("\\$END\\b", lines, ignore.case = TRUE)

  blocks <- list()
  for (s in start) {
    e <- end[end >= s][1]
    if (!is.na(e)) {
      blocks[[length(blocks) + 1]] <- lines[s:e]
    }
  }
  blocks
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


#' Parse GAMESS frequency values from "FREQUENCY:" lines
#'
#' GAMESS marks imaginary modes with a trailing "I" (e.g. "139.50 I"),
#' printed as a positive magnitude with the I suffix rather than a
#' negative number. This converts imaginary modes to negative numbers
#' (the convention used throughout this package - see has_imaginary /
#' imaginary columns elsewhere), so downstream code just checks sign.
#'
#' Previously duplicated identically in extract_ir_diagnostics.R and
#' extract_ir_spectrum.R - moved here as the one shared implementation,
#' same reasoning as get_gamess_block()/parse_gamess_block().
#'
#' @param freq_lines Character vector of raw "FREQUENCY: ..." lines
#'   (e.g. from grep("FREQUENCY:", lines, value = TRUE)).
#' @return A numeric vector of frequencies, imaginary modes negative.
#' @export
parse_gamess_frequencies <- function(freq_lines) {
  if (length(freq_lines) == 0) return(numeric(0))

  parse_one <- function(line) {
    m <- gregexpr("-?\\d+\\.\\d+\\s*I?", line, perl = TRUE)
    tokens <- regmatches(line, m)[[1]]

    vapply(tokens, function(t) {
      val <- as.numeric(gsub("I", "", t))
      if (grepl("I", t)) -abs(val) else val
    }, numeric(1))
  }

  unlist(lapply(freq_lines, parse_one), use.names = FALSE)
}
