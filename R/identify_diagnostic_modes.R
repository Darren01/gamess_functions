#' Identify which vibrational modes are diagnostically relevant
#'
#' A GAMESS vibrational analysis typically reports one frequency per
#' 3N-6 (or 3N-5) mode - most of which are neither queried nor
#' individually meaningful. Two kinds of mode ARE genuinely meaningful
#' on their own: any imaginary (negative) frequency (confirms a genuine
#' transition state, or flags an unconverged geometry), and the modes
#' GAMESS itself identifies as translation/rotation (a geometry-quality
#' signal - see check_vibrational_quality()). This function identifies
#' both, by mode index, for any caller that wants them - independent of
#' whether the result ever ends up written into an ontology graph at
#' all.
#'
#' Deliberately does no parsing of its own: extract_ir_diagnostics()
#' already parses the real "MODES N TO M ARE TAKEN AS ROTATIONS AND
#' TRANSLATIONS" line (confirmed against real data that this range
#' genuinely varies per file - not always 1-6, sometimes 2-7, or 3-8
#' when there are two imaginary modes) and already returns every
#' frequency in call order. This function only combines what's already
#' there, rather than re-deriving it.
#'
#' @param file Path to a GAMESS .log file.
#' @return A list: keep_modes (integer vector, sorted, the union of the
#'   two categories below - the modes worth keeping), imaginary_modes
#'   (integer vector, 1-based mode indices with a negative frequency),
#'   trans_rot_modes (integer vector, the indices GAMESS itself
#'   identified), frequencies (numeric vector, every mode, in order -
#'   passed through from extract_ir_diagnostics() for convenience,
#'   so a caller can look up any kept mode's real value without a
#'   second file read).
#' @export
identify_diagnostic_modes <- function(file) {
  diag <- extract_ir_diagnostics(file)

  imaginary_modes <- which(diag$frequencies < 0)
  trans_rot_modes <- diag$trans_rot_modes
  keep_modes <- sort(union(imaginary_modes, trans_rot_modes))

  list(
    keep_modes = keep_modes,
    imaginary_modes = imaginary_modes,
    trans_rot_modes = trans_rot_modes,
    frequencies = diag$frequencies
  )
}
