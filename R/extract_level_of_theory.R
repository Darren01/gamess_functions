#' Extract a level-of-theory label from a GAMESS log's input echo
#'
#' Builds a human-readable string like "6-31G(d,p) PCM(water)" from the
#' real basis set, method, and $PCM keywords in the input echo, for use
#' in summary tables.
#'
#' Reuses extract_basis_name() (extract_basis.R) for the basis set
#' portion, rather than a separate, less complete interpreter - a real
#' overlap found via a README review: an earlier version of this
#' function duplicated basis-name interpretation with a small lookup
#' table (only N21/N31), missing STO-nG, DZV/TZV, Dunning aug- prefix
#' handling, and polarization suffixes that extract_basis_name() already
#' handled correctly. Requires gamess_input_utils.R and extract_basis.R
#' to be sourced first (extract_basis_name()'s own dependency).
#'
#' Deliberately does NOT attempt to translate a BASNAM (mixed/custom
#' basis set) job into a single basis-set label - per GAMESS's own
#' documentation, BASNAM lets different atoms have completely
#' different, independently-defined basis sets, so no single string
#' could honestly summarize it. Flagged explicitly instead
#' ("Custom/mixed basis (BASNAM) - see input file") - a genuine gap in
#' extract_basis_name() too, which would otherwise fall through to the
#' less specific "Custom ($DATA)" for a BASNAM job.
#'
#' @param file Path to a GAMESS .log file.
#' @return A single character string.
#' @export
extract_level_of_theory <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)
  input_echo <- grep("^\\s*INPUT CARD>", lines, value = TRUE)

  get_kw <- function(name) {
    m <- regmatches(input_echo, regexpr(paste0(name, "\\s*=\\s*\\S+"), input_echo, ignore.case = TRUE))
    if (length(m) == 0) return(NA_character_)
    sub(".*=\\s*", "", m[1])
  }

  # ---- basis set: BASNAM checked first, since it needs to override
  # extract_basis_name()'s more generic "Custom ($DATA)" fallback.
  # Reuses extract_basis_name()'s own keyword parsing (via
  # extract_basis_block()/parse_basis_keywords(), also in
  # extract_basis.R) for the raw NDFUNC/NPFUNC values directly, rather
  # than post-processing its combined */** output string - that
  # approach couldn't distinguish "d only" from "p only" (both collapse
  # to the same single "*"), a real bug found on real data
  # (caa005bTSa.log: NPFUNC=1 with NO NDFUNC at all) that produced a
  # stray leading comma, "(,p)", instead of "(p)". ----
  has_basnam <- any(grepl("BASNAM", input_echo, ignore.case = TRUE))
  basis_label <- if (has_basnam) {
    "Custom/mixed basis (BASNAM) - see input file"
  } else {
    name <- extract_basis_name(file)
    if (is.na(name)) {
      "Basis set specified in $DATA block - not automatically extracted"
    } else {
      # strip extract_basis_name()'s own trailing */** - the d/p
      # suffix is rebuilt separately below, correctly distinguishing
      # which of the two is actually present
      base_with_diffuse <- sub("\\*+$", "", name)

      parsed <- parse_basis_keywords(extract_basis_block(lines))
      ndf <- !is.na(parsed$ndfunc) && parsed$ndfunc != 0
      npf <- !is.na(parsed$npfunc) && parsed$npfunc != 0
      letters_present <- c(if (ndf) "d", if (npf) "p")

      if (length(letters_present) > 0) {
        paste0(base_with_diffuse, "(", paste(letters_present, collapse = ","), ")")
      } else {
        base_with_diffuse
      }
    }
  }

  # ---- method - only shown explicitly when notable (correlated method
  # or DFT functional); plain RHF/UHF/ROHF stays implicit, matching the
  # convention in the real example table this was built against ----
  cctyp <- get_kw("CCTYP")
  mplevl <- get_kw("MPLEVL")
  dfttyp <- get_kw("DFTTYP")

  method <- if (!is.na(cctyp) && !identical(toupper(cctyp), "NONE")) {
    cctyp
  } else if (!is.na(mplevl) && !identical(mplevl, "0")) {
    paste0("MP", mplevl)
  } else if (!is.na(dfttyp) && !identical(toupper(dfttyp), "NONE")) {
    dfttyp
  } else {
    NA_character_
  }

  # ---- solvent ----
  solvent_label <- ""
  if (any(grepl("\\$PCM", input_echo, ignore.case = TRUE))) {
    solvnt <- get_kw("SOLVNT")
    solvent_label <- if (!is.na(solvnt)) paste0(" PCM(", tolower(solvnt), ")") else " PCM"
  }

  paste0(
    if (!is.na(method) && !is.na(basis_label)) paste0(method, "/") else "",
    basis_label, solvent_label
  )
}
