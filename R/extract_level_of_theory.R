#' (internal) Shared parsing behind both level-of-theory functions below
#'
#' Does the real work of pulling basis set, method, and solvent
#' information out of a GAMESS log's input echo. Not exported - both
#' extract_level_of_theory() (the existing, string-returning function)
#' and extract_level_of_theory_parts() (structured, for writing into
#' the ontology graph) call this rather than duplicating the parsing
#' logic twice.
#'
#' @param file Path to a GAMESS .log file.
#' @return A list: basis_label (character), correlated_method
#'   (character or NA - only set for a genuine CCTYP/MPLEVL/DFTTYP
#'   correlated or DFT method, never for plain RHF/UHF/ROHF), scftyp
#'   (character or NA - the raw SCFTYP keyword, whatever the method),
#'   solvent (character or NA), solvation_model (character or NA -
#'   "SMD" or "PCM").
.extract_level_of_theory_parts <- function(file) {

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

  # ---- method: the genuine correlated/DFT method (NA if plain
  # RHF/UHF/ROHF), and separately, the raw SCFTYP regardless ----
  cctyp <- get_kw("CCTYP")
  mplevl <- get_kw("MPLEVL")
  dfttyp <- get_kw("DFTTYP")

  correlated_method <- if (!is.na(cctyp) && !identical(toupper(cctyp), "NONE")) {
    cctyp
  } else if (!is.na(mplevl) && !identical(mplevl, "0")) {
    paste0("MP", mplevl)
  } else if (!is.na(dfttyp) && !identical(toupper(dfttyp), "NONE")) {
    dfttyp
  } else {
    NA_character_
  }

  scftyp <- get_kw("SCFTYP")

  # ---- solvent: which solvent, and which solvation model (SMD is a
  # keyword *within* the same $PCM group, not a separate group of its
  # own - checked explicitly since this project's own data shows it's
  # a real, scientifically meaningful distinction, not a minor detail
  # (see examples/aa's plain-PCM vs SMD comparison) ----
  solvent <- NA_character_
  solvation_model <- NA_character_
  if (any(grepl("\\$PCM", input_echo, ignore.case = TRUE))) {
    solvnt <- get_kw("SOLVNT")
    solvent <- if (!is.na(solvnt)) tolower(solvnt) else NA_character_
    smd <- get_kw("SMD")
    solvation_model <- if (!is.na(smd) && grepl("\\.T\\.?", smd, ignore.case = TRUE)) "SMD" else "PCM"
  }

  list(
    basis_label = basis_label,
    correlated_method = correlated_method,
    scftyp = scftyp,
    solvent = solvent,
    solvation_model = solvation_model
  )
}

#' Extract a level-of-theory label from a GAMESS log's input echo
#'
#' Builds a human-readable string like "6-31G(d,p) PCM(water)" from the
#' real basis set, method, and $PCM keywords in the input echo, for use
#' in summary tables. Unchanged in behaviour from earlier versions -
#' plain RHF/UHF/ROHF still stays implicit here, matching the
#' convention in the real example table this was built against; see
#' extract_level_of_theory_parts() for the ontology-facing version,
#' which makes a different, deliberate choice about that.
#'
#' @param file Path to a GAMESS .log file.
#' @return A single character string.
#' @export
extract_level_of_theory <- function(file) {
  parts <- .extract_level_of_theory_parts(file)

  solvent_label <- if (!is.na(parts$solvation_model)) {
    if (!is.na(parts$solvent)) {
      paste0(" ", parts$solvation_model, "(", parts$solvent, ")")
    } else {
      paste0(" ", parts$solvation_model)
    }
  } else {
    ""
  }

  paste0(
    if (!is.na(parts$correlated_method) && !is.na(parts$basis_label)) paste0(parts$correlated_method, "/") else "",
    parts$basis_label, solvent_label
  )
}

#' Extract level-of-theory information as separate, structured fields
#'
#' The ontology-facing counterpart to extract_level_of_theory(): rather
#' than one combined display string, returns method, basis set, and
#' solvent information as separate values, ready to write into
#' distinct graph properties (gc:hasMethod, gc:hasBasisSet, this
#' project's own ex:hasSolvent/ex:hasSolvationModel).
#'
#' Deliberately makes a different choice than extract_level_of_theory()
#' for the method field: plain RHF/UHF/ROHF is recorded explicitly here
#' (via the real SCFTYP keyword) rather than left blank, since a
#' competency question like "which experiments used RHF" should be
#' answerable - leaving gc:hasMethod empty for an uncorrelated
#' calculation would make it invisible to any method-based query,
#' not merely "using the implicit default".
#'
#' @param file Path to a GAMESS .log file.
#' @return A list: method (character - the real correlated/DFT method
#'   if one was used, otherwise the raw SCFTYP), basis_set (character),
#'   solvent (character or NA), solvation_model (character or NA -
#'   "SMD" or "PCM").
#' @export
extract_level_of_theory_parts <- function(file) {
  parts <- .extract_level_of_theory_parts(file)

  method <- if (!is.na(parts$correlated_method)) {
    parts$correlated_method
  } else if (!is.na(parts$scftyp)) {
    parts$scftyp
  } else {
    NA_character_
  }

  list(
    method = method,
    basis_set = parts$basis_label,
    solvent = parts$solvent,
    solvation_model = parts$solvation_model
  )
}
