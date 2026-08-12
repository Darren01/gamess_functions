#' Extract the "gold standard" electronic/free energy from a high-
#' precision SinglePoint run
#'
#' A real gap found via testing: extract_electronic_energy() only ever
#' finds the uncorrelated RHF reference energy ("FINAL RHF ENERGY IS"),
#' which is WRONG for a run that layers a correlated method (e.g.
#' CCSD(T)) and/or PCM solvation on top - neither ever appears in a
#' "FINAL ... ENERGY IS" line at all.
#'
#' Deliberately fails loudly rather than silently guessing wrong, in
#' two specific situations found to be real, not hypothetical:
#'   - A correlated method was requested (CCTYP or MPLEVL set, detected
#'     from the real input echo) but no PCM block was found - this
#'     function only knows how to correctly extract a correlated,
#'     solvated result via the PCM block. A correlated, gas-phase
#'     result needs its own, separately verified extraction path.
#'   - A solvation-related input keyword other than $PCM is present -
#'     this function only knows PCM's real output format.
#'
#' `method` reports the actual highest-level method that produced the
#' returned energy (e.g. "CCSD(T)"), not the reference SCF type - a
#' real, confirmed bug: GAMESS's "FINAL ... ENERGY IS" line always
#' names the SCF reference (e.g. "RHF"), never the correlated method
#' layered on top, so a naive read of that line silently mislabels a
#' CCSD(T) result as "RHF" - misleading once other, different
#' correlated methods are used and need distinguishing from each other.
#'
#' GAMESS prints the PCM total free energy twice - once in Hartree
#' (A.U.) and once in kcal/mol on a separate line. Only the A.U. line
#' is matched, to stay consistent with this project's Hartree
#' convention throughout.
#'
#' @param file Path to a GAMESS .log file.
#' @return A list: energy (Hartree), energy_unit ("Hartree"), source
#'   ("pcm_solvated" or "rhf_reference" - which one was actually used,
#'   never ambiguous after the fact), method (the actual method that
#'   produced this energy - a correlated method's name if one was used,
#'   otherwise the reference SCF type).
#' @export
extract_pcm_free_energy <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)
  input_echo <- grep("^\\s*INPUT CARD>", lines, value = TRUE)

  # ---- what was actually requested, from the real input echo ----
  cctyp_match <- regmatches(input_echo, regexpr("CCTYP\\s*=\\s*\\S+", input_echo, ignore.case = TRUE))
  cctyp <- if (length(cctyp_match) > 0) sub(".*=\\s*", "", cctyp_match[1]) else NA_character_
  correlated_requested <- !is.na(cctyp) && !identical(toupper(cctyp), "NONE")

  mplevl <- NA_character_
  if (!correlated_requested) {
    mplevl_match <- regmatches(input_echo, regexpr("MPLEVL\\s*=\\s*\\S+", input_echo, ignore.case = TRUE))
    if (length(mplevl_match) > 0) {
      mplevl <- sub(".*=\\s*", "", mplevl_match[1])
      correlated_requested <- !identical(mplevl, "0")
    }
  }

  has_pcm_input <- any(grepl("\\$PCM", input_echo, ignore.case = TRUE))

  # A conservative, non-exhaustive list - real solvation approaches
  # beyond PCM exist, this only flags a few common ones this function
  # doesn't know how to handle, rather than claiming to recognize all
  # of them.
  other_solvation_keywords <- c("COSMO", "SMD", "\\$EFP", "SCRF")
  other_solvation_found <- other_solvation_keywords[
    vapply(other_solvation_keywords, function(kw) any(grepl(kw, input_echo, ignore.case = TRUE)), logical(1))
  ]
  if (length(other_solvation_found) > 0 && !has_pcm_input) {
    stop("Possible solvation method other than PCM detected in ", file,
         " (matched: ", paste(other_solvation_found, collapse = ", "), "). ",
         "This function only knows PCM's real output format - extend it with a ",
         "verified extraction path for this method before trusting a result here.")
  }

  # ---- the actual method that will have produced the final energy:
  # the correlated method if one was requested, otherwise the reference
  # SCF type (SCFTYP) - NOT read from the "FINAL ... ENERGY IS" line,
  # which always names the SCF reference regardless of what correlated
  # method, if any, was layered on top of it. ----
  scftyp_match <- regmatches(input_echo, regexpr("SCFTYP\\s*=\\s*\\S+", input_echo, ignore.case = TRUE))
  scftyp <- if (length(scftyp_match) > 0) sub(".*=\\s*", "", scftyp_match[1]) else NA_character_

  method <- if (correlated_requested && !is.na(cctyp) && !identical(toupper(cctyp), "NONE")) {
    cctyp
  } else if (correlated_requested && !is.na(mplevl)) {
    paste0("MP", mplevl)
  } else {
    scftyp
  }

  # ---- reference RHF/SCF energy (for the internal sanity check only) ----
  ref_lines <- grep("FINAL.*ENERGY IS", lines, value = TRUE, ignore.case = TRUE)
  reference_energy <- NA_real_
  if (length(ref_lines) > 0) {
    ref_line <- ref_lines[length(ref_lines)]
    reference_energy <- suppressWarnings(as.numeric(
      regmatches(ref_line, regexpr("-?\\d+\\.\\d+", ref_line))))
  }

  # ---- PCM total free energy, Hartree (A.U.) line specifically ----
  pcm_lines <- grep("TOTAL FREE ENERGY IN SOLVENT", lines, value = TRUE, ignore.case = TRUE)
  pcm_lines <- pcm_lines[grepl("A\\.U\\.", pcm_lines, ignore.case = TRUE)]

  if (length(pcm_lines) > 0) {
    pcm_line <- pcm_lines[length(pcm_lines)]
    pcm_energy <- suppressWarnings(as.numeric(
      regmatches(pcm_line, regexpr("-?\\d+\\.\\d+", pcm_line))))

    if (!is.na(reference_energy) && abs(pcm_energy - reference_energy) > 5) {
      warning("PCM total free energy and RHF reference energy differ by more than ",
              "5 Hartree (", round(abs(pcm_energy - reference_energy), 2), ") in ", file,
              " - larger than typical correlation+solvation corrections. Worth checking ",
              "this is genuinely the right pair of values, not a mismatch.")
    }

    return(list(energy = pcm_energy, energy_unit = "Hartree",
                source = "pcm_solvated", method = method))
  }

  # ---- no PCM block found ----
  if (correlated_requested) {
    stop("A correlated method (", method, ") was requested in ", file,
         " but no PCM block was found, and this function doesn't have a verified way ",
         "to extract a correlated, gas-phase final energy - GAMESS doesn't print one via ",
         "'FINAL ... ENERGY IS' the way it does for the uncorrelated reference. ",
         "Refusing to guess - needs its own, separately verified extraction path.")
  }

  if (is.na(reference_energy)) {
    stop("No 'TOTAL FREE ENERGY IN SOLVENT' or 'FINAL ... ENERGY IS' line found in ",
         file, " - couldn't find any usable energy value.")
  }

  list(energy = reference_energy, energy_unit = "Hartree",
       source = "rhf_reference", method = method)
}
