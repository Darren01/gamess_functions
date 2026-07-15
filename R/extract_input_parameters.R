#' Extract $CONTRL/$STATPT/$SCF run parameters, plus basis set, from a
#' GAMESS input or output file
#'
#' Works on either a raw .inp file or a .log file (via gamess_input_utils.R
#' - source that first). Intended as the "calculation metadata" companion
#' to classify_gamess_job(): where that answers "what kind of job is this",
#' this answers "what settings was it run with" - basis, functional,
#' charge, multiplicity, convergence criteria - the kind of provenance a
#' results-template row needs to attach to a measurement.
#'
#' charge/multiplicity: GAMESS's own defaults (ICHARG=0, MULT=1) are
#' applied when the $CONTRL block was found but that specific keyword was
#' absent - a legitimate case, not a failure. If the $CONTRL block itself
#' couldn't be found at all (contrl_found = FALSE), charge/multiplicity
#' come back NA rather than silently defaulting - a fully-defaulted result
#' that looks identical to a genuine ICHARG=0/MULT=1 job was the original
#' bug here, so the two cases are now distinguishable via contrl_found.
#'
#' @param file Path to a GAMESS .inp or .log file.
#' @return A list: runtype, scftyp, dfttyp, mplevl, basis, charge,
#'   multiplicity, statpt (opttol, nstep), scf (conv, maxit), and
#'   contrl_found (logical - FALSE means nothing below charge/multiplicity
#'   should be trusted, since no $CONTRL block was matched at all).
#' @export
extract_input_parameters <- function(file) {
  lines <- strip_input_card_prefix(readLines(path.expand(file), warn = FALSE))

  contrl_block <- parse_gamess_block(get_gamess_block(lines, "CONTRL"))
  statpt_block <- parse_gamess_block(get_gamess_block(lines, "STATPT"))
  scf_block    <- parse_gamess_block(get_gamess_block(lines, "SCF"))

  contrl_found <- length(contrl_block) > 0
  if (!contrl_found) {
    warning("No $CONTRL block found in ", file, " - returning NA rather than defaults")
  }

  get_num <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) return(default)
    suppressWarnings(as.numeric(x))
  }
  get_chr <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) return(default)
    toupper(x)
  }

  runtype <- get_chr(contrl_block$runtyp)
  scftyp  <- get_chr(contrl_block$scftyp)
  dfttyp  <- get_chr(contrl_block$dfttyp)
  mplevl  <- get_num(contrl_block$mplevl, 0)

  # only apply GAMESS's real defaults if the block was actually found -
  # otherwise NA, so "found and legitimately 0/1" is distinguishable from
  # "block never matched, we know nothing"
  charge <- if (contrl_found) get_num(contrl_block$icharg, 0) else NA_real_
  mult   <- if (contrl_found) get_num(contrl_block$mult, 1)   else NA_real_

  opttol <- get_num(statpt_block$opttol)
  nstep  <- get_num(statpt_block$nstep)

  conv  <- get_num(scf_block$conv)
  maxit <- get_num(scf_block$maxit)

  # extract_basis_name() now uses the same shared block matcher, so this
  # works correctly on the same file this function was given - previously
  # this function required a .log and extract_basis_name() required a
  # .inp, so this call silently returned NA on whichever file worked for
  # everything else
  basis <- tryCatch(extract_basis_name(file), error = function(e) NA)

  list(
    runtype = runtype,
    scftyp = scftyp,
    dfttyp = dfttyp,
    mplevl = mplevl,

    basis = basis,
    charge = charge,
    multiplicity = mult,

    statpt = list(
      opttol = opttol,
      nstep = nstep
    ),

    scf = list(
      conv = conv,
      maxit = maxit
    ),

    contrl_found = contrl_found
  )
}
