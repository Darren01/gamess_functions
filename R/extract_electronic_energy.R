#' Extract the final SCF electronic energy from a GAMESS log
#'
#' Parses the "FINAL <method> ENERGY IS <value> AFTER <n> ITERATIONS"
#' line GAMESS prints on SCF convergence. This is the natural extractor
#' for a genuine RUNTYP=ENERGY (SinglePoint) job, where this line
#' appears exactly once.
#'
#' It also appears once per step in multi-step jobs (e.g. every NSERCH
#' iteration of a GeometryOptimization) - if more than one is found,
#' this function warns (rather than errors) and returns the LAST one,
#' since that's usually still a meaningful answer (the converged final
#' step's energy), but flags that extract_geometry_trajectory() is
#' probably what was actually wanted for that file.
#'
#' @param file Path to a GAMESS .log file with at least one converged
#'   SCF energy.
#' @return A list: energy (Hartree), energy_unit ("Hartree"), method
#'   (the SCF/DFT method GAMESS printed, e.g. "R-WB97X-D", "RHF"),
#'   n_scf_convergences (how many FINAL ENERGY lines were found - 1 for
#'   a genuine single-point job, >1 for a multi-step job).
#' @export
extract_electronic_energy <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  energy_lines <- grep("FINAL.*ENERGY IS", lines, value = TRUE, ignore.case = TRUE)

  if (length(energy_lines) == 0) {
    stop("No 'FINAL ... ENERGY IS' line found in ", file,
         " - did the SCF converge? (GAMESS only prints this line on convergence.)")
  }

  if (length(energy_lines) > 1) {
    warning(length(energy_lines), " 'FINAL ... ENERGY IS' lines found in ", file,
            " - this looks like a multi-step job (e.g. geometry optimisation), ",
            "not a single-point energy calculation. Returning the LAST one ",
            "(the converged final step), but extract_geometry_trajectory() is ",
            "probably what you actually want for this file.")
  }

  energy_line <- energy_lines[length(energy_lines)]

  method_match <- regmatches(energy_line,
    regexec("FINAL\\s+(\\S+)\\s+ENERGY IS", energy_line, ignore.case = TRUE))[[1]]
  method <- if (length(method_match) >= 2) method_match[2] else NA_character_

  energy <- suppressWarnings(as.numeric(
    regmatches(energy_line, regexpr("-?\\d+\\.\\d+", energy_line))))

  if (length(energy) == 0 || is.na(energy)) {
    stop("Found a 'FINAL ... ENERGY IS' line in ", file,
         " but couldn't parse a numeric value from it: '", energy_line, "'")
  }

  list(
    energy = energy,
    energy_unit = "Hartree",
    method = method,
    n_scf_convergences = length(energy_lines)
  )
}
