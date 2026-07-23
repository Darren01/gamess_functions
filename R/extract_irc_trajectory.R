#' Extract reaction path points from a GAMESS IRC log
#'
#' Parses RUNTYP=IRC output for each "POINT N ON THE REACTION PATH" block,
#' pulling the path distance (STOTAL, mass-weighted reaction coordinate)
#' and total energy (Hartree) at each point.
#'
#' This is per-run data (one forward log, or one backward log) - it does
#' not include the saddle point itself (point 0), since GAMESS doesn't
#' separately re-evaluate the energy at the saddle point coordinates
#' within the IRC run unless TSENGY=.T. was set (the saddle point's
#' energy normally comes from the separate SaddlePoint job that produced
#' the starting geometry for this IRC run).
#'
#' Complements (does not replace) the combined-.cml workflow: the same
#' physical points exist in the forward/backward logs individually and
#' in the external tool's combined .cml. This function reads the native
#' GAMESS format directly, which is more information-rich (energy in
#' Hartree with full precision, plus path distance) and doesn't require
#' the external tool to have already run.
#'
#' @param file Path to a GAMESS RUNTYP=IRC .log file (single direction -
#'   forward or backward, not the combined .cml).
#' @return A data.frame: point (integer, 1-based), stotal (path distance
#'   in BOHR*SQRT(AMU)), energy (Hartree, absolute - not yet relative to
#'   the saddle point).
#' @export
extract_irc_trajectory <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  point_idx <- grep("POINT\\s+\\d+\\s+ON THE REACTION PATH", lines)

  if (length(point_idx) == 0) {
    stop("No IRC path points found in ", file,
         " - is this a RUNTYP=IRC log with completed path points?")
  }

  rows <- lapply(point_idx, function(i) {
    header <- lines[i]
    point_match <- regmatches(header, regexpr("POINT\\s+\\d+", header))
    point_num <- as.numeric(sub("POINT\\s+", "", point_match))

    # STOTAL and TOTAL ENERGY appear within the next few lines after the
    # point header - search a defensive window rather than assuming an
    # exact fixed offset
    window <- lines[i:min(i + 10, length(lines))]

    stotal_line <- grep("AT PATH DISTANCE STOTAL", window, value = TRUE)
    stotal <- if (length(stotal_line) > 0) {
      as.numeric(regmatches(stotal_line[1], regexpr("-?\\d+\\.\\d+", stotal_line[1])))
    } else {
      NA_real_
    }

    energy_line <- grep("TOTAL ENERGY\\s*=", window, value = TRUE)
    energy <- if (length(energy_line) > 0) {
      as.numeric(regmatches(energy_line[1], regexpr("-?\\d+\\.\\d+", energy_line[1])))
    } else {
      NA_real_
    }

    data.frame(point = point_num, stotal = stotal, energy = energy)
  })

  result <- do.call(rbind, rows)
  result <- result[order(result$point), ]
  rownames(result) <- NULL
  result
}
