#' Plot a combined reaction path energy profile from an Avogadro-exported
#' .cml file
#'
#' KNOWN ISSUES, not fixed in this pass - deliberately deferred (see
#' project history): the unit conversion factor (627.51) converts
#' Hartree to kcal/mol, not kJ/mol as the axis label claims; this
#' function only produces a plot, no structured data is returned; and
#' it's off the automation-focused path now that
#' extract_irc_trajectory()/combine_irc_trajectories() can build the
#' same combined reaction path directly from the two native GAMESS logs,
#' without needing Avogadro's .cml export step at all. Fixed in this
#' pass: defensive file handling only (path.expand, file.exists,
#' informative error), consistent with the rest of the package.
#'
#' @param file Path to an Avogadro-exported multi-frame .cml file
#'   (combined forward+backward IRC trajectory).
#' @export
IRC_energy <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  a <- readLines(path.expand(file), warn = FALSE)
  b <- grep("\\\"Energy", a, value = TRUE)
  rm(a)

  if (length(b) == 0) {
    stop("No 'Energy' scalar tags found in ", file,
         " - is this an Avogadro-exported multi-frame .cml with energy data?")
  }

  b <- 627.51 * as.numeric(sapply(strsplit(b, split = "[<></]"), "[[", 3))
  b <- b - b[1]
  plot(b, xlab = "Geometric Structure", ylab = "Energy /KJ/mol",
       main = "Intrinsic Reaction Coordinat")
}
