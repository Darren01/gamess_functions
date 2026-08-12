#' Plot a combined reaction path energy profile from a wxMacMolPlt-exported
#' .cml file
#'
#' wxMacMolPlt (Bode, B. M.; Gordon, M. S. "MacMolPlt: A Graphical User
#' Interface for GAMESS." J. Mol. Graphics Modell. 1999, 16(3), 133-138.
#' DOI: 10.1016/S1093-3263(99)00002-9; https://github.com/brettbode/wxmacmolplt)
#' is the GAMESS (US) community's standard visualization tool - used here
#' to stitch a forward and backward IRC run into one combined .cml
#' trajectory for plotting.
#'
#' KNOWN ISSUES, not fixed in this pass - deliberately deferred (see
#' project history): this function only produces a plot, no structured
#' data is returned; and it's off the automation-focused path now that
#' extract_irc_trajectory()/combine_irc_trajectories() can build the
#' same combined reaction path directly from the two native GAMESS (US)
#' logs, without needing wxMacMolPlt's .cml export step at all. Fixed in
#' this pass: the axis label incorrectly said kJ/mol - the conversion
#' factor (627.51) is actually Hartree to kcal/mol, corrected to match;
#' a typo in the plot title corrected; the docstring previously
#' misattributed the .cml export to Avogadro rather than wxMacMolPlt,
#' corrected; defensive file handling (path.expand, file.exists,
#' informative error), consistent with the rest of the package.
#'
#' @param file Path to a wxMacMolPlt-exported multi-frame .cml file
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
         " - is this a wxMacMolPlt-exported multi-frame .cml with energy data?")
  }

  b <- 627.51 * as.numeric(sapply(strsplit(b, split = "[<></]"), "[[", 3))
  b <- b - b[1]
  plot(b, xlab = "Geometric Structure", ylab = "Energy /kcal/mol",
       main = "Intrinsic Reaction Coordinate")
}
