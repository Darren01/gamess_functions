#' Combine forward and backward IRC trajectories into one continuous
#' reaction path
#'
#' Both directions start from the same saddle point and move away from
#' it (confirmed against real matched forward/backward log pairs: both
#' begin at stotal = one STRIDE unit, e.g. 0.30000, since neither
#' trajectory file contains the saddle point itself - GAMESS doesn't
#' re-evaluate the energy there unless TSENGY=.T.). This function
#' reverses the backward run (so it runs from furthest-away back toward
#' the saddle point), assigns it negative path distance, and appends the
#' forward run with positive path distance - giving one continuous
#' reactant -> transition state -> product profile, centred on the
#' saddle point at path_distance = 0.
#'
#' Does this directly from the two native GAMESS logs via
#' extract_irc_trajectory() - no external tool (e.g. Avogadro's combined
#' .cml export) required. The saddle point itself is optional, since its
#' energy isn't in either trajectory file - it comes from the separate
#' SaddlePoint job that produced the geometry both IRC runs started from.
#'
#' @param forward_file Path to the forward-direction GAMESS IRC log.
#' @param backward_file Path to the backward-direction GAMESS IRC log.
#' @param saddle_energy Optional: the saddle point's own energy (Hartree),
#'   from a separate SaddlePoint job. If supplied, inserted at
#'   path_distance = 0 between the two runs. If NA (default), the saddle
#'   point itself is omitted - the combined path starts and ends one
#'   STRIDE away from it on each side.
#' @return A data.frame: index (1-based, reactant to product), 
#'   path_distance (negative on the backward side, positive on the
#'   forward side, 0 at the saddle point if included), energy (Hartree),
#'   source ("backward"/"saddle"/"forward"), source_point (the point
#'   number in the original trajectory, NA for the saddle point).
#' @export
combine_irc_trajectories <- function(forward_file, backward_file, saddle_energy = NA_real_) {

  forward  <- extract_irc_trajectory(forward_file)
  backward <- extract_irc_trajectory(backward_file)

  # backward side: reversed (largest stotal - furthest from saddle -
  # comes first), negative path distance
  backward_side <- backward[order(-backward$stotal), ]
  backward_side$path_distance <- -backward_side$stotal
  backward_side$source <- "backward"
  backward_side$source_point <- backward_side$point

  forward_side <- forward[order(forward$stotal), ]
  forward_side$path_distance <- forward_side$stotal
  forward_side$source <- "forward"
  forward_side$source_point <- forward_side$point

  parts <- list(backward_side[, c("path_distance", "energy", "source", "source_point")])

  if (!is.na(saddle_energy)) {
    parts <- c(parts, list(data.frame(
      path_distance = 0, energy = saddle_energy,
      source = "saddle", source_point = NA_integer_
    )))
  }

  parts <- c(parts, list(forward_side[, c("path_distance", "energy", "source", "source_point")]))

  combined <- do.call(rbind, parts)
  combined <- combined[order(combined$path_distance), ]
  combined$index <- seq_len(nrow(combined))
  rownames(combined) <- NULL

  combined[, c("index", "path_distance", "energy", "source", "source_point")]
}
