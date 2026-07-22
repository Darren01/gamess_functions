#' Convert a combined IRC path into rows for the reaction-path result
#' templates
#'
#' Implements the real gc: reification chain (verified against
#' releases/2026-07-21/gc_core.ttl):
#'
#'   forward experiment  --gc:hasResult--> ReactionPath (SAME individual)
#'   backward experiment --gc:hasResult--> ReactionPath (hasResult isn't
#'     FunctionalProperty, so both experiments can point at it)
#'     --gc:hasReactionPathPoint--> ReactionPathPoint (one per combined point)
#'       --gc:hasIndex--> literal integer
#'       --gc:hasPathEnergy--> FloatValue --gc:hasFloatValue/hasUnit--> literal/gc:hartree
#'
#' @param combined_path Output of combine_irc_trajectories().
#' @param forward_experiment_id The ex: ID of the forward IRC experiment.
#' @param backward_experiment_id The ex: ID of the backward IRC experiment.
#' @param label_suffix Human-readable suffix for labels (default: derived
#'   from forward_experiment_id by stripping "ex:exp_").
#' @return A named list of four data.frames:
#'   spectra_result (re-opens BOTH experiments, adds hasResult - same
#'     shape as ir_spectrum_to_templates()'s/thermochemistry_to_templates()'s,
#'     meant to be merged with them into the same shared file),
#'   reaction_path (the ReactionPath individual),
#'   reaction_path_points (one row per combined point),
#'   float_values (one row per point's energy - reuses the same shared
#'     FloatValue template as everything else).
#' @export
reaction_path_to_templates <- function(combined_path, forward_experiment_id,
                                        backward_experiment_id, label_suffix = NULL) {

  if (nrow(combined_path) == 0) {
    stop("combined_path has no rows - nothing to convert")
  }
  required_cols <- c("index", "path_distance", "energy", "source", "source_point")
  if (!all(required_cols %in% names(combined_path))) {
    stop("combined_path must have the columns combine_irc_trajectories() produces")
  }

  if (is.null(label_suffix)) {
    label_suffix <- sub("^ex:exp_", "", forward_experiment_id)
  }

  reactionpath_id <- paste0("ex:reactionpath_", label_suffix)
  point_ids  <- paste0("ex:pathpoint_", label_suffix, "_", combined_path$index)
  energy_ids <- paste0("ex:pathenergy_", label_suffix, "_", combined_path$index)

  # ---- 1. spectra_result: re-open BOTH experiments, both point at the
  # SAME ReactionPath. Same Type/no-Label reasoning as the other writers
  # (ROBOT defaults a bare ID with no Type to owl:Class, silently
  # dropping any object property assertion from it).
  spectra_result <- data.frame(
    ID = c(forward_experiment_id, backward_experiment_id),
    Type = "gc:MolecularComputation",
    hasResult = reactionpath_id,
    stringsAsFactors = FALSE
  )

  # ---- 2. the ReactionPath individual ----
  reaction_path <- data.frame(
    ID = reactionpath_id,
    Label = paste("Reaction path", label_suffix),
    Type = "gc:ReactionPath",
    hasReactionPathPoint = paste(point_ids, collapse = "|"),
    stringsAsFactors = FALSE
  )

  # ---- 3. one ReactionPathPoint per combined point ----
  reaction_path_points <- data.frame(
    ID = point_ids,
    Label = paste0("Path point ", combined_path$index, " (", combined_path$source, ") ", label_suffix),
    Type = "gc:ReactionPathPoint",
    hasIndex = combined_path$index,
    hasPathEnergy = energy_ids,
    stringsAsFactors = FALSE
  )

  # ---- 4. FloatValue per point energy - reuses the shared template ----
  float_values <- data.frame(
    ID = energy_ids,
    Label = paste0("Path energy ", combined_path$index, " ", label_suffix),
    Type = "gc:FloatValue",
    hasFloatValue = combined_path$energy,
    hasUnit = "gc:hartree",
    stringsAsFactors = FALSE
  )

  list(
    spectra_result = spectra_result,
    reaction_path = reaction_path,
    reaction_path_points = reaction_path_points,
    float_values = float_values
  )
}
