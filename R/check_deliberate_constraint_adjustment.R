#' Check a deliberate atom-pair distance adjustment between two geometries
#'
#' For the real, common case (found via testing) of manually shortening
#' the distance between two specific atoms before starting the next
#' run, without changing anything else - this checks whether that story
#' actually holds: is the displacement genuinely concentrated in just
#' the named pair, with everything else staying close to where it was?
#'
#' A confirmed pattern (large pair-distance change, small displacement
#' everywhere else) is good, positive evidence this was a deliberate,
#' correctly-executed constraint adjustment - worth recording in
#' run_notes precisely because it's real, deliberate methodology, not
#' something to leave undocumented. A pattern where OTHER atoms also
#' moved significantly is a stronger warning sign than a plain
#' check_geometry_continuity() mismatch alone - it means something
#' beyond just the intended pair changed.
#'
#' @param later_inp EITHER a path to the later run's .inp file, OR an
#'   already-extracted geometry data.frame.
#' @param earlier_log EITHER a path to the earlier run's .log file, OR
#'   an already-extracted geometry data.frame.
#' @param atom_indices Length-2 integer vector, the 1-based row indices
#'   (matching the geometry data.frame's row order) of the two atoms
#'   that were deliberately moved.
#' @param other_atom_tolerance Maximum acceptable displacement for
#'   every atom NOT in atom_indices. Default 0.05, same as
#'   check_geometry_continuity()'s default.
#' @return A list: pair_distance_earlier, pair_distance_later,
#'   pair_distance_change, other_atoms_ok (logical - TRUE if every
#'   other atom stayed within other_atom_tolerance), max_other_displacement,
#'   message.
#' @export
check_deliberate_constraint_adjustment <- function(later_inp, earlier_log, atom_indices,
                                                     other_atom_tolerance = 0.05) {

  if (length(atom_indices) != 2) {
    stop("atom_indices must be exactly 2 atom indices (the pair that was deliberately moved)")
  }

  later_geom <- if (is.data.frame(later_inp)) later_inp else extract_data_block_geometry(later_inp)
  earlier_geom <- if (is.data.frame(earlier_log)) earlier_log else extract_geometry_trajectory(earlier_log)$min_geometry

  if (nrow(later_geom) != nrow(earlier_geom)) {
    stop("Geometries have different atom counts (", nrow(later_geom), " vs ", nrow(earlier_geom),
         ") - can't meaningfully compare a specific atom pair across these.")
  }
  if (any(atom_indices < 1) || any(atom_indices > nrow(later_geom))) {
    stop("atom_indices must be between 1 and ", nrow(later_geom), " (the number of atoms found)")
  }

  euclidean_distance <- function(geom, i, j) {
    sqrt((geom$x[i] - geom$x[j])^2 + (geom$y[i] - geom$y[j])^2 + (geom$z[i] - geom$z[j])^2)
  }

  i <- atom_indices[1]; j <- atom_indices[2]
  pair_distance_earlier <- euclidean_distance(earlier_geom, i, j)
  pair_distance_later <- euclidean_distance(later_geom, i, j)
  pair_distance_change <- pair_distance_later - pair_distance_earlier

  # Per-atom displacement for every atom NOT in the specified pair
  other_idx <- setdiff(seq_len(nrow(later_geom)), atom_indices)
  other_displacements <- sqrt(
    (later_geom$x[other_idx] - earlier_geom$x[other_idx])^2 +
    (later_geom$y[other_idx] - earlier_geom$y[other_idx])^2 +
    (later_geom$z[other_idx] - earlier_geom$z[other_idx])^2
  )
  max_other_displacement <- if (length(other_displacements) > 0) max(other_displacements) else 0
  other_atoms_ok <- max_other_displacement <= other_atom_tolerance

  message <- if (other_atoms_ok) {
    paste0(
      "Consistent with a deliberate, clean constraint adjustment: atoms ", i, "-", j,
      " distance changed from ", round(pair_distance_earlier, 4), " to ", round(pair_distance_later, 4),
      " (", ifelse(pair_distance_change < 0, "shortened", "lengthened"), " by ",
      round(abs(pair_distance_change), 4), "), while every other atom stayed within ",
      other_atom_tolerance, " (max ", round(max_other_displacement, 4), ")."
    )
  } else {
    paste0(
      "Atoms ", i, "-", j, " distance changed from ", round(pair_distance_earlier, 4), " to ",
      round(pair_distance_later, 4), ", BUT other atoms also moved more than expected ",
      "(max ", round(max_other_displacement, 4), ", exceeds ", other_atom_tolerance, "). ",
      "This is NOT clean evidence of just the intended pair being adjusted - worth checking ",
      "more closely rather than assuming this is fully explained by the deliberate move."
    )
  }

  list(
    pair_distance_earlier = pair_distance_earlier,
    pair_distance_later = pair_distance_later,
    pair_distance_change = pair_distance_change,
    other_atoms_ok = other_atoms_ok,
    max_other_displacement = max_other_displacement,
    message = message
  )
}
