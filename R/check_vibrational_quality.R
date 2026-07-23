#' Check whether a vibrational analysis represents a converged geometry
#' or needs further refinement
#'
#' Geometry optimisation is typically iterative across escalating levels
#' of theory (e.g. molecular mechanics -> semi-empirical -> low-level QM
#' -> higher-level QM), with each step's optimised geometry becoming the
#' next step's input. max_trans_rot_error - how far GAMESS's own
#' identified translation/rotation modes are from the ideal 0 cm-1 -
#' is the signal for whether a given run's geometry is "good enough"
#' (below the working threshold, default 10 cm-1) or needs another
#' iteration.
#'
#' This function REPORTS that status - it does not decide whether to
#' write results. A "needs_refinement" result is not invalid data; it's
#' an expected, normal intermediate step in a longer optimisation chain
#' (confirmed against real data: rem01 -> rem01a -> rem01b in this
#' project's own example experiments is exactly this pattern - each
#' successive run's input file was generated from the previous run's
#' output). The eventual automated R package version of this pipeline
#' may use this to drive the next iteration automatically; for now it's
#' a flag for the person running the pipeline to act on manually.
#'
#' @param file Path to a GAMESS .log file with vibrational analysis output.
#' @param trans_rot_threshold Maximum acceptable translation/rotation
#'   mode error, in cm-1. Optional, default 10 - matches
#'   check_geometry_quality()'s own default, so both quality checks in
#'   this package agree by default rather than silently using different
#'   thresholds for the same underlying quantity.
#' @return A list: status ("converged" or "needs_refinement"),
#'   max_trans_rot_error, has_imaginary, message (human-readable).
#' @export
check_vibrational_quality <- function(file, trans_rot_threshold = 10) {

  diag <- tryCatch(
    extract_ir_diagnostics(file),
    error = function(e) {
      warning("Could not run vibrational diagnostics on ", file, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(diag)) {
    return(list(status = NA_character_, max_trans_rot_error = NA_real_,
                has_imaginary = NA, message = "Diagnostics unavailable"))
  }

  converged <- diag$max_trans_rot_error <= trans_rot_threshold

  message <- if (converged) {
    sprintf("Converged: max translation/rotation error %.2f cm-1 (threshold %.0f)",
            diag$max_trans_rot_error, trans_rot_threshold)
  } else {
    sprintf(paste(
      "NEEDS ANOTHER ITERATION: max translation/rotation error %.2f cm-1",
      "exceeds threshold %.0f. This geometry is likely an intermediate step -",
      "use its optimised geometry as the input for a further optimisation",
      "at a higher level of theory."
    ), diag$max_trans_rot_error, trans_rot_threshold)
  }

  if (isTRUE(diag$has_imaginary)) {
    message <- paste(message, "| NOTE: also has an imaginary frequency.")
  }

  list(
    status = if (converged) "converged" else "needs_refinement",
    max_trans_rot_error = diag$max_trans_rot_error,
    has_imaginary = diag$has_imaginary,
    message = message
  )
}
