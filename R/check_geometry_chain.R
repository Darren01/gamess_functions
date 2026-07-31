#' Check a whole claimed sequence for geometry continuity
#'
#' Walks a sequence of stems in the order given (e.g. c("caa004a",
#' "caa004b", "caa004c", ...)), checking each step's .inp against the
#' immediately preceding step's .log - the continuity a filename
#' sequence like this normally implies. If that specific check fails,
#' it doesn't stop there: it also checks every earlier step in the
#' sequence, since a mismatch against the immediate predecessor doesn't
#' necessarily mean data was lost - it might just mean the real lineage
#' skips a step (e.g. restarting from an earlier good result after a
#' failed run, rather than continuing from the failure), which is a
#' different, more benign finding than a genuinely lost file.
#'
#' The `status` column distinguishes two genuinely different situations
#' that both fail the "matches claimed predecessor within tolerance"
#' check, but mean very different things - found via real testing on a
#' PES-scan-style sequence, where deliberately moving a coordinate by a
#' real, meaningful amount between steps is the normal, expected
#' behaviour, not a sign of anything wrong:
#'   - "continuous": matches the claimed predecessor within tolerance.
#'   - "large_but_continuous": doesn't match within tolerance, but
#'     nothing else in the sequence fits better either - the claimed
#'     predecessor IS still the best match, just by more than
#'     tolerance. Consistent with a deliberate, larger move (e.g. a
#'     scan step), not evidence of a lost file - the chain itself is
#'     intact.
#'   - "discontinuous": a genuinely different, earlier step fits
#'     better than the claimed predecessor. This is the real signal
#'     worth investigating - either a deliberate restart after a
#'     failed run (benign, and worth confirming against your own
#'     run_notes if you logged it as such), or a genuinely lost/
#'     overwritten file (not benign).
#'
#' @param stems Character vector of file stems, in the order their
#'   names imply (e.g. c("caa004a", "caa004b", ...)). Assumes
#'   "<stem>.inp" and "<stem>.log" exist in input_dir/output_dir
#'   respectively.
#' @param input_dir Directory containing the .inp files.
#' @param output_dir Directory containing the .log files.
#' @param tolerance Passed through to check_geometry_continuity().
#' @return A data.frame, one row per step (from the second onward):
#'   stem, claimed_predecessor, status, best_match (the stem with the
#'   smallest displacement found), best_displacement.
#' @export
check_geometry_chain <- function(stems, input_dir, output_dir, tolerance = 0.05) {

  if (length(stems) < 2) {
    stop("Need at least 2 stems to check a chain")
  }

  results <- list()

  for (i in seq(2, length(stems))) {
    stem <- stems[i]
    inp_file <- file.path(input_dir, paste0(stem, ".inp"))
    claimed_predecessor <- stems[i - 1]
    claimed_log <- file.path(output_dir, paste0(claimed_predecessor, ".log"))

    if (!file.exists(inp_file)) {
      results[[length(results) + 1]] <- data.frame(
        stem = stem, claimed_predecessor = claimed_predecessor,
        status = "error", best_match = NA_character_, best_displacement = NA,
        note = paste("input file not found:", inp_file), stringsAsFactors = FALSE
      )
      next
    }

    later_geom <- tryCatch(extract_data_block_geometry(inp_file), error = function(e) NULL)
    if (is.null(later_geom)) {
      results[[length(results) + 1]] <- data.frame(
        stem = stem, claimed_predecessor = claimed_predecessor,
        status = "error", best_match = NA_character_, best_displacement = NA,
        note = "could not parse $DATA block", stringsAsFactors = FALSE
      )
      next
    }

    claimed_result <- if (file.exists(claimed_log)) {
      tryCatch(check_geometry_continuity(later_geom, claimed_log, tolerance = tolerance),
               error = function(e) NULL)
    } else NULL

    if (!is.null(claimed_result) && isTRUE(claimed_result$consistent)) {
      results[[length(results) + 1]] <- data.frame(
        stem = stem, claimed_predecessor = claimed_predecessor,
        status = "continuous", best_match = claimed_predecessor,
        best_displacement = claimed_result$max_displacement,
        note = "", stringsAsFactors = FALSE
      )
      next
    }

    # Claimed predecessor didn't match within tolerance (or its log is
    # missing) - check every earlier step to find the actual closest.
    earlier_stems <- stems[seq_len(i - 1)]
    best_stem <- NA_character_
    best_disp <- Inf

    for (es in earlier_stems) {
      log_file <- file.path(output_dir, paste0(es, ".log"))
      if (!file.exists(log_file)) next
      r <- tryCatch(check_geometry_continuity(later_geom, log_file, tolerance = tolerance),
                    error = function(e) NULL)
      if (is.null(r) || is.na(r$max_displacement)) next
      if (r$max_displacement < best_disp) {
        best_disp <- r$max_displacement
        best_stem <- es
      }
    }

    if (is.na(best_stem)) {
      status <- "error"
      note <- "no earlier step matched within tolerance either"
    } else if (identical(best_stem, claimed_predecessor)) {
      status <- "large_but_continuous"
      note <- "claimed predecessor is still the best match, just beyond tolerance - likely a deliberate move, not a lost file"
    } else {
      status <- "discontinuous"
      note <- "a different, earlier step fits better than the claimed predecessor - worth checking against run_notes"
    }

    results[[length(results) + 1]] <- data.frame(
      stem = stem, claimed_predecessor = claimed_predecessor,
      status = status, best_match = best_stem,
      best_displacement = if (is.infinite(best_disp)) NA else best_disp,
      note = note, stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}
