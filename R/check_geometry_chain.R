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
#' @param stems Character vector of file stems, in the order their
#'   names imply (e.g. c("caa004a", "caa004b", ...)). Assumes
#'   "<stem>.inp" and "<stem>.log" exist in input_dir/output_dir
#'   respectively.
#' @param input_dir Directory containing the .inp files.
#' @param output_dir Directory containing the .log files.
#' @param tolerance Passed through to check_geometry_continuity().
#' @return A data.frame, one row per step (from the second onward):
#'   stem, claimed_predecessor, matches_claimed (logical),
#'   best_match (the stem with the smallest displacement found, which
#'   equals claimed_predecessor when matches_claimed is TRUE),
#'   best_displacement.
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
        matches_claimed = NA, best_match = NA_character_, best_displacement = NA,
        note = paste("input file not found:", inp_file), stringsAsFactors = FALSE
      )
      next
    }

    later_geom <- tryCatch(extract_data_block_geometry(inp_file), error = function(e) NULL)
    if (is.null(later_geom)) {
      results[[length(results) + 1]] <- data.frame(
        stem = stem, claimed_predecessor = claimed_predecessor,
        matches_claimed = NA, best_match = NA_character_, best_displacement = NA,
        note = "could not parse $DATA block", stringsAsFactors = FALSE
      )
      next
    }

    # Check the claimed predecessor first
    claimed_result <- if (file.exists(claimed_log)) {
      tryCatch(check_geometry_continuity(later_geom, claimed_log, tolerance = tolerance),
               error = function(e) NULL)
    } else NULL

    matches_claimed <- !is.null(claimed_result) && isTRUE(claimed_result$consistent)

    if (matches_claimed) {
      results[[length(results) + 1]] <- data.frame(
        stem = stem, claimed_predecessor = claimed_predecessor,
        matches_claimed = TRUE, best_match = claimed_predecessor,
        best_displacement = claimed_result$max_displacement,
        note = "", stringsAsFactors = FALSE
      )
      next
    }

    # Claimed predecessor didn't match (or its log is missing) - check
    # every earlier step to find the actual closest match.
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

    results[[length(results) + 1]] <- data.frame(
      stem = stem, claimed_predecessor = claimed_predecessor,
      matches_claimed = FALSE,
      best_match = best_stem,
      best_displacement = if (is.infinite(best_disp)) NA else best_disp,
      note = if (is.na(best_stem)) "no earlier step matched within tolerance either" else "",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}
