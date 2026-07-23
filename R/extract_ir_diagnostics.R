#' Extract IR diagnostic information from a GAMESS log file
#'
#' Parses vibrational frequencies from a GAMESS (US) output file and
#' identifies translation/rotation modes for geometry quality checks.
#'
#' Removed in this version: is_opt_freq_job(), a helper that used to sit
#' in this file, undocumented and unused - it duplicated (with slightly
#' different logic - it also checked HSSEND) this function's own
#' validation below, and nothing in the package ever called it. If
#' something like it is needed again, build it fresh against current
#' requirements rather than resurrect dead code.
#'
#' @param file Path to GAMESS output file.
#' @return A list: frequencies (numeric vector, imaginary modes
#'   negative), trans_rot (the frequencies GAMESS itself identified as
#'   translation/rotation modes), trans_rot_modes (their indices),
#'   tr_range (c(start, end) mode numbers), max_trans_rot_error,
#'   has_imaginary (logical).
#' @export
extract_ir_diagnostics <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)
  text  <- toupper(paste(lines, collapse = " "))

  # ---- validate ----
  if (!grepl("RUNTYP\\s*=\\s*OPTIMIZE", text) ||
      !grepl("FREQUENCY", text)) {
    stop("Not an optimisation + frequency GAMESS job: ", file)
  }

  # =========================================================
  # 1. TR RANGE
  # =========================================================
  tr_line <- grep("MODES .* ARE TAKEN AS ROTATIONS AND TRANSLATIONS",
                  lines, value = TRUE, ignore.case = TRUE)

  tr_start <- 1
  tr_end   <- 6

  if (length(tr_line) > 0) {
    nums <- regmatches(tr_line, gregexpr("\\d+", tr_line))[[1]]
    if (length(nums) >= 2) {
      tr_start <- as.integer(nums[1])
      tr_end   <- as.integer(nums[2])
    }
  }

  # =========================================================
  # 2. FREQUENCIES (shared parser - see gamess_input_utils.R)
  # =========================================================
  freq_lines <- grep("FREQUENCY:", lines, value = TRUE, ignore.case = TRUE)
  freqs <- parse_gamess_frequencies(freq_lines)

  if (length(freqs) == 0) {
    stop("RUNTYP=OPTIMIZE + FREQUENCY found in ", file,
         " but no FREQUENCY: lines could be parsed")
  }

  # =========================================================
  # 3. TR MODE SELECTION
  # =========================================================
  mode_id <- seq_along(freqs)

  tr_start <- max(1, tr_start)
  tr_end   <- min(length(freqs), tr_end)

  tr_mask <- mode_id >= tr_start & mode_id <= tr_end
  trans_rot <- freqs[tr_mask]

  # =========================================================
  # 4. DIAGNOSTICS
  # =========================================================
  max_trans_rot_error <- max(abs(trans_rot), na.rm = TRUE)
  has_imaginary <- any(freqs < 0)

  list(
    frequencies = freqs,
    trans_rot = trans_rot,
    trans_rot_modes = mode_id[tr_mask],
    tr_range = c(tr_start, tr_end),
    max_trans_rot_error = max_trans_rot_error,
    has_imaginary = has_imaginary
  )
}
