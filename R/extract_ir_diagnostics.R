#' Extract IR diagnostic information from a GAMESS log file
#'
#' Parses vibrational frequencies from a GAMESS (US) output file and
#' identifies translation/rotation modes for geometry quality checks.
#'
#' @param file Path to GAMESS output file
#' @return A list containing frequencies, TR modes, and diagnostics
#' @export

is_opt_freq_job <- function(file) {
  lines <- readLines(file, warn = FALSE)
  text <- toupper(paste(lines, collapse = " "))
  
  has_opt   <- grepl("RUNTYP\\s*=\\s*OPTIMIZE", text)
  has_hess  <- grepl("HSSEND\\s*=\\s*\\.T", text)
  has_freq  <- grepl("FREQUENCY", text)
  
  has_opt && has_hess && has_freq
}

extract_ir_diagnostics <- function(file) {
  lines <- readLines(file, warn = FALSE)
  text  <- toupper(paste(lines, collapse = " "))
  
  # ---- validate ----
  if (!grepl("RUNTYP\\s*=\\s*OPTIMIZE", text) ||
      !grepl("FREQUENCY", text)) {
    stop("Not an optimisation + frequency GAMESS job")
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
  # 2. STRICT SINGLE PARSE (NO DUPLICATION POSSIBLE)
  # =========================================================
  freq_lines <- grep("FREQUENCY:", lines, value = TRUE, ignore.case = TRUE)
  
  parse_freq <- function(x) {
    # capture number + optional I
    m <- gregexpr("-?\\d+\\.\\d+\\s*I?", x, perl = TRUE)
    tok <- regmatches(x, m)[[1]]
    
    vapply(tok, function(t) {
      num <- as.numeric(gsub("I", "", t))
      if (grepl("I", t)) -abs(num) else num
    }, numeric(1))
  }
  
  freqs <- unlist(lapply(freq_lines, parse_freq), use.names = FALSE)
  
  # ---- HARD GUARANTEE: only numeric vector survives ----
  freqs <- as.numeric(freqs)
  
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
  
  # =========================================================
  # 5. OUTPUT (SINGLE REPRESENTATION ONLY)
  # =========================================================
  trans_rot_modes <- which(seq_along(freqs) >= tr_start & seq_along(freqs) <= tr_end)
  
  list(
    frequencies = freqs,
    trans_rot = trans_rot,
    trans_rot_modes = trans_rot_modes,
    tr_range = c(tr_start, tr_end),
    max_trans_rot_error = max_trans_rot_error,
    has_imaginary = has_imaginary
  )
}
