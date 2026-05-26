#' Extract thermochemistry data from a GAMESS log file
#'
#' Parses zero-point energy, enthalpy, entropy, and Gibbs free energy
#' from a GAMESS (US) output file and returns a tidy data frame.
#'
#' @param file Path to GAMESS output file
#' @param units "native" (default) or "kJ"
#' @return A data.frame with thermochemical quantities
#' @export
extract_thermochemistry <- function(file, units = "native") {
  
  lines <- readLines(file, warn = FALSE)
  
  # =========================================================
  # 1. ZPE (HARTREE)
  # =========================================================
  zpe_line_idx <- grep("ZERO POINT ENERGY", lines, ignore.case = TRUE)
  
  zpe <- NA_real_
  
  if (length(zpe_line_idx) > 0) {
    line <- lines[zpe_line_idx[1] + 1]
    nums <- regmatches(line, gregexpr("-?\\d+\\.\\d+", line))[[1]]
    
    if (length(nums) > 0) {
      zpe <- as.numeric(nums[1])
    }
  }
  
  # =========================================================
  # 2. THERMOCHEMISTRY TABLE (KJ/MOL)
  # =========================================================
  kj_block_idx <- grep("KJ/MOL", lines)
  
  enthalpy <- NA_real_
  entropy  <- NA_real_
  gibbs    <- NA_real_
  
  if (length(kj_block_idx) > 0) {
    search_range <- (kj_block_idx[1] + 1):(kj_block_idx[1] + 20)
    search_range <- search_range[search_range <= length(lines)]
    
    total_line <- grep("^\\s*TOTAL", lines[search_range], value = TRUE)
    
    if (length(total_line) > 0) {
      nums <- regmatches(total_line[1], gregexpr("-?\\d+\\.\\d+", total_line[1]))[[1]]
      
      if (length(nums) >= 6) {
        enthalpy <- as.numeric(nums[2])
        gibbs    <- as.numeric(nums[3])
        entropy  <- as.numeric(nums[6])
      }
    }
  }
  
  # =========================================================
  # 3. UNIT CONVERSION
  # =========================================================
  hartree_to_kj <- 2625.5
  
  if (units == "kJ") {
    zpe <- zpe * hartree_to_kj
  }
  
  # =========================================================
  # 4. RETURN DATA FRAME
  # =========================================================
  data.frame(
    file = basename(file),
    zpe = zpe,
    enthalpy = enthalpy,
    gibbs = gibbs,
    entropy = entropy,
    units = units,
    stringsAsFactors = FALSE
  )
}