#' Extract thermochemistry data from a GAMESS log file
#'
#' Parses zero-point energy, enthalpy, entropy, and Gibbs free energy
#' from a GAMESS (US) output file and returns a tidy data frame.
#'
#' Returns native values only, each with its own accurate unit label -
#' a single shared "units" column (the previous design) can't correctly
#' describe these four quantities, which span three different unit
#' families: ZPE is in Hartree, enthalpy/Gibbs are in kJ/mol, and entropy
#' is in J/(mol K) - a genuinely different unit (energy per mole per
#' kelvin, not a sub-unit of kJ/mol). The previous version's "units"
#' parameter also only ever affected zpe - enthalpy/gibbs were always
#' pulled from GAMESS's KJ/MOL table regardless of what was requested,
#' so a "native" row would still silently report kJ/mol values for two
#' of its four quantities.
#'
#' @param file Path to GAMESS output file
#' @return A data.frame: file, zpe, zpe_unit, enthalpy, enthalpy_unit,
#'   gibbs, gibbs_unit, entropy, entropy_unit. Unit labels are plain,
#'   human-readable strings (e.g. "Hartree") - mapping these to gc:
#'   ontology unit IRIs is the writer function's job, not this one's
#'   (matching the separation already used in extract_ir_spectrum.R /
#'   ir_spectrum_to_templates.R).
#' @export
extract_thermochemistry <- function(file) {
  
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)
  
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
  # 2. THERMOCHEMISTRY TABLE
  # GAMESS prints E/H/G in KJ/MOL but S (entropy) in J/MOL-K -
  # a different unit family, not a sub-unit of kJ/mol. Keeping these
  # as separate fields with separate unit labels, rather than one
  # shared "units" value, is the actual fix here.
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
  # 3. RETURN DATA FRAME - native values, per-quantity unit labels
  # =========================================================
  data.frame(
    file          = basename(file),
    zpe           = zpe,
    zpe_unit      = "Hartree",
    enthalpy      = enthalpy,
    enthalpy_unit = "kJ/mol",
    gibbs         = gibbs,
    gibbs_unit    = "kJ/mol",
    entropy       = entropy,
    entropy_unit  = "J/(mol K)",
    stringsAsFactors = FALSE
  )
}