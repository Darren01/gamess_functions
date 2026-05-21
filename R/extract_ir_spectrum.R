#' Extract IR spectrum from a GAMESS log file
#'
#' Parses vibrational frequencies and corresponding IR intensities
#' from a GAMESS (US) output file.
#'
#' @param file Path to GAMESS output file
#' @return A data.frame with mode, frequency, intensity, and imaginary flag
#' @export
#' 
extract_ir_spectrum <- function(file, drop_imaginary = FALSE) {
  lines <- readLines(file, warn = FALSE)
  
  # ---------------------------------------------------------
  # 1. FIND BLOCKS
  # ---------------------------------------------------------
  freq_lines <- grep("FREQUENCY:", lines, value = TRUE, ignore.case = TRUE)
  ir_lines   <- grep("IR INTENSITY:", lines, value = TRUE, ignore.case = TRUE)
  
  if (length(freq_lines) == 0 || length(ir_lines) == 0) {
    stop("No frequency or IR intensity data found")
  }
  
  # ---------------------------------------------------------
  # 2. PARSE FREQUENCIES
  # ---------------------------------------------------------
  parse_freq <- function(line) {
    m <- gregexpr("-?\\d+\\.\\d+\\s*I?", line, perl = TRUE)
    tokens <- regmatches(line, m)[[1]]
    
    vapply(tokens, function(t) {
      val <- as.numeric(gsub("I", "", t))
      if (grepl("I", t)) -abs(val) else val
    }, numeric(1))
  }
  
  freqs <- unlist(lapply(freq_lines, parse_freq), use.names = FALSE)
  
  # ---------------------------------------------------------
  # 3. PARSE INTENSITIES
  # ---------------------------------------------------------
  parse_ir <- function(line) {
    m <- gregexpr("\\d+\\.\\d+", line)
    tokens <- regmatches(line, m)[[1]]
    as.numeric(tokens)
  }
  
  intensities <- unlist(lapply(ir_lines, parse_ir), use.names = FALSE)
  
  # ---------------------------------------------------------
  # 4. ALIGN LENGTHS (important safety)
  # ---------------------------------------------------------
  n <- min(length(freqs), length(intensities))
  
  freqs <- freqs[1:n]
  intensities <- intensities[1:n]
  
  # ---------------------------------------------------------
  # 5. BUILD OUTPUT
  # ---------------------------------------------------------
  df <- data.frame(
    mode = seq_len(n),
    frequency = freqs,
    intensity = intensities,
    imaginary = freqs < 0,
    stringsAsFactors = FALSE
  )
  
  if (drop_imaginary) {
    df <- df[!df$imaginary, ]
  }
  
  return(df)
}