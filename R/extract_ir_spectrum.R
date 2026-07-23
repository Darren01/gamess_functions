#' Extract IR spectrum from a GAMESS log file
#'
#' Parses vibrational frequencies and corresponding IR intensities
#' from a GAMESS (US) output file.
#'
#' @param file Path to GAMESS output file.
#' @param drop_imaginary If TRUE, remove imaginary modes from the result.
#' @return A data.frame with mode, frequency, intensity, and imaginary flag.
#' @export
extract_ir_spectrum <- function(file, drop_imaginary = FALSE) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  # ---------------------------------------------------------
  # 1. FIND BLOCKS
  # ---------------------------------------------------------
  freq_lines <- grep("FREQUENCY:", lines, value = TRUE, ignore.case = TRUE)
  ir_lines   <- grep("IR INTENSITY:", lines, value = TRUE, ignore.case = TRUE)

  if (length(freq_lines) == 0 || length(ir_lines) == 0) {
    stop("No frequency or IR intensity data found in ", file)
  }

  # ---------------------------------------------------------
  # 2. PARSE FREQUENCIES (shared parser - see gamess_input_utils.R)
  # ---------------------------------------------------------
  freqs <- parse_gamess_frequencies(freq_lines)

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

  if (length(freqs) != length(intensities)) {
    warning("Frequency count (", length(freqs), ") and intensity count (",
            length(intensities), ") don't match in ", file,
            " - truncating to the shorter of the two (", n, ")")
  }

  freqs <- freqs[seq_len(n)]
  intensities <- intensities[seq_len(n)]

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

  df
}
