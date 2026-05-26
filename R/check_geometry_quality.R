#' Assess geometry quality from GAMESS vibrational analysis
#'
#' Evaluates translation/rotation modes and classifies stationary point type.
#'
#' @param file Path to GAMESS output file
#' @param tr_threshold Threshold for acceptable TR frequencies (cm^-1)
#' @return A list with structure type and quality assessment
#' @export
check_geometry_quality <- function(file, tr_threshold = 10) {
  
  diag <- extract_ir_diagnostics(file)
  
  freqs <- diag$frequencies
  
  # ---------------------------------------------------------
  # 1. CLASSIFY STATIONARY POINT
  # ---------------------------------------------------------
  n_imag <- sum(freqs < 0)
  
  structure_type <- if (n_imag == 0) {
    "minimum"
  } else if (n_imag == 1) {
    "transition_state"
  } else {
    "higher_order_saddle"
  }
  
  # ---------------------------------------------------------
  # 2. CHECK TR QUALITY
  # ---------------------------------------------------------
  max_tr <- diag$max_trans_rot_error
  
  quality <- if (max_tr < 1) {
    "excellent"
  } else if (max_tr < tr_threshold) {
    "acceptable"
  } else {
    "poor"
  }
  
  # ---------------------------------------------------------
  # 3. RETURN STRUCTURED RESULT
  # ---------------------------------------------------------
  list(
    structure_type = structure_type,
    n_imaginary = n_imag,
    max_trans_rot_error = max_tr,
    tr_quality = quality
  )
}