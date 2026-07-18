#' Convert an extracted IR spectrum into rows for the frequency/intensity
#' result templates
#'
#' Implements the real gc: reification chain (verified against
#' releases/2026-07-16/gc_core.ttl):
#'
#'   experiment --gc:hasResult--> VibrationalSpectra
#'     --gc:hasFrequencyPeak--> FrequencyPeak (one per mode)
#'       --gc:hasFrequency--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:cm-1
#'       --gc:hasIntensity--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:debyeSquaredPerAmuAngstromSquared
#'
#' Four levels, four templates - this function produces one row set per
#' level rather than one flat row per measurement, because that's what the
#' ontology's own property domains require (hasFrequencyPeak's domain is
#' specifically VibrationalSpectra/NMRSpectra/ElectronicSpectra, not the
#' computation itself - see conversation history for how this was found).
#'
#' @param ir_df Output of extract_ir_spectrum(): data.frame with columns
#'   mode, frequency, intensity, imaginary.
#' @param experiment_id The ex: ID of the experiment this spectrum belongs
#'   to (e.g. "ex:exp_rem01b") - must already exist and be correctly typed
#'   gc:VibrationalAnalysis (via classify_gamess_job() + process_experiments.R)
#'   before these rows are merged in; this function doesn't check that.
#' @param label_suffix Human-readable suffix for labels (default: derived
#'   from experiment_id by stripping "ex:exp_").
#' @return A named list of four data.frames, one per template:
#'   spectra_result (re-opens experiment_id, adds hasResult),
#'   spectra (the VibrationalSpectra individual),
#'   peaks (one row per mode),
#'   float_values (one row per frequency AND one per intensity value -
#'   twice the row count of peaks).
#' @export
ir_spectrum_to_templates <- function(ir_df, experiment_id, label_suffix = NULL) {

  if (nrow(ir_df) == 0) {
    stop("ir_df has no rows - nothing to convert")
  }
  if (!all(c("mode", "frequency", "intensity") %in% names(ir_df))) {
    stop("ir_df must have mode, frequency, intensity columns (output of extract_ir_spectrum())")
  }

  if (is.null(label_suffix)) {
    label_suffix <- sub("^ex:exp_", "", experiment_id)
  }

  spectrum_id <- paste0("ex:spectrum_", label_suffix)
  peak_ids    <- paste0("ex:peak_", label_suffix, "_", ir_df$mode)
  freq_ids    <- paste0("ex:freqval_", label_suffix, "_", ir_df$mode)
  int_ids     <- paste0("ex:intval_", label_suffix, "_", ir_df$mode)

  # ---- 1. spectra_result: re-open the experiment, add hasResult ----
  # No Label column - the experiment already has one from
  # experiment_template_instances.tsv, and re-asserting a different one
  # would repeat the duplicate-assertion bug already fixed once in
  # process_contraints.R.
  #
  # Type IS needed, though - without any Type column, ROBOT's template
  # engine defaults every bare ID to owl:Class (confirmed: this silently
  # created an empty <http://example.org/exp_rem01b> owl:Class and
  # dropped the hasResult triple entirely, since object properties can't
  # have a Class as their subject). gc:MolecularComputation is
  # hasResult's actual declared domain - the shared ancestor of
  # GeometryOptimization/SinglePoint/VibrationalAnalysis - so asserting
  # it here is true and non-contradictory alongside whichever more
  # specific type the experiment already has, unlike the old
  # ex:GeometryOptimization bug (which asserted a wrong, competing
  # sibling type, not a true ancestor type).
  spectra_result <- data.frame(
    ID = experiment_id,
    Type = "gc:MolecularComputation",
    hasResult = spectrum_id,
    stringsAsFactors = FALSE
  )

  # ---- 2. spectra: the VibrationalSpectra individual ----
  spectra <- data.frame(
    ID = spectrum_id,
    Label = paste("Vibrational spectrum", label_suffix),
    Type = "gc:VibrationalSpectra",
    hasFrequencyPeak = paste(peak_ids, collapse = "|"),
    stringsAsFactors = FALSE
  )

  # ---- 3. peaks: one FrequencyPeak individual per mode ----
  peaks <- data.frame(
    ID = peak_ids,
    Label = paste0("Mode ", ir_df$mode, " ", label_suffix,
                    ifelse(ir_df$imaginary, " (imaginary)", "")),
    Type = "gc:FrequencyPeak",
    hasFrequency = freq_ids,
    hasIntensity = int_ids,
    stringsAsFactors = FALSE
  )

  # ---- 4. float_values: frequency AND intensity values, same template ----
  freq_values <- data.frame(
    ID = freq_ids,
    Label = paste0("Frequency mode ", ir_df$mode, " ", label_suffix),
    Type = "gc:FloatValue",
    hasFloatValue = ir_df$frequency,
    hasUnit = "gc:cm-1",
    stringsAsFactors = FALSE
  )
  int_values <- data.frame(
    ID = int_ids,
    Label = paste0("IR intensity mode ", ir_df$mode, " ", label_suffix),
    Type = "gc:FloatValue",
    hasFloatValue = ir_df$intensity,
    hasUnit = "gc:debyeSquaredPerAmuAngstromSquared",
    stringsAsFactors = FALSE
  )
  float_values <- rbind(freq_values, int_values)

  list(
    spectra_result = spectra_result,
    spectra = spectra,
    peaks = peaks,
    float_values = float_values
  )
}
