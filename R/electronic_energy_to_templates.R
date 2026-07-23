#' Convert an extracted electronic energy into rows for the energies
#' result templates
#'
#' Same gc: reification chain as thermochemistry_to_templates()
#' (experiment --hasResult--> SystemEnergies --hasElectronicEnergy-->
#' FloatValue --hasFloatValue/hasUnit--> literal/gc:hartree), but for a
#' standalone SinglePoint job rather than a VibrationalAnalysis. These
#' don't co-occur (RUNTYP=ENERGY and RUNTYP=OPTIMIZE+HSSEND are mutually
#' exclusive), so this always creates its own SystemEnergies individual
#' with only hasElectronicEnergy populated, rather than needing to merge
#' with an existing one - unlike frequency/thermochemistry, which can
#' both apply to the same VibrationalAnalysis experiment.
#'
#' @param energy_result Output of extract_electronic_energy().
#' @param experiment_id The ex: ID of the experiment (e.g. "ex:exp_sp01") -
#'   must already exist and be correctly typed (gc:SinglePoint) before
#'   these rows are merged in; this function doesn't check that.
#' @param label_suffix Human-readable suffix for labels (default: derived
#'   from experiment_id by stripping "ex:exp_").
#' @return A named list of three data.frames: spectra_result (re-opens
#'   experiment_id, adds hasResult - same shape as every other writer's),
#'   energies (the SystemEnergies individual), float_values (one row).
#' @export
electronic_energy_to_templates <- function(energy_result, experiment_id, label_suffix = NULL) {

  if (is.null(energy_result$energy) || is.na(energy_result$energy)) {
    stop("energy_result has no energy value - nothing to convert")
  }

  if (is.null(label_suffix)) {
    label_suffix <- sub("^ex:exp_", "", experiment_id)
  }

  energies_id  <- paste0("ex:energies_", label_suffix)
  energy_value_id <- paste0("ex:electronic_energy_", label_suffix)

  # ---- 1. spectra_result: re-open the experiment, add hasResult ----
  # Same Type/no-Label reasoning as every other writer this session.
  spectra_result <- data.frame(
    ID = experiment_id,
    Type = "gc:MolecularComputation",
    hasResult = energies_id,
    stringsAsFactors = FALSE
  )

  # ---- 2. energies: the SystemEnergies individual ----
  # Only hasElectronicEnergy populated - a SinglePoint job never has
  # thermochemistry data (RUNTYP=ENERGY has no HSSEND/frequency stage).
  # Full column set matches energies_template.tsv's schema exactly (the
  # same shared template thermochemistry_to_templates() writes to).
  energies <- data.frame(
    ID = energies_id,
    Label = paste("System energies", label_suffix),
    Type = "gc:SystemEnergies",
    hasZeroPointEnergy = "",
    hasEnthalpy = "",
    hasEntropy = "",
    hasGibbsFreeEnergy = "",
    hasElectronicEnergy = energy_value_id,
    stringsAsFactors = FALSE
  )

  # ---- 3. float_value: the electronic energy itself ----
  float_values <- data.frame(
    ID = energy_value_id,
    Label = paste("Electronic energy", label_suffix,
                  if (!is.na(energy_result$method)) paste0("(", energy_result$method, ")") else ""),
    Type = "gc:FloatValue",
    hasFloatValue = energy_result$energy,
    hasUnit = "gc:hartree",
    stringsAsFactors = FALSE
  )

  list(
    spectra_result = spectra_result,
    energies = energies,
    float_values = float_values
  )
}
