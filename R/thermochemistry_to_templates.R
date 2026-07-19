#' Convert extracted thermochemistry data into rows for the energies
#' result templates
#'
#' Implements the real gc: reification chain (verified against
#' releases/2026-07-19/gc_core.ttl):
#'
#'   experiment --gc:hasResult--> SystemEnergies
#'     --gc:hasZeroPointEnergy--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:hartree
#'     --gc:hasEnthalpy--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:kiloJoules
#'     --gc:hasEntropy--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:joulePerMoleKelvin
#'     --gc:hasGibbsFreeEnergy--> FloatValue --gc:hasFloatValue--> literal ; --gc:hasUnit--> gc:kiloJoules
#'
#' Three levels, not four - unlike the frequency/intensity chain,
#' SystemEnergies holds multiple energy properties directly (no
#' per-value intermediate node like FrequencyPeak is needed), since
#' hasZeroPointEnergy/hasEnthalpy/hasEntropy/hasGibbsFreeEnergy all have
#' SystemEnergies itself as their domain.
#'
#' spectra_result and float_values row shapes are identical to (and meant
#' to be merged with) ir_spectrum_to_templates()'s output - hasResult and
#' FloatValue are both generic, reused across every result type, not
#' frequency-specific despite the filename history.
#'
#' @param thermo_df One-row output of extract_thermochemistry().
#' @param experiment_id The ex: ID of the experiment (e.g. "ex:exp_rem01b") -
#'   must already exist and be correctly typed before these rows are
#'   merged in; this function doesn't check that.
#' @param label_suffix Human-readable suffix for labels (default: derived
#'   from experiment_id by stripping "ex:exp_").
#' @return A named list of three data.frames: spectra_result (re-opens
#'   experiment_id, adds hasResult - same shape as
#'   ir_spectrum_to_templates()'s), energies (the SystemEnergies
#'   individual), float_values (up to 4 rows - only for quantities that
#'   were actually extracted, i.e. not NA).
#' @export
thermochemistry_to_templates <- function(thermo_df, experiment_id, label_suffix = NULL) {

  if (nrow(thermo_df) != 1) {
    stop("thermo_df must be a single row (output of extract_thermochemistry())")
  }
  required_cols <- c("zpe", "zpe_unit", "enthalpy", "enthalpy_unit",
                      "gibbs", "gibbs_unit", "entropy", "entropy_unit")
  if (!all(required_cols %in% names(thermo_df))) {
    stop("thermo_df must have the columns extract_thermochemistry() produces")
  }

  if (is.null(label_suffix)) {
    label_suffix <- sub("^ex:exp_", "", experiment_id)
  }

  energies_id <- paste0("ex:energies_", label_suffix)

  # map plain-English unit labels (from extract_thermochemistry(), which
  # stays ontology-agnostic on purpose) to real gc: unit IRIs - same
  # separation of concerns as ir_spectrum_to_templates() hardcoding
  # gc:cm-1/gc:debyeSquaredPerAmuAngstromSquared rather than having
  # extract_ir_spectrum() know about the ontology at all
  unit_map <- c(
    "Hartree"    = "gc:hartree",
    "kJ/mol"     = "gc:kiloJoules",
    "J/(mol K)"  = "gc:joulePerMoleKelvin"
  )

  # ---- 1. spectra_result: re-open the experiment, add hasResult ----
  # Identical shape to ir_spectrum_to_templates()'s - same reasons apply
  # (no Label re-declaration; Type=gc:MolecularComputation needed so ROBOT
  # doesn't default the bare ID to owl:Class).
  spectra_result <- data.frame(
    ID = experiment_id,
    Type = "gc:MolecularComputation",
    hasResult = energies_id,
    stringsAsFactors = FALSE
  )

  # ---- 2. energies: the SystemEnergies individual ----
  # quantities not extracted (NA) get an empty cell, not a reference to
  # a FloatValue that was never created below
  quantity_id <- function(name, value) if (is.na(value)) "" else paste0("ex:", name, "_", label_suffix)

  zpe_id      <- quantity_id("zpe",      thermo_df$zpe)
  enthalpy_id <- quantity_id("enthalpy", thermo_df$enthalpy)
  entropy_id  <- quantity_id("entropy",  thermo_df$entropy)
  gibbs_id    <- quantity_id("gibbs",    thermo_df$gibbs)

  energies <- data.frame(
    ID = energies_id,
    Label = paste("System energies", label_suffix),
    Type = "gc:SystemEnergies",
    hasZeroPointEnergy = zpe_id,
    hasEnthalpy = enthalpy_id,
    hasEntropy = entropy_id,
    hasGibbsFreeEnergy = gibbs_id,
    stringsAsFactors = FALSE
  )

  # ---- 3. float_values: only for quantities actually extracted ----
  quantities <- list(
    list(id = zpe_id,      value = thermo_df$zpe,      unit = thermo_df$zpe_unit,      label = "Zero-point energy"),
    list(id = enthalpy_id, value = thermo_df$enthalpy, unit = thermo_df$enthalpy_unit, label = "Enthalpy"),
    list(id = entropy_id,  value = thermo_df$entropy,  unit = thermo_df$entropy_unit,  label = "Entropy"),
    list(id = gibbs_id,    value = thermo_df$gibbs,    unit = thermo_df$gibbs_unit,    label = "Gibbs free energy")
  )
  quantities <- Filter(function(q) q$id != "", quantities)

  if (length(quantities) == 0) {
    float_values <- data.frame(ID = character(0), Label = character(0), Type = character(0),
                                hasFloatValue = character(0), hasUnit = character(0),
                                stringsAsFactors = FALSE)
  } else {
    float_values <- do.call(rbind, lapply(quantities, function(q) {
      gc_unit <- unit_map[[q$unit]]
      if (is.null(gc_unit)) {
        stop("No gc: unit mapping for '", q$unit, "' - add it to unit_map")
      }
      data.frame(
        ID = q$id,
        Label = paste(q$label, label_suffix),
        Type = "gc:FloatValue",
        hasFloatValue = q$value,
        hasUnit = gc_unit,
        stringsAsFactors = FALSE
      )
    }))
  }

  list(
    spectra_result = spectra_result,
    energies = energies,
    float_values = float_values
  )
}
