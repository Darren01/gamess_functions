#' Convert extracted NMR data into rows linking real gc:Atom individuals
#' to their computed shielding values
#'
#' Uses the real, confirmed gc: chain (nothing invented): Atom
#' --hasShielding--> CalculatedProperties --hasNmrShieldingIsotropic/
#' hasNmrShieldingAnisotropy--> FloatValue. This is the same
#' reification pattern as SystemEnergies/ReactionPath elsewhere in this
#' project, not a new design.
#'
#' Requires the Atom individuals for this experiment to already exist -
#' i.e. geometry_to_atoms() must already have been run for the same
#' experiment/geometry that the NMR calculation used, since this reuses
#' those exact Atom IDs (ex:atom_<stem>_<atom_index>) rather than
#' creating new ones. extract_nmr()'s atom_index is assumed to use the
#' same 1-based ordering as the geometry those atoms came from - if the
#' NMR run used a different geometry/atom ordering than what
#' geometry_to_atoms() was given, this will silently link to the wrong
#' atoms, so don't assume this without checking when in doubt.
#'
#' @param nmr_data Output of extract_nmr() - a data.frame with columns
#'   atom_index, element, isotropic_shielding, anisotropy.
#' @param experiment_id The ex: ID of the experiment (e.g. "ex:exp_X") -
#'   used to derive the same stem geometry_to_atoms() used for its atom
#'   IDs.
#' @return A named list of two data.frames: nmr_rows (goes into a new
#'   nmr_template_instances.tsv - the Atom-reopening rows plus the
#'   CalculatedProperties rows), float_values (goes into the existing,
#'   shared float_value_template_instances.tsv).
#' @export
nmr_to_templates <- function(nmr_data, experiment_id) {

  required_cols <- c("atom_index", "isotropic_shielding", "anisotropy")
  if (!all(required_cols %in% names(nmr_data))) {
    stop("nmr_data must have columns: ", paste(required_cols, collapse = ", "))
  }
  if (nrow(nmr_data) == 0) {
    stop("nmr_data has no rows - nothing to convert")
  }

  stem <- sub("^ex:exp_", "", experiment_id)

  atom_ids <- paste0("ex:atom_", stem, "_", nmr_data$atom_index)
  calcprops_ids <- paste0("ex:calcprops_", stem, "_", nmr_data$atom_index)
  iso_ids <- paste0("ex:nmriso_", stem, "_", nmr_data$atom_index)
  aniso_ids <- paste0("ex:nmraniso_", stem, "_", nmr_data$atom_index)

  # ---- 1. reopen each atom, link to its CalculatedProperties ----
  atom_rows <- data.frame(
    ID = atom_ids,
    Label = "",
    Type = "gc:Atom",
    hasShielding = calcprops_ids,
    hasNmrShieldingIsotropic = "",
    hasNmrShieldingAnisotropy = "",
    stringsAsFactors = FALSE
  )

  # ---- 2. CalculatedProperties -> the two shielding FloatValues ----
  calcprops_rows <- data.frame(
    ID = calcprops_ids,
    Label = paste0("Calculated NMR properties, atom ", nmr_data$atom_index, ", ", stem),
    Type = "gc:CalculatedProperties",
    hasShielding = "",
    hasNmrShieldingIsotropic = iso_ids,
    hasNmrShieldingAnisotropy = aniso_ids,
    stringsAsFactors = FALSE
  )

  nmr_rows <- rbind(atom_rows, calcprops_rows)

  # ---- 3. the actual values (shared float_value_template) ----
  float_values <- data.frame(
    ID = c(iso_ids, aniso_ids),
    Label = c(
      paste0("NMR isotropic shielding, atom ", nmr_data$atom_index, ", ", stem),
      paste0("NMR shielding anisotropy, atom ", nmr_data$atom_index, ", ", stem)
    ),
    Type = "gc:FloatValue",
    hasFloatValue = c(nmr_data$isotropic_shielding, nmr_data$anisotropy),
    hasUnit = "gc:ppm",
    stringsAsFactors = FALSE
  )

  list(nmr_rows = nmr_rows, float_values = float_values)
}
