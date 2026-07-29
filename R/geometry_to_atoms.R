#' Convert a geometry into gc:Molecule / gc:Atom individuals
#'
#' Takes a geometry data.frame - the same shape produced by
#' extract_geometry_trajectory()'s min_geometry (atom, charge, x, y, z) -
#' and produces real gc:Atom individuals with real gc:hasAtomCoordinateX/Y/Z
#' values, linked from a gc:Molecule individual, linked from the
#' experiment via gc:hasMolecule.
#'
#' All four terms used here (gc:Molecule, gc:Atom, gc:hasMolecule,
#' gc:hasAtom, gc:hasAtomCoordinateX/Y/Z) are real, already-declared
#' terms in the actual gc: source (confirmed directly in
#' gc07_without_imports.owl, not assumed) - this needed no new ontology
#' additions at all.
#'
#' Known limitation, deliberately not addressed here: the atom index in
#' this numbering (1, 2, 3...) is just the order atoms appear in the
#' GAMESS $DATA block - it has no necessary relationship to IUPAC atom
#' numbering for the named compound. Mapping between the two is a
#' separate, later piece of work.
#'
#' @param geometry A data.frame: atom (element symbol), charge, x, y, z -
#'   the same shape as extract_geometry_trajectory()'s min_geometry.
#' @param experiment_id The ex: ID of the experiment this geometry
#'   belongs to (e.g. "ex:exp_rem01b"). Must already exist and be
#'   correctly typed before these rows are merged in - this function
#'   doesn't check that.
#' @return A named list of three data.frames: molecule_rows (the
#'   experiment-reopening row plus the Molecule row plus every Atom
#'   row - all three share molecule_template.tsv's schema), and
#'   float_values (3 rows per atom: x/y/z coordinates, unit gc:angstrom -
#'   goes into the existing, shared float_value_template_instances.tsv).
#' @export
geometry_to_atoms <- function(geometry, experiment_id) {

  required_cols <- c("atom", "x", "y", "z")
  if (!all(required_cols %in% names(geometry))) {
    stop("geometry must have columns: ", paste(required_cols, collapse = ", "))
  }
  if (nrow(geometry) == 0) {
    stop("geometry has no rows - nothing to convert")
  }

  stem <- sub("^ex:exp_", "", experiment_id)
  molecule_id <- paste0("ex:molecule_", stem)
  n <- nrow(geometry)

  atom_ids <- paste0("ex:atom_", stem, "_", seq_len(n))
  coordx_ids <- paste0("ex:atomcoordx_", stem, "_", seq_len(n))
  coordy_ids <- paste0("ex:atomcoordy_", stem, "_", seq_len(n))
  coordz_ids <- paste0("ex:atomcoordz_", stem, "_", seq_len(n))

  # ---- 1. experiment -> molecule ----
  exp_row <- data.frame(
    ID = experiment_id,
    Label = "",
    Type = "gc:MolecularComputation",
    hasMolecule = molecule_id,
    hasAtom = "",
    hasAtomCoordinateX = "",
    hasAtomCoordinateY = "",
    hasAtomCoordinateZ = "",
    stringsAsFactors = FALSE
  )

  # ---- 2. molecule -> atoms ----
  molecule_row <- data.frame(
    ID = molecule_id,
    Label = paste("Molecule for", stem),
    Type = "gc:Molecule",
    hasMolecule = "",
    hasAtom = paste(atom_ids, collapse = "|"),
    hasAtomCoordinateX = "",
    hasAtomCoordinateY = "",
    hasAtomCoordinateZ = "",
    stringsAsFactors = FALSE
  )

  # ---- 3. atoms -> coordinates ----
  atom_rows <- data.frame(
    ID = atom_ids,
    Label = paste0("Atom ", seq_len(n), " (", geometry$atom, ") for ", stem),
    Type = "gc:Atom",
    hasMolecule = "",
    hasAtom = "",
    hasAtomCoordinateX = coordx_ids,
    hasAtomCoordinateY = coordy_ids,
    hasAtomCoordinateZ = coordz_ids,
    stringsAsFactors = FALSE
  )

  molecule_rows <- rbind(exp_row, molecule_row, atom_rows)

  # ---- 4. coordinate float values (shared float_value_template) ----
  float_values <- data.frame(
    ID = c(coordx_ids, coordy_ids, coordz_ids),
    Label = c(
      paste0("X coordinate, atom ", seq_len(n), ", ", stem),
      paste0("Y coordinate, atom ", seq_len(n), ", ", stem),
      paste0("Z coordinate, atom ", seq_len(n), ", ", stem)
    ),
    Type = "gc:FloatValue",
    hasFloatValue = c(geometry$x, geometry$y, geometry$z),
    hasUnit = "gc:angstrom",
    stringsAsFactors = FALSE
  )

  list(molecule_rows = molecule_rows, float_values = float_values)
}
