#' Species identifiers (InChI / InChIKey) for a minimum-energy geometry
#'
#' Splits a geometry into its species (for example chloroacetaldehyde plus two
#' waters) and gives each species a gc:Molecule node carrying gc:hasInChIString
#' and gc:hasInChIKey, linked to the atoms that belong to it.
#'
#' The rule of this function is that a blank identifier is better than a
#' misleading one. An identifier is only written when it can be shown to
#' describe the molecule in the geometry. In particular:
#'   * a SMILES or InChI that leaves a stereocentre or a double bond undefined
#'     is REFUSED, not warned about - a standard InChI made from it would
#'     describe a mixture of stereoisomers, not your molecule;
#'   * a supplied InChI/InChIKey/SMILES that does not match the geometry
#'     (formula, connectivity, stereochemistry, key) is REFUSED;
#'   * if anything you supply is refused, nothing at all is written for that
#'     system until it is fixed (a generated identifier never silently
#'     replaces a wrong one of yours);
#'   * a species whose bonds cannot be perceived cleanly (a proton in transit,
#'     a zwitterion, radicals, open-shell systems) gets no node at all.
#'
#' Scope: minima and isolated species only. Do not use it on transition-state
#' or IRC geometries. SMILES is accepted as an input to derive an InChI but is
#' never stored in the graph.
#'
#' Needs Python with RDKit (conda install -c conda-forge rdkit). Without it the
#' function stops: nothing can be verified, so nothing is written.
#'
#' Atom ids follow geometry_to_atoms() (ex:atom_<stem>_<n>), so run that first
#' for the same experiment.
#'
#' @param geometry data.frame with columns atom, x, y, z (angstrom), in $DATA order.
#' @param experiment_id e.g. "ex:exp_caa008-aldehyde-bare".
#' @param charge,mult total charge and spin multiplicity of the system.
#' @param supplied NULL (generate everything from the geometry) or a data.frame
#'   with any of the columns inchi, inchikey, smiles - one row per species TYPE
#'   (a row for water covers every water in the system). Species without a row
#'   are generated from the geometry.
#' @param helper path to inchi_species.py.
#' @param python the Python executable that has RDKit.
#' @return list(molecule_rows, report, ok). molecule_rows follows
#'   molecule_template.tsv (with the two InChI columns); report has one row per
#'   species with status and the reason for any refusal.
#' @export
species_inchi_to_templates <- function(geometry, experiment_id, charge = 0, mult = 1,
                                       supplied = NULL,
                                       helper = getOption("gamess.inchi_helper", "python/inchi_species.py"),
                                       python = "python3") {

  if (!all(c("atom", "x", "y", "z") %in% names(geometry))) {
    stop("geometry must have columns: atom, x, y, z")
  }
  if (!file.exists(helper)) stop("helper script not found: ", helper)
  have <- suppressWarnings(system2(python, c("-c", shQuote("import rdkit")), stdout = FALSE, stderr = FALSE))
  if (!identical(have, 0L)) {
    stop("Python with RDKit is not available ('", python, "'). Nothing can be verified, so nothing is written.\n",
         "Install it with: conda install -c conda-forge rdkit")
  }

  # ---- quick format checks in plain R (cheap, and gives the clearest message) ----
  if (!is.null(supplied)) {
    for (col in c("inchi", "inchikey", "smiles")) if (!col %in% names(supplied)) supplied[[col]] <- ""
    supplied[] <- lapply(supplied, function(x) ifelse(is.na(x), "", trimws(as.character(x))))
    bad_i <- supplied$inchi != "" & !grepl("^InChI=1S/\\S+$", supplied$inchi)
    bad_k <- supplied$inchikey != "" & !grepl("^[A-Z]{14}-[A-Z]{8}[SN][A-Z]-[A-Z]$", supplied$inchikey)
    if (any(bad_i)) stop("Supplied InChI in row ", paste(which(bad_i), collapse = ", "),
                         " is not a standard InChI (it must start 'InChI=1S/'). Nothing written.")
    if (any(bad_k)) stop("Supplied InChIKey in row ", paste(which(bad_k), collapse = ", "),
                         " is not of the form XXXXXXXXXXXXXX-XXXXXXXXXX-X. Nothing written.")
  }

  tmp <- tempfile("species_")
  gfile <- paste0(tmp, "_geom.csv")
  utils::write.csv(data.frame(atom = geometry$atom, x = geometry$x, y = geometry$y, z = geometry$z),
                   gfile, row.names = FALSE, quote = FALSE)
  args <- c(shQuote(helper), "--geometry", shQuote(gfile), "--charge", charge, "--mult", mult,
            "--tsv", shQuote(tmp))
  if (!is.null(supplied)) {
    cfile <- paste0(tmp, "_claims.tsv")
    utils::write.table(supplied[, c("inchi", "inchikey", "smiles")], cfile, sep = "\t",
                       row.names = FALSE, quote = FALSE)
    args <- c(args, "--claims", shQuote(cfile))
  }
  system2(python, c("-I", args), stdout = FALSE, stderr = FALSE)
  sp <- utils::read.delim(paste0(tmp, ".species.tsv"), stringsAsFactors = FALSE, colClasses = "character")
  cl <- utils::read.delim(paste0(tmp, ".claims.tsv"), stringsAsFactors = FALSE, colClasses = "character")

  advice <- paste("This graph is read by other people and by your future self, so an identifier is left blank",
                  "rather than guessed. Correct the SMILES/InChI (for example add the missing stereochemistry),",
                  "then run this again.")

  if (nrow(cl) > 0 && any(cl$status != "ok")) {
    msgs <- paste0("  supplied entry ", cl$entry[cl$status != "ok"], ": ", cl$message[cl$status != "ok"])
    smsg <- paste0("  species ", sp$index[sp$status != "ok"], " (", sp$formula[sp$status != "ok"], "): ",
                   sp$message[sp$status != "ok"])
    stop("Identifiers you supplied were refused, so NOTHING was written for ", experiment_id, ":\n",
         paste(c(msgs, smsg), collapse = "\n"), "\n", advice)
  }

  stem <- sub("^ex:exp_", "", experiment_id)
  report <- sp[, c("index", "atoms", "formula", "status", "route", "inchi", "inchikey", "message")]
  refused <- report[report$status != "ok", , drop = FALSE]
  if (nrow(refused) > 0) {
    warning("Some species in ", experiment_id, " were refused and have no node:\n",
            paste0("  species ", refused$index, " (", refused$formula, "): ", refused$message, collapse = "\n"),
            "\n", advice, call. = FALSE)
  }
  good <- report[report$status == "ok", , drop = FALSE]
  empty <- data.frame(ID = character(), Label = character(), Type = character(), hasMolecule = character(),
                      hasAtom = character(), hasInChIString = character(), hasInChIKey = character(),
                      stringsAsFactors = FALSE)
  if (nrow(good) == 0) return(list(molecule_rows = empty, report = report, ok = FALSE))

  sp_ids <- paste0("ex:species_", stem, "_", good$index)
  atoms_of <- vapply(strsplit(good$atoms, ","), function(a) paste0("ex:atom_", stem, "_", a, collapse = "|"), "")
  species_rows <- data.frame(
    ID = sp_ids,
    Label = paste0("Species ", good$index, " (", good$formula, ") in ", stem),
    Type = "gc:Molecule",
    hasMolecule = "",
    hasAtom = atoms_of,
    hasInChIString = good$inchi,
    hasInChIKey = good$inchikey,
    stringsAsFactors = FALSE
  )
  system_row <- data.frame(
    ID = paste0("ex:molecule_", stem), Label = paste("Molecule for", stem), Type = "gc:Molecule",
    hasMolecule = paste(sp_ids, collapse = "|"), hasAtom = "", hasInChIString = "", hasInChIKey = "",
    stringsAsFactors = FALSE
  )
  list(molecule_rows = rbind(system_row, species_rows), report = report, ok = nrow(refused) == 0)
}
