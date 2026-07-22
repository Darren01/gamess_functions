#' Convert extracted constraints into rows for constraint_template.tsv
#'
#' Unlike the other result writers, constraint data all goes into one
#' template file (not split across several) - the constraint rows and
#' the experiment-linking rows share exactly the same schema already,
#' so there's no need to split them the way frequency/thermochemistry/
#' reaction-path results are.
#'
#' @param constraints_df Output of extract_constraints().
#' @param experiment_id The ex: ID of the experiment these constraints
#'   belong to (e.g. "ex:exp_rem01b").
#' @return A data.frame matching constraint_template.tsv's columns: ID,
#'   Label, Type, hasConstraint, involvesAtom1-4, targetValue, hasUnit,
#'   constraintMode, forceConstant. One constraint row plus one
#'   experiment-linking row per constraint (the linking row's Type is
#'   deliberately blank - see the comment in the code: the experiment's
#'   type is already asserted once, correctly, elsewhere, and
#'   redeclaring it here caused a real, previously-fixed bug).
#' @export
constraints_to_templates <- function(constraints_df, experiment_id) {

  empty <- data.frame(ID = character(0), Label = character(0), Type = character(0),
                       hasConstraint = character(0), involvesAtom1 = character(0),
                       involvesAtom2 = character(0), involvesAtom3 = character(0),
                       involvesAtom4 = character(0), targetValue = numeric(0),
                       hasUnit = character(0), constraintMode = character(0),
                       forceConstant = character(0), stringsAsFactors = FALSE)

  if (nrow(constraints_df) == 0) return(empty)

  unit_map <- c(distance = "gc:angstrom", angle = "gc:degree", dihedral = "gc:degree")
  type_map <- c(distance = "ex:DistanceConstraint", angle = "ex:AngleConstraint",
                dihedral = "ex:DihedralConstraint")

  make_exp_link <- function(cid) {
    data.frame(
      ID = experiment_id,
      Label = paste("Experiment", sub("^ex:exp_", "", experiment_id)),
      # Type intentionally blank - the experiment's type is already
      # asserted once, correctly, in experiment_template_instances.tsv
      # (via classify_gamess_job()). Redeclaring it here previously
      # caused a real bug: this row hardcoded ex:GeometryOptimization,
      # silently contradicting the correct type for anything that
      # wasn't one (e.g. rem01b, a VibrationalAnalysis).
      Type = "",
      hasConstraint = cid,
      involvesAtom1 = "", involvesAtom2 = "", involvesAtom3 = "", involvesAtom4 = "",
      targetValue = 0, hasUnit = "", constraintMode = "", forceConstant = "",
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  counters <- c(distance = 0L, angle = 0L, dihedral = 0L)
  abbrev <- c(distance = "d", angle = "a", dihedral = "dih")
  label_word <- c(distance = "Distance", angle = "Angle", dihedral = "Dihedral")

  stem <- sub("^ex:exp_", "", experiment_id)

  for (i in seq_len(nrow(constraints_df))) {
    r <- constraints_df[i, ]

    atom_ids <- c(
      if (!is.na(r$atom1)) paste0("ex:atom_", r$atom1) else "",
      if (!is.na(r$atom2)) paste0("ex:atom_", r$atom2) else "",
      if (!is.na(r$atom3)) paste0("ex:atom_", r$atom3) else "",
      if (!is.na(r$atom4)) paste0("ex:atom_", r$atom4) else ""
    )

    counters[[r$type]] <- counters[[r$type]] + 1L
    # ex:constraint_<stem>_<type-abbrev><n> - matches the established
    # type-prefix + shared-stem convention already used for every other
    # result container this session (spectrum_<stem>, energies_<stem>,
    # reactionpath_<stem>), rather than encoding atom numbers into the ID
    # itself (redundant with involvesAtom1-4, and collides if the same
    # atom pair is ever constrained twice with different values in one
    # file - a sequential counter can't collide that way).
    cid <- paste0("ex:constraint_", stem, "_", abbrev[[r$type]], counters[[r$type]])

    id_parts <- c(r$atom1, r$atom2, r$atom3, r$atom4)
    id_parts <- id_parts[!is.na(id_parts)]
    label <- paste0(label_word[[r$type]], " constraint ", stem)

    constraint_row <- data.frame(
      ID = cid,
      Label = label,
      Type = type_map[[r$type]],
      hasConstraint = "",
      involvesAtom1 = atom_ids[1], involvesAtom2 = atom_ids[2],
      involvesAtom3 = atom_ids[3], involvesAtom4 = atom_ids[4],
      targetValue = r$value,
      hasUnit = unit_map[[r$type]],
      constraintMode = "fixed",
      forceConstant = "",
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- constraint_row
    rows[[length(rows) + 1]] <- make_exp_link(cid)
  }

  do.call(rbind, rows)
}
