#' Compare thermochemistry/electronic energies between two experiments
#'
#' Queries an already-built graph for both experiments' energies and
#' reports the raw differences (exp_b - exp_a) - nothing more. Deriving
#' a rate constant, activation energy label, or anything requiring a
#' temperature input is deliberately a separate function's job, using
#' this one's output as its input - keeping this one honest and simple.
#'
#' Typical use: compare_energies(graph, "ex:exp_caa005b", "ex:exp_caa005bTSa")
#' for an activation energy (substrate -> transition state), or
#' compare_energies(graph, "ex:exp_caa005b", "ex:exp_caa006b") for a
#' heat of reaction (substrate -> product), depending on which
#' experiments those IDs actually correspond to in your data.
#'
#' KNOWN GAP, deliberately not addressed here: temperature is not yet
#' written into the graph at all (gc:hasSystemTemperature exists and is
#' real, but its declared domain is gc:MolecularSystem, a concept this
#' project hasn't instantiated - extending the writer to capture it
#' would mean touching the shared spectra_result row shape used by
#' multiple writers, which deserves its own careful, separate pass
#' rather than being rushed in here). Every comparison from this
#' function should currently be read as "at whatever temperature GAMESS
#' used for both runs" - check the raw logs directly if that matters
#' for a specific comparison, rather than assume it's tabulated here.
#'
#' @param graph_file Path to a graph built with build_ontology_graph().
#' @param exp_a The "before" experiment ID (e.g. "ex:exp_caa005b").
#' @param exp_b The "after" experiment ID (e.g. "ex:exp_caa005bTSa").
#' @return A data.frame: quantity, value_a, value_b, difference, unit -
#'   one row per energy quantity present for BOTH experiments. A
#'   quantity missing from either experiment is silently excluded, not
#'   reported as NA - see the printed message for what's excluded and
#'   why.
#' @export
compare_energies <- function(graph_file, exp_a, exp_b) {

  if (!file.exists(graph_file)) {
    stop("Graph file not found: ", graph_file)
  }

  get_energies <- function(exp_id) {
    sparql_query(
      graph_file = graph_file,
      query = paste0(
        "SELECT ?prop ?val ?unit WHERE {
           ", exp_id, " gc:hasResult ?energies .
           ?energies a gc:SystemEnergies .
           ?energies ?prop ?fv .
           ?fv gc:hasFloatValue ?val .
           ?fv gc:hasUnit ?unit .
           FILTER(?prop IN (gc:hasZeroPointEnergy, gc:hasEnthalpy, gc:hasEntropy,
                             gc:hasGibbsFreeEnergy, gc:hasElectronicEnergy))
         }"
      )
    )
  }

  energies_a <- get_energies(exp_a)
  energies_b <- get_energies(exp_b)

  if (nrow(energies_a) == 0) {
    stop("No SystemEnergies found for ", exp_a, " - check the experiment ID and that it has thermochemistry/energy results in this graph.")
  }
  if (nrow(energies_b) == 0) {
    stop("No SystemEnergies found for ", exp_b, " - check the experiment ID and that it has thermochemistry/energy results in this graph.")
  }

  shorten <- function(uri) sub("^.*[/#]", "", uri)
  energies_a$prop <- shorten(energies_a$prop)
  energies_b$prop <- shorten(energies_b$prop)

  common <- intersect(energies_a$prop, energies_b$prop)
  only_a <- setdiff(energies_a$prop, energies_b$prop)
  only_b <- setdiff(energies_b$prop, energies_a$prop)

  if (length(only_a) > 0) {
    cat("Note:", exp_a, "has", paste(only_a, collapse = ", "), "but", exp_b, "does not - excluded from comparison.\n")
  }
  if (length(only_b) > 0) {
    cat("Note:", exp_b, "has", paste(only_b, collapse = ", "), "but", exp_a, "does not - excluded from comparison.\n")
  }
  if (length(common) == 0) {
    stop("No energy quantities present in both experiments - nothing to compare.")
  }

  result <- do.call(rbind, lapply(common, function(p) {
    row_a <- energies_a[energies_a$prop == p, ]
    row_b <- energies_b[energies_b$prop == p, ]

    unit_a <- shorten(row_a$unit[1])
    unit_b <- shorten(row_b$unit[1])
    if (unit_a != unit_b) {
      warning(p, ": unit mismatch between ", exp_a, " (", unit_a, ") and ",
              exp_b, " (", unit_b, ") - difference NOT computed for this row, check manually.")
      return(data.frame(quantity = p, value_a = as.numeric(row_a$val[1]),
                         value_b = as.numeric(row_b$val[1]), difference = NA,
                         unit = paste0(unit_a, " vs ", unit_b, " - MISMATCH"),
                         stringsAsFactors = FALSE))
    }

    val_a <- as.numeric(row_a$val[1])
    val_b <- as.numeric(row_b$val[1])

    data.frame(quantity = p, value_a = val_a, value_b = val_b,
               difference = val_b - val_a, unit = unit_a,
               stringsAsFactors = FALSE)
  }))

  cat("Comparison:", exp_b, "minus", exp_a, "\n")
  print(result)
  invisible(result)
}
