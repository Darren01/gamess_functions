#' Build species InChI rows for every minimum-energy log in a folder
#'
#' Selects GAMESS logs that are completed geometry optimisations that located an
#' equilibrium geometry (RUNTYP=OPTIMIZE and "EQUILIBRIUM GEOMETRY LOCATED"),
#' takes the minimum-energy geometry, and calls species_inchi_to_templates().
#' Transition-state, IRC, hessian-only and single-point-only logs are skipped.
#'
#' @param log_dir folder of .log files (e.g. "examples/caa/outputs").
#' @param out_dir folder to write the instances file and species_report.tsv.
#' @param instances_name file name for the molecule rows. Use "molecule_template_instances.tsv"
#'   for build_ontology_graph() to pick it up (process_gamess_directory(species = TRUE) does this).
#' @param ... passed to species_inchi_to_templates() (helper, python).
#' @return the report data.frame, invisibly.
#' @export
build_species_table <- function(log_dir, out_dir, instances_name = "molecule_species_instances.tsv", ...) {
  logs <- sort(list.files(log_dir, pattern = "\\.log$", full.names = TRUE))
  rows <- list(); reps <- list()
  for (f in logs) {
    txt <- readLines(f, warn = FALSE)
    is_opt <- any(grepl("RUNTYP=OPTIMIZE", toupper(txt[grepl("INPUT CARD>", txt)]), fixed = TRUE))
    done <- any(grepl("EQUILIBRIUM GEOMETRY LOCATED", txt, fixed = TRUE))
    stem <- sub("\\.log$", "", basename(f))
    if (!is_opt || !done) {
      reps[[stem]] <- data.frame(experiment = stem, index = NA, atoms = "", formula = "", status = "skipped",
                                 route = "", inchi = "", inchikey = "",
                                 message = if (!is_opt) "not a geometry optimisation" else "no equilibrium geometry located",
                                 stringsAsFactors = FALSE)
      next
    }
    g <- extract_geometry_trajectory(f)$min_geometry
    ch <- regmatches(paste(txt, collapse = " "), regexpr("ICHARG=(-?[0-9]+)", paste(txt, collapse = " ")))
    charge <- if (length(ch)) as.integer(sub("ICHARG=", "", ch)) else 0L
    mu <- regmatches(paste(txt, collapse = " "), regexpr("MULT=([0-9]+)", paste(txt, collapse = " ")))
    mult <- if (length(mu)) as.integer(sub("MULT=", "", mu)) else 1L
    res <- tryCatch(suppressWarnings(species_inchi_to_templates(g, paste0("ex:exp_", stem), charge, mult, ...)),
                    error = function(e) stop(conditionMessage(e), call. = FALSE))
    rows[[stem]] <- res$molecule_rows
    reps[[stem]] <- cbind(experiment = stem, res$report)
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  header <- c("ID", "Label", "Type", "hasMolecule", "hasAtom", "hasInChIString", "hasInChIKey")
  tmpl <- c("ID", "LABEL", "TYPE", "I gc:hasMolecule SPLIT=|", "I gc:hasAtom SPLIT=|",
            "AT gc:hasInChIString^^xsd:string", "AT gc:hasInChIKey^^xsd:string")
  all_rows <- do.call(rbind, rows)
  if (is.null(all_rows)) all_rows <- data.frame(matrix(character(), 0, 7, dimnames = list(NULL, header)))
  out <- file.path(out_dir, instances_name)
  writeLines(c(paste(header, collapse = "\t"), paste(tmpl, collapse = "\t")), out)
  utils::write.table(all_rows, out, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  rep <- do.call(rbind, reps)
  utils::write.table(rep, file.path(out_dir, "species_report.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(rep)
}
