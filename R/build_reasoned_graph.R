#' Build a reasoned (inferred) graph from an already-built, asserted graph
#'
#' Wraps `robot merge` + `robot reason` (chained in one call, matching
#' robot's own recommended style), following validate_graph_shacl()'s
#' established system2()-based pattern for invoking an external
#' command-line tool.
#'
#' Produces a SEPARATE output file - never overwrites the asserted
#' graph. The asserted graph (from build_ontology_graph()) stays
#' exactly what SHACL validates against and what's tested throughout
#' this project; the reasoned graph is a genuinely optional, additional
#' product, for a person who wants the benefit of the materialized
#' inferences (e.g. querying `?peak a ex:ImaginaryFrequencyPeak`
#' directly, rather than `FILTER(?freq < 0)` with its xsd:decimal/
#' xsd:float gotcha) - see COMPETENCY_QUESTIONS.md and the SPARQL
#' playground for that comparison directly.
#'
#' "Asserted" vs "reasoned" (not "raw") is this project's own,
#' deliberate terminology - the standard pair used throughout the
#' semantic web/OWL world for exactly this distinction.
#'
#' Requires `robot` on your PATH (or pass the full "java -jar
#' /path/to/robot.jar" via robot_cmd) - the same tool
#' build_ontology_graph() already uses for `robot template`, just a
#' different subcommand.
#'
#' A real, honest thing found while building this: reasoning over a
#' genuinely full, real project graph (~29,000 triples, the real caa
#' graph) took about 6 minutes wall-clock with HermiT in testing, even
#' though `robot template`/`robot merge` alone are fast - this is
#' real, expected DL reasoning cost, not a hang. Your own machine may
#' be faster or slower; set timeout_sec generously the first time you
#' run this against a full graph, and expect to wait rather than
#' assume something's stuck.
#'
#' @param asserted_graph_file Path to an already-built graph (from
#'   build_ontology_graph()).
#' @param extensions_file Path to a .ttl file with this project's own
#'   DL axioms (e.g. ont_mm/dl_axioms.ttl) - kept separate from
#'   gc_core.ttl (the upstream Gainesville Core mirror) rather than
#'   modifying it directly.
#' @param output_file Path to write the reasoned graph to. Must differ
#'   from asserted_graph_file - refuses to silently overwrite it.
#' @param reasoner Which OWL reasoner robot should use (default
#'   "HermiT" - needed for the datatype facet restrictions
#'   ImaginaryFrequencyPeak uses; ELK does not support these).
#' @param axiom_generators Which kinds of inferred axiom to
#'   materialize into the output (default "ClassAssertion" - the kind
#'   that actually changes what's queryable; robot's own default
#'   omits this entirely, generating only subclass axioms instead).
#' @param robot_cmd Command to invoke robot (default "robot"; use
#'   "java -jar /path/to/robot.jar" if it's not on PATH).
#' @param timeout_sec Maximum time to wait, in seconds (default 600 -
#'   generous, given real reasoning time on a full graph; 0 means no
#'   timeout at all).
#' @return Invisibly, a list: success (TRUE/FALSE), raw_output (the
#'   full text output, for reading directly).
#' @export
build_reasoned_graph <- function(asserted_graph_file, extensions_file, output_file,
                                  reasoner = "HermiT",
                                  axiom_generators = "ClassAssertion",
                                  robot_cmd = "robot",
                                  timeout_sec = 600) {

  asserted_graph_file <- path.expand(asserted_graph_file)
  extensions_file <- path.expand(extensions_file)
  output_file <- path.expand(output_file)

  if (!file.exists(asserted_graph_file)) {
    stop("Asserted graph file not found: ", asserted_graph_file)
  }
  if (!file.exists(extensions_file)) {
    stop("DL extensions file not found: ", extensions_file)
  }
  if (normalizePath(asserted_graph_file, mustWork = FALSE) ==
      normalizePath(output_file, mustWork = FALSE)) {
    stop("output_file must differ from asserted_graph_file - refusing to ",
         "overwrite the asserted graph with the reasoned one.")
  }

  args <- c(
    "merge",
    "--input", asserted_graph_file,
    "--input", extensions_file,
    "reason",
    "--reasoner", reasoner,
    "--axiom-generators", axiom_generators,
    "--output", output_file
  )

  # robot_cmd can be a bare command ("robot") or a multi-word one
  # ("java -jar /path/to/robot.jar") when robot itself isn't on PATH -
  # a real, confirmed need: system2() requires a single executable as
  # its first argument, not a string with embedded arguments, so a
  # multi-word robot_cmd has to be split, with everything after the
  # first token prepended to args instead.
  cmd_parts <- strsplit(robot_cmd, "\\s+")[[1]]
  robot_exe <- cmd_parts[1]
  if (length(cmd_parts) > 1) {
    args <- c(cmd_parts[-1], args)
  }

  cat("Reasoning over", asserted_graph_file, "with", reasoner,
      "- this can genuinely take several minutes on a full graph...\n")

  result <- system2(robot_exe, args, stdout = TRUE, stderr = TRUE,
                     timeout = timeout_sec)
  status <- attr(result, "status")
  output <- paste(result, collapse = "\n")

  # ---- A non-zero exit status always stops. The two real failure
  # modes actually hit while building this: (1) an unknown
  # --axiom-generators name (robot's own real names are e.g.
  # "ClassAssertion", "SubClass" - not "SubClassOf" or similar
  # guesses); (2) a genuinely inconsistent ontology - found via this
  # project's own real data (a pre-existing, unrelated bug: hasIndex
  # values written as untyped literals rather than
  # xsd:nonNegativeInteger, confirmed and fixed in
  # process_reaction_path_results.R). Point directly at `robot
  # explain -M inconsistency` for case (2), since that's what actually
  # diagnosed it. ----
  if (!is.null(status) && status != 0) {
    hint <- if (grepl("inconsistent", output, ignore.case = TRUE)) {
      paste0(
        "\n\nThe ontology is inconsistent - this is a real finding about ",
        "the data or axioms, not a tool failure. Run robot explain ",
        "directly for the actual reason:\n",
        "  ", robot_cmd, " merge --input ", asserted_graph_file,
        " --input ", extensions_file,
        " explain --reasoner ", reasoner, " -M inconsistency --explanation explanation.md"
      )
    } else {
      ""
    }
    stop("robot reason failed (exit status ", status, "):\n", output, hint)
  }

  cat("Reasoned graph written to", output_file, "\n")
  invisible(list(success = TRUE, raw_output = output))
}
