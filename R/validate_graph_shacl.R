#' Validate a built graph against a SHACL shapes file
#'
#' Thin wrapper around Jena's `shacl validate` command-line tool,
#' matching build_ontology_graph()'s own system2()-based pattern for
#' invoking an external command-line tool.
#'
#' Requires `shacl` on your PATH (part of the Apache Jena command-line
#' tools distribution - see https://jena.apache.org/download/).
#'
#' Two real bugs found via testing, both fixed here:
#'
#' 1. All paths are explicitly path.expand()-ed before being passed to
#'    system2() - "~/..." reaches the underlying shell literally,
#'    unlike file.exists() and similar, which expand it internally.
#'
#' 2. Jena's shacl script blindly trusts JAVA_HOME if it's set at all,
#'    with no check that it actually points at a working Java install -
#'    unlike robot, which doesn't reference JAVA_HOME anywhere in its
#'    own script (confirmed directly). A stale JAVA_HOME (pointing at
#'    a long-gone Java 8 install, from a source never fully tracked
#'    down - not .bashrc, /etc/environment, or any of the other usual
#'    places) made shacl fail outright.
#'
#'    Fixed by clearing JAVA_HOME for the duration of the shacl call
#'    only, via Sys.setenv()/on.exit() (restoring the original value
#'    afterwards even if the call errors), which makes shacl correctly
#'    fall back to `which java`. NOTE: an earlier version of this fix
#'    used system2()'s own `env` argument instead, on the reasoning
#'    that it should override an inherited variable for just that one
#'    subprocess call without touching the wider R session at all -
#'    that reasoning turned out to be wrong in practice, confirmed by
#'    testing on real data: the env= approach still failed, and only
#'    Sys.setenv()/on.exit() genuinely worked.
#'
#' @param graph_file Path to a built graph (e.g. from
#'   build_ontology_graph()).
#' @param shapes_file Path to a SHACL shapes .ttl file (e.g.
#'   ont_mm/shapes/gc_core_shapes.ttl).
#' @param shacl_cmd Command to invoke shacl (default "shacl").
#' @return Invisibly, a list: conforms (TRUE/FALSE), n_violations
#'   (count), raw_output (the full text report, for reading directly).
#' @export
validate_graph_shacl <- function(graph_file, shapes_file, shacl_cmd = "shacl") {

  graph_file <- path.expand(graph_file)
  shapes_file <- path.expand(shapes_file)
  shacl_cmd <- path.expand(shacl_cmd)

  if (!file.exists(graph_file)) {
    stop("Graph file not found: ", graph_file)
  }
  if (!file.exists(shapes_file)) {
    stop("Shapes file not found: ", shapes_file)
  }

  args <- c("validate", "--data", graph_file, "--shapes", shapes_file)

  # Confirmed-working fix for a stale JAVA_HOME breaking shacl's own
  # script - see docstring for why the simpler env= argument approach
  # didn't actually work.
  old_java_home <- Sys.getenv("JAVA_HOME")
  Sys.setenv(JAVA_HOME = "")
  on.exit(Sys.setenv(JAVA_HOME = old_java_home), add = TRUE)

  result <- system2(shacl_cmd, args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")

  if (!is.null(status) && status != 0 && length(result) == 0) {
    stop("shacl validate failed to run - check that '", shacl_cmd,
         "' is reachable (Apache Jena command-line tools) and that a ",
         "working Java is available via `which java`.")
  }

  output <- paste(result, collapse = "\n")
  conforms <- grepl("sh:conforms\\s+true", output)
  n_violations <- length(gregexpr("sh:Violation", output)[[1]])
  if (conforms) n_violations <- 0  # gregexpr returns 1 (not 0) on no match

  cat(if (conforms) "PASSED - no violations found\n" else
      paste0("FAILED - ", n_violations, " violation(s) found\n"))
  cat(output, "\n")

  invisible(list(conforms = conforms, n_violations = n_violations, raw_output = output))
}
