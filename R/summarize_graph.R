#' Summarize a built ontology graph - no SPARQL required
#'
#' Tier 1 of the query workflow: run this first, on any graph, to see
#' what's actually in it - experiments, your own review notes, results
#' quality signals, constraints - all without writing a single SPARQL
#' query yourself. Every query used internally here has been tested
#' against real data this session (the caa project) - not just
#' theoretical examples.
#'
#' @param graph_file Path to a graph built with build_ontology_graph().
#' @return Invisibly, a named list of the individual result data.frames
#'   (experiments, annotations, imaginary_frequencies, constraints) -
#'   printed as a readable summary as a side effect either way.
#' @export
summarize_graph <- function(graph_file) {

  if (!file.exists(graph_file)) {
    stop("Graph file not found: ", graph_file)
  }

  cat("=== Summary of", basename(graph_file), "===\n\n")

  experiments <- sparql_query(
    graph_file = graph_file,
    query = "SELECT ?exp ?type WHERE {
               ?exp a ?type .
               FILTER(?type IN (gc:GeometryOptimization, gc:SinglePoint,
                                 gc:VibrationalAnalysis, gc:SaddlePoint, gc:IRC))
             } ORDER BY ?type ?exp"
  )
  cat("Experiments:", nrow(experiments), "\n")
  if (nrow(experiments) > 0) {
    print(table(basename(as.character(experiments$type))))
  }

  annotations <- sparql_query(
    graph_file = graph_file,
    query = "SELECT ?exp ?comment WHERE {
               ?exp a gc:MolecularComputation .
               ?exp skos:editorialNote ?comment .
             } ORDER BY ?exp"
  )
  cat("\nYour own review notes:", nrow(annotations), "\n")
  if (nrow(annotations) > 0) {
    for (i in seq_len(nrow(annotations))) {
      cat(" -", basename(annotations$exp[i]), ":", annotations$comment[i], "\n")
    }
  }

  imaginary <- sparql_query(
    graph_file = graph_file,
    query = "SELECT ?spectrum ?freq WHERE {
               ?spectrum gc:hasFrequencyPeak ?peak .
               ?peak gc:hasFrequency ?fv .
               ?fv gc:hasFloatValue ?freq .
               FILTER(?freq < 0)
             } ORDER BY ?spectrum ?freq"
  )
  cat("\nImaginary (negative) frequencies found:", nrow(imaginary), "\n")
  if (nrow(imaginary) > 0) {
    cat("  (worth checking - these experiments' geometries may not be genuine minima)\n")
    print(imaginary)
  }

  constraints <- sparql_query(
    graph_file = graph_file,
    query = "SELECT ?constraint ?type ?target ?unit WHERE {
               ?constraint a ?type ; ex:targetValue ?target ; gc:hasUnit ?unit .
               FILTER(?type IN (ex:DistanceConstraint, ex:AngleConstraint, ex:DihedralConstraint))
             } ORDER BY ?constraint"
  )
  cat("\nConstraints:", nrow(constraints), "\n")

  invisible(list(
    experiments = experiments,
    annotations = annotations,
    imaginary_frequencies = imaginary,
    constraints = constraints
  ))
}
