# Query your own ont_mm ontology
#
# A starting point for querying ANY graph built with this pipeline -
# not tied to Darren01/ont_mm's own rem01/rem01a/rem01b example data.
# The one thing you need to change is GRAPH_FILE below; everything else
# runs against whatever's actually in your own graph.
#
# Prerequisites: `robot` on your PATH (test with:
# system2("robot", "--version", stdout = TRUE, stderr = TRUE) - should
# print a version number, not an error).

# ---------------------------------------------------------------------
# CHANGE THIS to point at your own built graph
# (the output_file you passed to build_ontology_graph())
# ---------------------------------------------------------------------
GRAPH_FILE <- "path/to/your/built_graph.ttl"

source("gamess_functions/R/sparql_to_file.R")   # adjust path to your gamess_functions checkout

stopifnot(file.exists(GRAPH_FILE))

# ---------------------------------------------------------------------
# 1. What experiments are in this graph, and what type is each one?
#    A good first query on any unfamiliar graph - answers "what's
#    actually in here" before asking anything more specific.
# ---------------------------------------------------------------------
cat("=== Experiments by type ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?exp ?type WHERE {
             ?exp a ?type .
             FILTER(?type IN (gc:GeometryOptimization, gc:SinglePoint,
                               gc:VibrationalAnalysis, gc:SaddlePoint, gc:IRC))
           } ORDER BY ?type ?exp"
)
print(res)

# ---------------------------------------------------------------------
# 2. Any vibrational analyses with an imaginary frequency?
#    Useful real question: which experiments' geometries might not be
#    genuine minima.
# ---------------------------------------------------------------------
cat("\n=== Peaks with negative (imaginary) frequency ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?spectrum ?freq WHERE {
             ?spectrum gc:hasFrequencyPeak ?peak .
             ?peak gc:hasFrequency ?fv .
             ?fv gc:hasFloatValue ?freq .
             FILTER(?freq < 0)
           }"
)
print(res)

# ---------------------------------------------------------------------
# 3. All thermochemistry/electronic energy results in one table
# ---------------------------------------------------------------------
cat("\n=== System energies ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?energies ?prop ?val WHERE {
             ?energies a gc:SystemEnergies .
             ?energies ?prop ?fv .
             ?fv gc:hasFloatValue ?val .
             FILTER(?prop IN (gc:hasZeroPointEnergy, gc:hasEnthalpy, gc:hasEntropy,
                               gc:hasGibbsFreeEnergy, gc:hasElectronicEnergy))
           } ORDER BY ?energies"
)
print(res)

# ---------------------------------------------------------------------
# 4. Every constraint in the graph, with its target value and unit
# ---------------------------------------------------------------------
cat("\n=== Constraints ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?constraint ?type ?target ?unit WHERE {
             ?constraint a ?type ; ex:targetValue ?target ; gc:hasUnit ?unit .
             FILTER(?type IN (ex:DistanceConstraint, ex:AngleConstraint, ex:DihedralConstraint))
           }"
)
print(res)

# ---------------------------------------------------------------------
# 5. Provenance chain: which experiment's output became which
#    experiment's input? (the rem01 -> rem01a -> rem01b pattern, or
#    whatever your own successive-refinement sequence looks like)
# ---------------------------------------------------------------------
cat("\n=== Provenance: output-to-input chains ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?earlier_exp ?file ?later_exp WHERE {
             ?file prov:wasGeneratedBy ?earlier_exp .
             ?later_exp ex:hasInputFile ?file .
           }"
)
print(res)

# ---------------------------------------------------------------------
# Your own query - adapt any of the above, or write from scratch.
# Every property/class name used above (gc:hasFrequencyPeak,
# gc:SystemEnergies, ex:DistanceConstraint, etc) is real and can be
# explored further - e.g. SELECT ?p WHERE { ?s ?p ?o } to see every
# property a specific individual actually has.
# ---------------------------------------------------------------------
