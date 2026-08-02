# Query your own ont_mm ontology
#
# Three tiers, roughly in order of how much SPARQL you need to know:
#
#   Tier 1 - summarize_graph(graph_file) - zero SPARQL, one function
#            call, a real overview of what's in your data. Start here.
#   Tier 2 - the templates below - you know roughly what you want to
#            ask, copy the closest template and change the ID/filter.
#   Tier 3 - write your own - a short primer, enough to read and adapt
#            what's here, not a substitute for a real SPARQL tutorial
#            (linked at the bottom for that).
#
# Prerequisites: `robot` on your PATH (test with:
# system2("robot", "--version", stdout = TRUE, stderr = TRUE) - should
# print a version number, not an error).

# ---------------------------------------------------------------------
# CHANGE THIS to point at your own built graph
# (the output_file you passed to build_ontology_graph())
# ---------------------------------------------------------------------
GRAPH_FILE <- "path/to/your/built_graph.ttl"

source("gamess_functions/R/sparql_to_file.R")     # adjust path to your gamess_functions checkout
source("gamess_functions/R/summarize_graph.R")

stopifnot(file.exists(GRAPH_FILE))

# =======================================================================
# TIER 1 - zero SPARQL required
# =======================================================================

summarize_graph(GRAPH_FILE)

# That's genuinely it for Tier 1 - experiments, your own review notes,
# imaginary-frequency quality flags, and constraints, all in one call.
# Move to Tier 2 once you have a more specific question in mind.

# =======================================================================
# TIER 2 - templates: copy the closest one, change the ID/filter
# =======================================================================

# ---------------------------------------------------------------------
# 2a. All results for ONE specific experiment
#     CHANGE: "ex:exp_rem01b" to your own experiment's ID
#     (get real IDs from summarize_graph()'s output above)
# ---------------------------------------------------------------------
cat("=== All results for one experiment ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?predicate ?object WHERE {
             ex:exp_rem01b ?predicate ?object .
           } ORDER BY ?predicate"
)
print(res)

# ---------------------------------------------------------------------
# 2b. All thermochemistry/electronic energy results in one table,
#     WITH units - CHANGED: now fetches gc:hasUnit too, not just the
#     bare number, which the original version of this example omitted.
# ---------------------------------------------------------------------
cat("\n=== System energies ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?energies ?prop ?val ?unit WHERE {
             ?energies a gc:SystemEnergies .
             ?energies ?prop ?fv .
             ?fv gc:hasFloatValue ?val .
             ?fv gc:hasUnit ?unit .
             FILTER(?prop IN (gc:hasZeroPointEnergy, gc:hasEnthalpy, gc:hasEntropy,
                               gc:hasGibbsFreeEnergy, gc:hasElectronicEnergy))
           } ORDER BY ?energies ?prop"
)
print(res)

# ---------------------------------------------------------------------
# 2c. Provenance chain: which experiment's output became which
#     experiment's input? Nothing to change - works as-is.
# ---------------------------------------------------------------------
cat("\n=== Provenance: output-to-input chains ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?earlier_exp ?file ?later_exp WHERE {
             ?file prov:wasGeneratedBy ?earlier_exp .
             ?later_exp ex:hasInputFile ?file .
           } ORDER BY ?earlier_exp ?later_exp"
)
print(res)

# ---------------------------------------------------------------------
# 2d. Filter constraints to just one target value
#     CHANGE: 2.0 to whatever value you're actually interested in
#     Uses FILTER rather than a direct literal match in the pattern -
#     see the primer below for why that distinction matters here.
# ---------------------------------------------------------------------
cat("\n=== Constraints with a specific target value ===\n")
res <- sparql_query(
  graph_file = GRAPH_FILE,
  query = "SELECT ?constraint ?type WHERE {
             ?constraint a ?type ; ex:targetValue ?target ; gc:hasUnit gc:angstrom .
             FILTER(?target = 2.0)
             FILTER(?type IN (ex:DistanceConstraint, ex:AngleConstraint, ex:DihedralConstraint))
           } ORDER BY ?constraint"
)
print(res)

# =======================================================================
# TIER 3 - write your own: a short primer
# =======================================================================
#
# Every SPARQL query in this file has the same shape:
#
#   SELECT ?variable1 ?variable2 WHERE {
#     <subject> <predicate> ?variable1 .
#     ?variable1 <predicate2> ?variable2 .
#   }
#
# - Anything starting with "?" is a variable - SPARQL fills it in with
#   whatever matches, and returns one row per match.
# - Lines separated by " . " are separate facts that must ALL be true
#   at once (an AND) - each line's ?variable can be reused in the next
#   line to chain facts together, which is how you "walk" from one
#   thing to a related thing (e.g. Tier 2b: energies -> its properties
#   -> their actual values, three chained facts).
# - FILTER(...) narrows results after matching - IN (...) checks
#   membership in a list, < / > do numeric comparison (only on
#   properly-typed numeric literals - see the real bug this project hit
#   with this exact class of comparison, now fixed).
# - A bare number written directly in a triple pattern (e.g.
#   "ex:targetValue 2.0") is NOT the same as filtering for that value -
#   a real bug found this way: a plain "2.0" typically parses as
#   xsd:decimal, which does not exact-match real data stored as
#   xsd:float, even though they're the same number. Use
#   ?x ex:targetValue ?val . FILTER(?val = 2.0) instead of a direct
#   literal in the pattern - FILTER's numeric comparison correctly
#   works across compatible types, a direct pattern match requires
#   exact term equality, datatype included.
# - Every gc:/ex:/prov: name used above is real - to see EVERY property
#   a specific individual actually has, with no assumptions:
#
#     SELECT ?p ?o WHERE { ex:exp_rem01b ?p ?o . }
#
#   is always a safe way to explore something you don't yet know the
#   shape of.
# - SPARQL makes no guarantee about result order unless you add
#   ORDER BY - found twice in practice: results appeared in what
#   looked like a random order (neither alphabetical nor sorted by
#   value), because nothing had actually asked for either. Every
#   example in this file now has an explicit ORDER BY for exactly this
#   reason - worth adding to your own queries too, rather than
#   assuming any particular order without asking for one.
#
# This is deliberately not a full SPARQL tutorial - for that, the
# official W3C SPARQL 1.1 Query Language spec
# (https://www.w3.org/TR/sparql11-query/) is the authoritative
# reference, and there are many good general tutorials online. This
# primer is only meant to get you reading and adapting what's already
# here.
