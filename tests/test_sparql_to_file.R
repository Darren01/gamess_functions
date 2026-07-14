# Test script for sparql_to_file.R
#
# Lives in gamess_functions/tests/. Uses absolute paths throughout so it
# can be run from any working directory - adjust the two paths below to
# match your checkout locations.
#
# Prerequisites:
#   - `robot` on PATH (test with: system2("robot", "--version"))
#     If it's not on PATH, set robot_cmd below, e.g.
#     robot_cmd <- "java -jar /path/to/robot.jar"

gamess_functions_path <- "~/Projects/active/gamess_functions"
ont_mm_path           <- "~/Projects/active/ont_mm"

source(file.path(gamess_functions_path, "R/sparql_to_file.R"))

# Note: builds/gc_core.ttl is schema-only (no instance data) as currently
# built - see the open question about whether it should ever carry
# individuals. Use the populated example graph until that's settled.
#
# gc_core_full_*.ttl is dated per release - always pick the most recent
# one rather than hardcoding a date that will go stale at the next rebuild.
graph_candidates <- list.files(file.path(ont_mm_path, "examples/ont"),
                                pattern = "^gc_core_full_.*\\.ttl$",
                                full.names = TRUE)
graph <- graph_candidates[order(graph_candidates, decreasing = TRUE)][1]
cat("Using graph:", graph, "\n")
robot_cmd <- "robot"              # adjust if not on PATH

stopifnot(file.exists(graph))

# ---------------------------------------------------------------------
# Test 1: does robot run at all?
# ---------------------------------------------------------------------
cat("=== Test 1: robot availability ===\n")
v <- tryCatch(
  system2(strsplit(robot_cmd, "\\s+")[[1]][1], "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NA
)
print(v)
cat("\n")

# ---------------------------------------------------------------------
# Test 2: the exact query from ont_mm/examples/README.md
# Expected: 3 rows (rem01_dat, rem01_log, rem01a_inp)
# ---------------------------------------------------------------------
cat("=== Test 2: known query, known answer ===\n")
res <- sparql_query(
  graph_file = graph,
  query = "SELECT ?output WHERE {
             ?exp ex:hasInputFile ex:file_rem01_inp .
             ?output prov:wasGeneratedBy ?exp .
           }",
  robot_cmd = robot_cmd
)
print(res)
cat("Expected 3 rows (file_rem01_dat, file_rem01_log, file_rem01a_inp)\n\n")

# ---------------------------------------------------------------------
# Test 3: pull every fileURL out of the real graph and resolve them
# On your own machine these SHOULD mostly resolve (unlike my sandbox test,
# where none of your paths existed). Anything that doesn't resolve here
# is either a genuinely moved/missing file, or a graph path worth fixing.
# ---------------------------------------------------------------------
cat("=== Test 3: resolve every fileURL in the graph ===\n")
urls <- sparql_query(
  graph_file = graph,
  query = "SELECT ?url WHERE { ?f ex:fileURL ?url . }",
  robot_cmd = robot_cmd
)
br <- batch_resolve(urls$url)
print(br)
cat(sum(br$exists, na.rm = TRUE), "of", nrow(br), "resolved on this machine\n\n")

# ---------------------------------------------------------------------
# Test 4: full chain - query, resolve, hand straight to an extractor
# Only run this on a row where exists == TRUE from Test 3.
# ---------------------------------------------------------------------
cat("=== Test 4: full chain into an existing extractor ===\n")
ok <- br[which(br$exists), ]
if (nrow(ok) > 0) {
  source(file.path(gamess_functions_path, "R/extract_geometry_trajectory.R"))
  target <- ok$path[grepl("\\.log$", ok$path)][1]
  if (!is.na(target)) {
    traj <- extract_geometry_trajectory(target)
    cat("File:", target, "\n")
    cat("n_geometries:", traj$n_geometries, "\n")
    cat("min_energy:", traj$min_energy, "at step", traj$min_step, "\n")
  } else {
    cat("No .log file resolved locally - skipping.\n")
  }
} else {
  cat("Nothing resolved locally yet - fix fileURL paths in the graph, or\n")
  cat("point `graph` above at a build where they do, then re-run.\n")
}

# ---------------------------------------------------------------------
# Test 5: error handling - bad graph path, empty result
# ---------------------------------------------------------------------
cat("\n=== Test 5: error handling ===\n")
tryCatch(
  sparql_query("does_not_exist.ttl", "SELECT * WHERE { ?s ?p ?o }", robot_cmd = robot_cmd),
  error = function(e) cat("OK - caught missing-graph error:", conditionMessage(e), "\n")
)

empty <- sparql_query(
  graph_file = graph,
  query = "SELECT ?x WHERE { ?x ex:thisPredicateDefinitelyDoesNotExist ?y . }",
  robot_cmd = robot_cmd
)
cat("OK - empty result handled, nrow =", nrow(empty), "\n")
