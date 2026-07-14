# Test script for Phase 2: classify_gamess_job.R + the process_experiments.R patch
#
# Lives in gamess_functions/tests/. Adjust the two paths below to match
# your checkout locations.

gamess_functions_path <- "~/Projects/active/gamess_functions"
ont_mm_path           <- "~/Projects/active/ont_mm"

source(file.path(gamess_functions_path, "R/classify_gamess_jobs.R"))

inputs_dir <- file.path(ont_mm_path, "examples/inputs")
stopifnot(dir.exists(inputs_dir))

# ---------------------------------------------------------------------
# Test 1: classify every real .inp file, print RUNTYP/HSSEND -> job_type
# Expected: rem01 -> GeometryOptimization, rem01a -> GeometryOptimization,
#           rem01b -> VibrationalAnalysis (HSSEND=.t.)
# ---------------------------------------------------------------------
cat("=== Test 1: classify real .inp files ===\n")
for (f in list.files(inputs_dir, pattern = "\\.inp$", full.names = TRUE)) {
  res <- classify_gamess_job(f)
  cat(sprintf("%-15s RUNTYP=%-10s HSSEND=%-6s -> %s\n",
              basename(f), res$runtyp, res$hssend, res$job_type))
}
cat("Expected: rem01/rem01a = GeometryOptimization, rem01b = VibrationalAnalysis\n\n")

# ---------------------------------------------------------------------
# Test 2: batch classification
# ---------------------------------------------------------------------
cat("=== Test 2: classify_gamess_jobs() batch ===\n")
print(classify_gamess_jobs(inputs_dir))
cat("\n")

# ---------------------------------------------------------------------
# Test 3: unrecognised RUNTYP and missing RUNTYP don't crash, and warn
# ---------------------------------------------------------------------
cat("=== Test 3: edge cases ===\n")
tmp_irc <- tempfile(fileext = ".inp")
writeLines(" $CONTRL SCFTYP=RHF RUNTYP=IRC $END", tmp_irc)
res <- withCallingHandlers(
  classify_gamess_job(tmp_irc),
  warning = function(w) { cat("Got expected warning:", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
stopifnot(is.na(res$job_type))
cat("OK - unrecognised RUNTYP handled without crashing\n")

tmp_none <- tempfile(fileext = ".inp")
writeLines(" $CONTRL SCFTYP=RHF $END", tmp_none)
res2 <- withCallingHandlers(
  classify_gamess_job(tmp_none),
  warning = function(w) { cat("Got expected warning:", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
stopifnot(is.na(res2$job_type))
cat("OK - missing RUNTYP handled without crashing\n\n")

# ---------------------------------------------------------------------
# Test 4: classifying from a .log instead of .inp gives the same answer
# ---------------------------------------------------------------------
cat("=== Test 4: .log vs .inp agreement ===\n")
log_file <- file.path(ont_mm_path, "examples/outputs/rem01.log")
if (file.exists(log_file)) {
  from_log <- classify_gamess_job(log_file)
  from_inp <- classify_gamess_job(file.path(inputs_dir, "rem01.inp"))
  stopifnot(identical(from_log$job_type, from_inp$job_type))
  cat("OK - .log and .inp classification agree:", from_log$job_type, "\n\n")
} else {
  cat("Skipped - no rem01.log found at", log_file, "\n\n")
}

# ---------------------------------------------------------------------
# Test 5: run the patched process_experiments.R end-to-end, and confirm
# rem01b comes out as VibrationalAnalysis rather than GeometryOptimization
# ---------------------------------------------------------------------
cat("=== Test 5: full process_experiments.R run ===\n")
source(file.path(ont_mm_path, "scripts/build_provenance.R"))
source(file.path(ont_mm_path, "scripts/process_experiments.R"))

out_file <- tempfile(fileext = ".tsv")
process_experiments(
  template_file = file.path(ont_mm_path, "templates/experiment_template.tsv"),
  input_dir     = inputs_dir,
  data_dir      = file.path(ont_mm_path, "examples/data"),
  output_dir    = file.path(ont_mm_path, "examples/outputs"),
  output_file   = out_file
)

result <- readLines(out_file)
rem01b_line <- grep("^ex:exp_rem01b\\t", result, value = TRUE)
cat("rem01b row:\n")
cat(rem01b_line, "\n")
if (grepl("VibrationalAnalysis", rem01b_line)) {
  cat("OK - rem01b correctly classified as VibrationalAnalysis\n")
} else {
  cat("UNEXPECTED - rem01b was not classified as VibrationalAnalysis, check the patch was applied\n")
}
