#' Validate a built graph against a SHACL shapes file
#'
#' Thin wrapper around Jena's `shacl` command-line tool, matching
#' build_ontology_graph()'s own system2()-based pattern for invoking
#' an external command-line tool.
#'
#' Requires `shacl` (Linux/Mac) or `shacl.bat` (Windows) on your PATH
#' (part of the Apache Jena command-line tools distribution - see
#' https://jena.apache.org/download/).
#'
#' Real bugs found via testing, all fixed here:
#'
#' 1. All paths are explicitly path.expand()-ed before being passed to
#'    system2() - "~/..." reaches the underlying shell literally,
#'    unlike file.exists() and similar, which expand it internally.
#'
#' 2. A genuinely serious bug: if shacl fails to run at all (e.g. a
#'    Java version mismatch), it still prints a lot of text (a Java
#'    stack trace) - but that text is not a real SHACL report. An
#'    earlier version of this function didn't distinguish the two: a
#'    stack trace happens not to contain the literal string
#'    "sh:Violation" anywhere, and the old counting logic silently
#'    misread that absence as "exactly one violation" rather than
#'    "zero, because this isn't a report at all" - producing a real,
#'    dangerous false result ("FAILED - 1 violation(s) found") when
#'    shacl never actually validated anything. Fixed with two
#'    independent checks: a non-zero exit status always stops now,
#'    regardless of whether there was output; and separately, the
#'    output must contain "sh:ValidationReport" before any conforms/
#'    violation count is trusted at all.
#'
#' 3. Getting shacl to use a specific, compatible Java (rather than
#'    whatever's the system default) turns out to work differently on
#'    different platforms - confirmed by testing on real data, not
#'    assumed:
#'
#'    - The Linux/Mac `shacl` script explicitly checks JAVA_HOME
#'      itself (using it to build $JAVA_HOME/bin/java if set), so
#'      setting JAVA_HOME is sufficient there.
#'    - Windows' `shacl.bat` does NOT read JAVA_HOME at all - checked
#'      directly against Jena's own real source
#'      (apache/jena/blob/main/apache-jena/bat/shacl.bat): it calls
#'      bare `java` with no path prefix whatsoever, relying entirely
#'      on whatever the system PATH resolves it to. Setting JAVA_HOME
#'      alone was tested and confirmed to have no effect on Windows -
#'      the actual, real fix needed there is prepending the desired
#'      Java's bin/ folder to PATH itself.
#'
#'    Given this platform difference, java_home below sets BOTH
#'    JAVA_HOME and prepends its bin/ folder to PATH, so it works
#'    correctly regardless of which script ends up being used.
#'
#' @param graph_file Path to a built graph (e.g. from
#'   build_ontology_graph()).
#' @param shapes_file Path to a SHACL shapes .ttl file (e.g.
#'   ont_mm/shapes/gc_core_shapes.ttl).
#' @param shacl_cmd Command to invoke shacl (default "shacl" - use
#'   "shacl.bat" explicitly on Windows if the bare name doesn't
#'   resolve).
#' @param java_home Optional explicit path to a compatible Java
#'   install's home directory (e.g. one found via
#'   `check_robot_setup()`'s own scan) - use this if the default/
#'   fallback Java is too old. Check the exact required version from
#'   any "UnsupportedClassVersionError" message first (class file
#'   version 65 = Java 21, 61 = Java 17, 55 = Java 11 - matching to
#'   the wrong one just produces the same error again).
#' @return Invisibly, a list: conforms (TRUE/FALSE), n_violations
#'   (count), raw_output (the full text report, for reading directly).
#' @export
validate_graph_shacl <- function(graph_file, shapes_file, shacl_cmd = "shacl", java_home = NULL) {

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

  old_java_home <- Sys.getenv("JAVA_HOME")
  old_path <- Sys.getenv("PATH")
  on.exit({
    Sys.setenv(JAVA_HOME = old_java_home)
    Sys.setenv(PATH = old_path)
  }, add = TRUE)

  if (!is.null(java_home)) {
    java_home <- path.expand(java_home)
    Sys.setenv(JAVA_HOME = java_home)
    java_bin <- file.path(java_home, "bin")
    Sys.setenv(PATH = paste(java_bin, old_path, sep = .Platform$path.sep))
  } else {
    Sys.setenv(JAVA_HOME = "")
  }

  result <- system2(shacl_cmd, args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  output <- paste(result, collapse = "\n")

  # ---- Check 1: non-zero exit status always stops, regardless of
  # whether there was output - a real error can and does still print
  # a lot of text (e.g. a Java stack trace). ----
  if (!is.null(status) && status != 0) {
    stop(
      "shacl validate failed to run (exit status ", status, "):\n", output,
      "\n\nIf this mentions 'UnsupportedClassVersionError' or 'compiled by a ",
      "more recent version', the Java actually being used is too old for this ",
      "Jena install. Find a compatible Java (e.g. via check_robot_setup()'s ",
      "own scan) and pass its home directory via this function's java_home ",
      "argument. On Windows specifically, setting JAVA_HOME alone is NOT ",
      "enough - shacl.bat doesn't read it at all - java_home here handles ",
      "this correctly by also adjusting PATH, but a manually-set JAVA_HOME ",
      "elsewhere in your environment will not help on its own."
    )
  }

  # ---- Check 2: even with a zero/unavailable status, the output must
  # genuinely look like a SHACL report before any parsing is trusted -
  # a real, confirmed bug: it previously wasn't, and a Java crash's
  # stack trace was silently misread as "1 violation found". ----
  if (!grepl("sh:ValidationReport", output, fixed = TRUE)) {
    stop(
      "shacl ran without a clear error status, but its output doesn't look ",
      "like a real SHACL validation report (no 'sh:ValidationReport' found) - ",
      "refusing to guess at a result. Raw output was:\n", output
    )
  }

  conforms <- grepl("sh:conforms\\s+true", output)
  n_violations <- if (conforms) 0 else length(gregexpr("sh:Violation", output, fixed = TRUE)[[1]])

  cat(if (conforms) "PASSED - no violations found\n" else
      paste0("FAILED - ", n_violations, " violation(s) found\n"))
  cat(output, "\n")

  invisible(list(conforms = conforms, n_violations = n_violations, raw_output = output))
}
