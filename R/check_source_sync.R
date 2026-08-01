#' Check that ont_mm's copy of the source ontology matches GC-Ontology-Mirror
#'
#' Real bug found via testing: ont_mm/source/gnvc_improved.owl and
#' GC-Ontology-Mirror/gc07_without_imports.owl are two separate files
#' that have to be kept in sync by hand - nothing catches it when they
#' drift. A real addition (gc:ppm) sat correctly in GC-Ontology-Mirror
#' for a while without ever being copied across, so a schema rebuild
#' silently used stale source with no error at any step - the same
#' "no error, just wrong" failure mode as several other bugs this
#' project has hit.
#'
#' Run this before any schema rebuild (robot merge -> report -> query
#' fixes -> annotate), not after - the whole point is catching this
#' before time gets spent on a rebuild that turns out to be stale.
#'
#' @param mirror_file Path to GC-Ontology-Mirror/gc07_without_imports.owl.
#' @param source_file Path to ont_mm/source/gnvc_improved.owl.
#' @return Invisibly, TRUE if in sync; stops with a clear message
#'   (including the exact copy command to fix it) if not.
#' @export
check_source_sync <- function(mirror_file, source_file) {

  if (!file.exists(mirror_file)) {
    stop("GC-Ontology-Mirror source not found: ", mirror_file)
  }
  if (!file.exists(source_file)) {
    stop("ont_mm source copy not found: ", source_file,
         "\nIf this is a fresh checkout, copy it across first:\n",
         "  cp ", mirror_file, " ", source_file)
  }

  mirror_hash <- tools::md5sum(path.expand(mirror_file))
  source_hash <- tools::md5sum(path.expand(source_file))

  if (mirror_hash == source_hash) {
    cat("In sync - ont_mm's copy matches GC-Ontology-Mirror exactly.\n")
    return(invisible(TRUE))
  }

  # Give a same/different line count as a quick, human-readable hint
  # at HOW different they are, without needing a full diff here.
  mirror_lines <- length(readLines(mirror_file, warn = FALSE))
  source_lines <- length(readLines(source_file, warn = FALSE))

  stop(
    "OUT OF SYNC: ont_mm's copy of the source ontology does not match ",
    "GC-Ontology-Mirror.\n",
    "  GC-Ontology-Mirror: ", mirror_lines, " lines\n",
    "  ont_mm's copy:      ", source_lines, " lines\n",
    "Any schema rebuild done now would silently use stale source, ",
    "with no error at any step - fix this first:\n",
    "  cp ", mirror_file, " ", source_file, "\n",
    "Then re-run this check to confirm before rebuilding."
  )
}
