#' Classify a GAMESS calculation by job type
#'
#' Inspects a GAMESS input (.inp) or output (.log) file and determines
#' which ontology calculation class it corresponds to, based on the
#' $CONTRL RUNTYP keyword and, where relevant, $STATPT HSSEND.
#'
#' Works from either an .inp or a .log file: GAMESS echoes the input
#' deck verbatim into the log ("INPUT CARD> ..."), so RUNTYP and HSSEND
#' are readable from either. This means classification can happen before
#' a job has even run, not just after.
#'
#' Only classes actually present in the current gc: ontology
#' (GeometryOptimization, SinglePoint, VibrationalAnalysis) are returned
#' with confidence. Anything else (RUNTYP=IRC, SADPOINT, NMR, HESSIAN,
#' DRC, ...) comes back as job_type = NA with a warning naming the RUNTYP
#' found, rather than being silently folded into GeometryOptimization -
#' that silent fallback is the bug this function replaces.
#'
#' @param file Path to a GAMESS .inp or .log file.
#' @return A list:
#'   \item{job_type}{One of "GeometryOptimization", "SinglePoint",
#'     "VibrationalAnalysis", or NA if unrecognised. Unprefixed - the
#'     caller decides whether to write "gc:X" or "ex:X" (see the
#'     ex:/gc: prefix inconsistency noted for experiment_template.tsv).}
#'   \item{runtyp}{The raw RUNTYP value found (character, or NA).}
#'   \item{hssend}{Logical, or NA if not present/applicable.}
#'   \item{basis_for_classification}{Which file type informed the call:
#'     "inp" or "log", detected from the extension.}
#' @export
classify_gamess_job <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(file, warn = FALSE)
  text  <- toupper(paste(lines, collapse = " "))

  file_kind <- if (grepl("\\.inp$", file, ignore.case = TRUE)) "inp" else "log"

  runtyp_match <- regmatches(text, regexpr("RUNTYP\\s*=\\s*[A-Z]+", text))
  runtyp <- if (length(runtyp_match) > 0) {
    sub("RUNTYP\\s*=\\s*", "", runtyp_match)
  } else {
    NA_character_
  }

  hssend_match <- regmatches(text, regexpr("HSSEND\\s*=\\s*\\.[A-Z]+\\.", text))
  hssend <- if (length(hssend_match) > 0) {
    grepl("\\.T\\.?", hssend_match)
  } else {
    NA
  }

  if (is.na(runtyp)) {
    warning("No RUNTYP found in ", file, " - cannot classify job type")
    return(list(job_type = NA_character_, runtyp = NA_character_,
                hssend = NA, basis_for_classification = file_kind))
  }

  job_type <- switch(
    runtyp,
    "OPTIMIZE" = if (isTRUE(hssend)) "VibrationalAnalysis" else "GeometryOptimization",
    "ENERGY"   = "SinglePoint",
    "HESSIAN"  = "VibrationalAnalysis",
    NA_character_
  )

  if (is.na(job_type)) {
    warning(
      "RUNTYP=", runtyp, " in ", file,
      " has no corresponding class in the current gc: ontology ",
      "(only GeometryOptimization, SinglePoint, VibrationalAnalysis exist). ",
      "Returning job_type = NA rather than guessing."
    )
  }

  list(
    job_type = job_type,
    runtyp = runtyp,
    hssend = hssend,
    basis_for_classification = file_kind
  )
}


#' Classify every .inp file in a directory
#'
#' Batch wrapper around classify_gamess_job(), for the common case of
#' dispatching a whole folder of experiments at once (e.g. inside
#' process_experiments.R).
#'
#' @param dir Directory containing .inp files.
#' @return A data.frame with columns: file, job_type, runtyp, hssend.
#' @export
classify_gamess_jobs <- function(dir) {
  files <- list.files(dir, pattern = "\\.inp$", full.names = TRUE)

  rows <- lapply(files, function(f) {
    res <- classify_gamess_job(f)
    data.frame(
      file = f,
      job_type = ifelse(is.na(res$job_type), NA_character_, res$job_type),
      runtyp = res$runtyp,
      hssend = res$hssend,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
