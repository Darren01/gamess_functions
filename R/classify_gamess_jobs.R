#' Classify a GAMESS calculation by job type
#'
#' Inspects a GAMESS input (.inp) or output (.log) file and determines
#' which ontology calculation class it corresponds to, based on the
#' $CONTRL RUNTYP keyword and, where relevant, $STATPT HSSEND or $IRC
#' FORWRD.
#'
#' Works from either an .inp or a .log file: GAMESS echoes the input
#' deck verbatim into the log ("INPUT CARD> ..."), so RUNTYP and HSSEND
#' are readable from either. This means classification can happen before
#' a job has even run, not just after.
#'
#' RUNTYP=IRC jobs also get a direction ("forward" or "backward"), read
#' from the $IRC group's FORWRD keyword. GAMESS's own default is
#' FORWRD=.TRUE. (forward) when not given at all, so backward runs must
#' explicitly set FORWRD=.F. - this function follows that same
#' documented default rather than treating "not specified" as unknown.
#'
#' Only classes actually present in the current gc: ontology
#' (GeometryOptimization, SinglePoint, VibrationalAnalysis, SaddlePoint,
#' IRC) are returned with confidence. Anything else (NMR, HESSIAN, DRC,
#' ...) comes back as job_type = NA with a warning naming the RUNTYP
#' found, rather than being silently folded into GeometryOptimization -
#' that silent fallback is the bug this function replaces.
#'
#' @param file Path to a GAMESS .inp or .log file.
#' @return A list:
#'   \item{job_type}{One of "GeometryOptimization", "SinglePoint",
#'     "VibrationalAnalysis", "SaddlePoint", "IRC", or NA if
#'     unrecognised. Unprefixed - the caller decides whether to write
#'     "gc:X" or "ex:X" (see the ex:/gc: prefix inconsistency noted for
#'     experiment_template.tsv).}
#'   \item{runtyp}{The raw RUNTYP value found (character, or NA).}
#'   \item{hssend}{Logical, or NA if not present/applicable.}
#'   \item{irc_direction}{"forward" or "backward" if job_type == "IRC",
#'     NA otherwise.}
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
                hssend = NA, irc_direction = NA_character_,
                basis_for_classification = file_kind))
  }

  job_type <- switch(
    runtyp,
    "OPTIMIZE" = if (isTRUE(hssend)) "VibrationalAnalysis" else "GeometryOptimization",
    "ENERGY"   = "SinglePoint",
    "HESSIAN"  = "VibrationalAnalysis",
    "SADPOINT" = "SaddlePoint",
    "IRC"      = "IRC",
    NA_character_
  )

  if (is.na(job_type)) {
    warning(
      "RUNTYP=", runtyp, " in ", file,
      " has no corresponding class in the current gc: ontology ",
      "(only GeometryOptimization, SinglePoint, VibrationalAnalysis, ",
      "SaddlePoint, IRC exist). Returning job_type = NA rather than guessing."
    )
  }

  # FORWRD is only meaningful for RUNTYP=IRC. GAMESS's documented default
  # is .TRUE. (forward) - only an explicit FORWRD=.F. means backward.
  irc_direction <- NA_character_
  if (!is.na(job_type) && job_type == "IRC") {
    forwrd_match <- regmatches(text, regexpr("FORWRD\\s*=\\s*\\.[A-Z]+\\.", text))
    irc_direction <- if (length(forwrd_match) > 0 && grepl("\\.F\\.?", forwrd_match)) {
      "backward"
    } else {
      "forward"
    }
  }

  list(
    job_type = job_type,
    runtyp = runtyp,
    hssend = hssend,
    irc_direction = irc_direction,
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
#' @return A data.frame with columns: file, job_type, runtyp, hssend,
#'   irc_direction.
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
      irc_direction = res$irc_direction,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
