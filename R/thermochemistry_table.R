#' Build a thermochemistry summary table across one or more log files
#'
#' Matches the table format used in the caa write-up directly: one row
#' per file, ZPE/Enthalpy/Entropy/Gibbs plus a level-of-theory label
#' and an optional notes column. A single bad or unusable file doesn't
#' abort the whole batch - it's reported as a row of NAs with a reason,
#' so a table across many files degrades gracefully rather than
#' stopping partway through.
#'
#' @param files A character vector of log file paths. If unnamed, the
#'   ScanNo column is derived from each file's basename (stem, no
#'   extension) - e.g. "caa005bTSa.log" becomes "caa005bTSa". If named,
#'   the names are used as ScanNo directly instead.
#' @param notes Optional named character vector, keyed by ScanNo (the
#'   same labels that end up in the table), for the Notes column - e.g.
#'   c(caa005bTSa = "Transition State"). Any ScanNo without a matching
#'   name gets an empty Notes cell, not NA.
#' @return A data.frame with columns: ScanNo, ZPE_Ha, Enthalpy_kJ,
#'   Entropy_JmolK, GibbsFreeEnergy_kJ, LevelOfTheory, Notes.
#' @export
thermochemistry_table <- function(files, notes = character(0)) {

  scan_ids <- if (!is.null(names(files)) && all(nzchar(names(files)))) {
    names(files)
  } else {
    tools::file_path_sans_ext(basename(files))
  }

  rows <- lapply(seq_along(files), function(i) {
    f <- files[i]
    id <- scan_ids[i]

    thermo <- tryCatch(extract_thermochemistry(f), error = function(e) NULL)
    level <- tryCatch(extract_level_of_theory(f), error = function(e) NA_character_)

    note <- if (id %in% names(notes)) notes[[id]] else ""

    if (is.null(thermo)) {
      warning("No thermochemistry found for ", id, " (", f, ") - reported as NA.")
      return(data.frame(ScanNo = id, ZPE_Ha = NA_real_, Enthalpy_kJ = NA_real_,
                         Entropy_JmolK = NA_real_, GibbsFreeEnergy_kJ = NA_real_,
                         LevelOfTheory = level, Notes = note, stringsAsFactors = FALSE))
    }

    data.frame(ScanNo = id, ZPE_Ha = thermo$zpe, Enthalpy_kJ = thermo$enthalpy,
               Entropy_JmolK = thermo$entropy, GibbsFreeEnergy_kJ = thermo$gibbs,
               LevelOfTheory = level, Notes = note, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

#' Print a thermochemistry table as a copy-paste-ready Markdown table
#'
#' @param table Output of thermochemistry_table().
#' @export
print_markdown_table <- function(table) {
  header <- c("ScanNo", "ZPE-Ha", "Enthalpy-KJ", "Entropy-J/molK",
              "GibbsFreeEnergy-kJ", "LevelOfTheory", "Notes")
  fmt_num <- function(x) ifelse(is.na(x), "", formatC(x, digits = 6, format = "f"))

  rows <- lapply(seq_len(nrow(table)), function(i) {
    c(table$ScanNo[i], fmt_num(table$ZPE_Ha[i]), fmt_num(table$Enthalpy_kJ[i]),
      fmt_num(table$Entropy_JmolK[i]), fmt_num(table$GibbsFreeEnergy_kJ[i]),
      ifelse(is.na(table$LevelOfTheory[i]), "", table$LevelOfTheory[i]),
      table$Notes[i])
  })

  cat("|", paste(header, collapse = " | "), "|\n")
  cat("|", paste(rep("---", length(header)), collapse = " | "), "|\n")
  for (r in rows) cat("|", paste(r, collapse = " | "), "|\n")

  invisible(NULL)
}
