#' Extract GIAO NMR chemical shielding tensors from a GAMESS log
#'
#' Parses RUNTYP=NMR output for each atom's isotropic shielding and
#' anisotropy from the "GIAO CHEMICAL SHIELDING TENSOR" section.
#'
#' Each atom's block has this shape (real example):
#'
#'     1 SI        X    480.5633       -0.2482        0.0257
#'                 Y     -0.1847      480.5256       -0.1838
#'                 Z      0.0302       -0.1836      480.9664
#'                                                                  480.6851
#'      EIGENVALS:      480.3075      480.6919      481.0559
#'                                                             (      0.5563 )
#'
#' The isotropic shielding is a lone number on the line immediately
#' before EIGENVALS; the anisotropy is in parentheses on the line
#' immediately after. This function anchors on EIGENVALS (a reliable,
#' unambiguous marker) and reads the two adjacent lines with regex-based
#' number extraction, rather than the previous version's approach of
#' applying one fixed column range (characters 66-74) across all three
#' structurally different line types - which only worked by coincidence
#' of typical number widths, and would have silently misparsed (not
#' errored) on a value wide enough to shift outside that window, e.g. a
#' negative isotropic shielding value or a larger system's atom index.
#'
#' @param file Path to a GAMESS .log file with completed NMR shieldings
#'   (RUNTYP=NMR).
#' @return A data.frame: atom_index (integer, 1-based, as GAMESS numbers
#'   it), element (character, e.g. "SI", "C", "H"), isotropic_shielding
#'   (ppm), anisotropy (ppm).
#' @export
extract_nmr <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  start_idx <- grep("GIAO CHEMICAL SHIELDING TENSOR", lines)
  end_idx   <- grep("DONE WITH NMR SHIELDINGS", lines)

  if (length(start_idx) == 0 || length(end_idx) == 0) {
    stop("No GIAO chemical shielding tensor section found in ", file,
         " - is this a RUNTYP=NMR log with completed shieldings?")
  }

  block_lines <- lines[start_idx[1]:end_idx[1]]

  # each atom's block starts with a line like "    1 SI        X    480.5633 ..."
  atom_header_idx <- grep("^\\s*\\d+\\s+[A-Za-z]+\\s+X\\s", block_lines)

  if (length(atom_header_idx) == 0) {
    stop("Found the GIAO shielding section in ", file,
         " but no atom shielding blocks within it")
  }

  rows <- lapply(atom_header_idx, function(i) {
    header <- block_lines[i]

    atom_match <- regmatches(header, regexec("^\\s*(\\d+)\\s+([A-Za-z]+)", header))[[1]]
    if (length(atom_match) < 3) {
      warning("Could not parse atom index/element from '", header, "' in ", file, " - skipping")
      return(NULL)
    }
    atom_index <- as.integer(atom_match[2])
    element    <- atom_match[3]

    # search a defensive window after the header rather than assume an
    # exact fixed line offset
    window <- block_lines[i:min(i + 6, length(block_lines))]
    eigenvals_idx <- grep("EIGENVALS:", window)

    if (length(eigenvals_idx) == 0) {
      warning("Could not find EIGENVALS line for atom ", atom_index, " in ", file, " - skipping")
      return(NULL)
    }

    isotropic_line  <- window[eigenvals_idx[1] - 1]
    anisotropy_line <- window[eigenvals_idx[1] + 1]

    isotropic  <- suppressWarnings(as.numeric(
      regmatches(isotropic_line, regexpr("-?\\d+\\.\\d+", isotropic_line))))
    anisotropy <- suppressWarnings(as.numeric(
      regmatches(anisotropy_line, regexpr("-?\\d+\\.\\d+", anisotropy_line))))

    if (length(isotropic) == 0)  isotropic  <- NA_real_
    if (length(anisotropy) == 0) anisotropy <- NA_real_

    data.frame(
      atom_index = atom_index,
      element = element,
      isotropic_shielding = isotropic,
      anisotropy = anisotropy,
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    stop("Found atom shielding blocks in ", file, " but couldn't parse any of them")
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
