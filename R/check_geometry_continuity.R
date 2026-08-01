#' Extract the starting geometry from a GAMESS .inp file's $DATA block
#'
#' Same output shape as extract_geometry_trajectory()'s min_geometry
#' (atom, charge, x, y, z) - specifically so the two can be compared
#' directly, which is the whole point: checking whether a run's actual
#' starting geometry matches what its supposed predecessor's log
#' actually converged to. See check_geometry_continuity() for that
#' comparison itself.
#'
#' @param file Path to a GAMESS .inp file.
#' @return A data.frame: atom, charge, x, y, z.
#' @export
extract_data_block_geometry <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  # GAMESS echoes input lines in .log files prefixed with "INPUT CARD>"
  # (a raw .inp file has no such prefix) - strip it if present, so this
  # function works on either a raw .inp or a .log's own input echo,
  # without needing two separate parsers for the same underlying format.
  lines <- sub("^\\s*INPUT CARD>", "", lines)

  start <- grep("^\\s*\\$DATA\\s*$", lines, ignore.case = TRUE)
  if (length(start) == 0) {
    stop("No $DATA block found in ", file)
  }
  start <- start[1]

  end <- start + which(grepl("^\\s*\\$END\\s*$", lines[(start + 1):length(lines)], ignore.case = TRUE))[1]
  if (is.na(end)) {
    stop("Found $DATA in ", file, " but no matching $END")
  }

  # $DATA block: title line, point-group line, then atom rows
  block <- lines[(start + 1):(end - 1)]
  block <- block[nzchar(trimws(block))]

  if (length(block) < 2) {
    stop("$DATA block in ", file, " has no atom rows after the title/symmetry lines")
  }

  atom_lines <- block[-(1:2)]  # drop title, point-group symmetry line

  is_atom_line <- grepl("^\\s*[A-Za-z][a-z]?\\s+[-0-9.]+\\s+[-0-9.Ee+]+\\s+[-0-9.Ee+]+\\s+[-0-9.Ee+]+\\s*$", atom_lines)
  atom_lines <- atom_lines[is_atom_line]

  if (length(atom_lines) == 0) {
    stop("$DATA block in ", file, " - no lines matched the expected 'element charge x y z' format")
  }

  parsed <- lapply(atom_lines, function(x) {
    parts <- strsplit(trimws(x), "\\s+")[[1]]
    data.frame(atom = parts[1], charge = as.numeric(parts[2]),
               x = as.numeric(parts[3]), y = as.numeric(parts[4]), z = as.numeric(parts[5]),
               stringsAsFactors = FALSE)
  })

  do.call(rbind, parsed)
}


#' Check whether a run's starting geometry matches its supposed
#' predecessor's converged geometry
#'
#' The palimpsest check: if an earlier input file was accidentally
#' overwritten before being saved under its own name, the resulting gap
#' in the provenance chain is only ever discoverable in hindsight, from
#' static evidence - never at the moment it happens. This is that
#' static evidence: does the later run's actual starting geometry match
#' what the earlier run's log genuinely converged to?
#'
#' Small numerical differences are expected and fine (manual retyping,
#' rounding, minor formatting differences) - this flags large
#' displacements, not exact mismatches. Element symbol comparison is
#' case-insensitive - GAMESS's own log output and a hand-typed $DATA
#' block don't necessarily agree on case (e.g. "CL" vs "Cl") even for
#' the same real element, and case is never chemically meaningful.
#'
#' @param later_inp EITHER a path to the later run's .inp file, OR an
#'   already-extracted geometry data.frame (e.g. the output of calling
#'   extract_data_block_geometry() yourself first) - both work.
#' @param earlier_log EITHER a path to the earlier run's .log file
#'   (the supposed predecessor), OR an already-extracted geometry
#'   data.frame (e.g. extract_geometry_trajectory(...)$min_geometry
#'   called yourself first) - both work.
#' @param tolerance Maximum acceptable per-atom displacement, in the
#'   same units as the geometry (Angstrom for GAMESS). Default 0.05 -
#'   deliberately loose, tuned to tolerate manual retyping/rounding
#'   while still catching a genuinely different molecule/geometry.
#' @return A list: consistent (logical), max_displacement (the largest
#'   single-atom movement found), n_atoms_compared, message.
#' @export
check_geometry_continuity <- function(later_inp, earlier_log, tolerance = 0.05) {

  # Accept either a file path (character) or an already-extracted
  # geometry (data.frame) for each argument - composing
  # extract_data_block_geometry()/extract_geometry_trajectory()
  # yourself first and passing the result in is a completely
  # reasonable way to use this, not something that should require
  # re-reading the same file from disk internally.
  later_geom <- if (is.data.frame(later_inp)) {
    later_inp
  } else {
    extract_data_block_geometry(later_inp)
  }

  earlier_geom <- if (is.data.frame(earlier_log)) {
    earlier_log
  } else {
    extract_geometry_trajectory(earlier_log)$min_geometry
  }

  later_label <- if (is.data.frame(later_inp)) "later geometry" else basename(later_inp)
  earlier_label <- if (is.data.frame(earlier_log)) "earlier geometry" else basename(earlier_log)

  if (nrow(later_geom) != nrow(earlier_geom)) {
    return(list(
      consistent = FALSE,
      max_displacement = NA,
      n_atoms_compared = NA,
      message = paste0(
        later_label, " has ", nrow(later_geom), " atoms, but ",
        earlier_label, "'s converged geometry has ", nrow(earlier_geom),
        " - these can't be the same molecule/system, so this can't be a genuine continuation."
      )
    ))
  }

  later_elements <- toupper(trimws(later_geom$atom))
  earlier_elements <- toupper(trimws(earlier_geom$atom))

  if (!identical(later_elements, earlier_elements)) {
    mismatch_idx <- which(later_elements != earlier_elements)
    return(list(
      consistent = FALSE,
      max_displacement = NA,
      n_atoms_compared = nrow(later_geom),
      message = paste0(
        "Same atom count but different element identities between ",
        later_label, " and ", earlier_label, " (even case-insensitively) ",
        "at position(s) ", paste(mismatch_idx, collapse = ", "), ": ",
        paste0(later_geom$atom[mismatch_idx], " vs ", earlier_geom$atom[mismatch_idx], collapse = "; "),
        " - check these are really meant to be the same sequence."
      )
    ))
  }

  displacements <- sqrt(
    (later_geom$x - earlier_geom$x)^2 +
    (later_geom$y - earlier_geom$y)^2 +
    (later_geom$z - earlier_geom$z)^2
  )
  max_disp <- max(displacements)

  consistent <- max_disp <= tolerance

  message <- if (consistent) {
    paste0(later_label, "'s starting geometry matches ", earlier_label,
           "'s converged geometry (max displacement ", round(max_disp, 4), " - within tolerance).")
  } else {
    paste0("MISMATCH: ", later_label, "'s starting geometry does NOT match ",
           earlier_label, "'s converged geometry (max displacement ", round(max_disp, 4),
           ", exceeds tolerance of ", tolerance, "). This is exactly the signature of a lost/",
           "overwritten intermediate file - the input this run actually started from may not be ",
           "the one its filename implies.")
  }

  list(
    consistent = consistent,
    max_displacement = max_disp,
    n_atoms_compared = nrow(later_geom),
    message = message
  )
}
