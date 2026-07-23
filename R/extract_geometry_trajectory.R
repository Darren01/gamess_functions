#' Extract the full geometry optimisation trajectory from a GAMESS log
#'
#' Parses every "COORDINATES OF ALL ATOMS" block (one per optimisation
#' step) alongside each step's energy (from the NSERCH: lines), and
#' identifies the converged minimum-energy geometry.
#'
#' @param file Path to a GAMESS .log file with completed optimisation
#'   steps (RUNTYP=OPTIMIZE).
#' @return A list: n_geometries, n_energies (non-NA count), steps,
#'   energies (Hartree, one per geometry - NA where GAMESS's own energy
#'   print didn't align 1:1 with a geometry block), geometries (a list
#'   of data.frames, one per step: atom, charge, x, y, z), min_energy,
#'   min_step, min_geometry (the geometry at the lowest-energy step -
#'   when multiple steps tie within tolerance, the last of them is used,
#'   since GAMESS's optimiser converges toward later steps being more
#'   reliable than earlier, numerically-noisier ones at the same nominal
#'   energy).
#' @export
extract_geometry_trajectory <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)

  # -----------------------------
  # helpers
  # -----------------------------

  is_coord_start <- function(x) {
    grepl("COORDINATES OF ALL ATOMS", x)
  }

  is_atom_line <- function(x) {
    grepl("^\\s*[A-Z][A-Z]?\\s+[-0-9.]+\\s+[-0-9.Ee+\\-]+\\s+[-0-9.Ee+\\-]+\\s+[-0-9.Ee+\\-]+\\s*$", x)
  }

  parse_atom <- function(x) {
    parts <- strsplit(trimws(x), "\\s+")[[1]]
    data.frame(
      atom = parts[1],
      charge = as.numeric(parts[2]),
      x = as.numeric(parts[3]),
      y = as.numeric(parts[4]),
      z = as.numeric(parts[5]),
      stringsAsFactors = FALSE
    )
  }

  # -----------------------------
  # 1. ENERGY EXTRACTION
  # -----------------------------

  ns_lines <- grep("^\\s*NSERCH:\\s*", lines, value = TRUE)

  energies <- sapply(ns_lines, function(x) {
    m <- regexpr("E=\\s*[-0-9.]+", x)
    if (m[1] == -1) return(NA_real_)
    as.numeric(sub("E=\\s*", "", regmatches(x, m)))
  })
  names(energies) <- NULL

  # -----------------------------
  # 2. GEOMETRY EXTRACTION
  # -----------------------------

  geometries <- list()

  i <- 1
  n <- length(lines)

  while (i <= n) {

    if (is_coord_start(lines[i])) {

      # move to ATOM table
      j <- i
      while (j <= n && !grepl("^\\s*ATOM\\s+CHARGE", lines[j])) {
        j <- j + 1
      }

      if (j > n) {
        i <- i + 1
        next
      }

      j <- j + 2  # skip header + separator

      geom <- list()

      while (j <= n && is_atom_line(lines[j])) {
        geom[[length(geom) + 1]] <- parse_atom(lines[j])
        j <- j + 1
      }

      if (length(geom) > 0) {
        geometries[[length(geometries) + 1]] <- do.call(rbind, geom)
      }

      i <- j
      next
    }

    i <- i + 1
  }

  if (length(geometries) == 0) {
    stop("No 'COORDINATES OF ALL ATOMS' blocks found in ", file,
         " - is this a completed GAMESS optimisation log?")
  }

  # -----------------------------
  # 3. ALIGNMENT
  # -----------------------------
  # GAMESS usually prints one energy per NSERCH step, but geometry blocks
  # can lag or include extra prints - pad/truncate energies to match the
  # number of geometry blocks actually found, rather than assume 1:1.

  n_geom <- length(geometries)
  n_energy <- length(energies)

  if (n_energy < n_geom) {
    energies <- c(energies, rep(NA_real_, n_geom - n_energy))
  } else if (n_energy > n_geom) {
    energies <- energies[seq_len(n_geom)]
  }

  steps <- seq_len(n_geom)

  # -----------------------------
  # 4. MINIMUM ENERGY
  # -----------------------------

  tol <- 1e-6

  if (all(is.na(energies))) {
    min_energy <- NA
    min_step <- NA
    min_geometry <- NULL
  } else {
    min_energy <- min(energies, na.rm = TRUE)
    idxs <- which(abs(energies - min_energy) < tol)
    min_step <- max(idxs)  # last of any tied steps - see @return above
    min_geometry <- geometries[[min_step]]
  }

  list(
    n_geometries = n_geom,
    n_energies = sum(!is.na(energies)),
    steps = steps,
    energies = energies,
    geometries = geometries,
    min_energy = min_energy,
    min_step = min_step,
    min_geometry = min_geometry
  )
}
