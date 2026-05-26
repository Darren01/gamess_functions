extract_geometry_trajectory <- function(logfile) {
  lines <- readLines(path.expand(logfile), warn = FALSE)
  
  # -----------------------------
  # helpers
  # -----------------------------
  
  is_coord_start <- function(x) {
    grepl("COORDINATES OF ALL ATOMS", x)
  }
  
  is_separator <- function(x) {
    grepl("^-{3,}", x)
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
  # 1. ENERGY EXTRACTION (FIXED)
  # -----------------------------
  
  # ns_lines <- grep("^\\s*NSERCH:\\s*", lines, value = TRUE)
  # 
  # energies <- sapply(ns_lines, function(x) {
  #   m <- regexpr("E=\\s*[-0-9.]+", x)
  #   if (m[1] == -1) return(NA_real_)
  #   as.numeric(sub("E=\\s*", "", regmatches(x, m)))
  # })
  
  # -----------------------------
  # 1. ENERGY EXTRACTION (CLEAN FIX)
  # -----------------------------
  
  ns_lines <- grep("^\\s*NSERCH:\\s*", lines, value = TRUE)
  
  energies <- sapply(ns_lines, function(x) {
    m <- regexpr("E=\\s*[-0-9.]+", x)
    if (m[1] == -1) return(NA_real_)
    as.numeric(sub("E=\\s*", "", regmatches(x, m)))
  })
  
  # IMPORTANT: strip names (this is your current issue)
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
  
  # -----------------------------
  # 3. ALIGNMENT (CRITICAL FIX)
  # -----------------------------
  
  n_geom <- length(geometries)
  n_energy <- length(energies)
  
  # IMPORTANT: GAMESS often has 1 energy per NSERCH step,
  # but geometry blocks may lag or include extra prints
  
  if (n_energy < n_geom) {
    energies <- c(energies, rep(NA_real_, n_geom - n_energy))
  } else if (n_energy > n_geom) {
    energies <- energies[seq_len(n_geom)]
  }
  
  steps <- seq_len(n_geom)
  
  # -----------------------------
  # min energy
  # -----------------------------
  
  tol <- 1e-6
  
  if (all(is.na(energies))) {
    min_energy <- NA
    min_step <- NA
    min_geometry <- NULL
  } else {
    min_energy <- min(energies, na.rm = TRUE)
    idxs <- which(abs(energies - min_energy) < tol)
    min_step <- max(idxs)   # <-- critical fix
    min_geometry <- geometries[[min_step]]
  }
  
  # -----------------------------
  # output
  # -----------------------------
  
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