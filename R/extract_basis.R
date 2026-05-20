map_gbasis_to_name <- function(gbasis_line) {
  # Normalize input
  line <- toupper(gbasis_line)
  
  # Extract values safely
  gbasis <- sub(".*GBASIS\\s*=\\s*([A-Z0-9]+).*", "\\1", line)
  
  ngauss <- if (grepl("NGAUSS\\s*=", line)) {
    sub(".*NGAUSS\\s*=\\s*([0-9]+).*", "\\1", line)
  } else {
    NA
  }
  
  # Named mapping table
  base_map <- c(
    "N21"  = "3-21G",
    "N31"  = "6-31G",
    "N311" = "6-311G",
    "DZV"  = "DZV",
    "TZV"  = "TZV",
    "CC"   = "cc-pVXZ"
  )
  
  # Special cases
  if (gbasis == "STO" && !is.na(ngauss)) {
    return(paste0("STO-", ngauss, "G"))
  }
  
  if (gbasis %in% names(base_map)) {
    return(base_map[[gbasis]])
  }
  
  # Fallback
  return(gbasis)
}


extract_basis_name <- function(file) {
  lines <- readLines(file, warn = FALSE)
  
  # Find GBASIS line (case-insensitive)
  gbasis_line <- grep("GBASIS", lines, value = TRUE, ignore.case = TRUE)
  
  if (length(gbasis_line) > 0) {
    return(map_gbasis_to_name(gbasis_line[1]))
  }
  
  # Detect custom basis
  if (any(grepl("^\\s*\\$DATA", lines, ignore.case = TRUE))) {
    return("Custom ($DATA)")
  }
  
  return(NA_character_)
}


extract_basis_folder <- function(folder_path, pattern = "\\.(inp|in)$") {
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  data.frame(
    file = basename(files),
    basis = vapply(files, extract_basis_name, character(1)),
    stringsAsFactors = FALSE
  )
}