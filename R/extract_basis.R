# ============================================================
# GAMESS Basis Set Extractor
# ------------------------------------------------------------
# Extracts and interprets basis set names from GAMESS input files
# Supports:
#   - Pople basis sets (e.g. 6-31+G*, 6-311++G**)
#   - STO-nG
#   - Dunning (cc-pVXZ, aug-cc-pVXZ)
#   - Custom basis detection ($DATA)
#
# Depends on gamess_input_utils.R (strip_input_card_prefix, get_gamess_block)
# - source that first.
# ============================================================


# ---- Extract $BASIS block ----
# Uses the shared block matcher (gamess_input_utils.R) so this works on
# both raw .inp files and .log files' echoed "INPUT CARD>" text. The
# previous version anchored to start-of-line only, which meant it silently
# returned NA when given a .log file - a mirror-image of the opposite bug
# in extract_input_parameters(), which only worked on .log. Both are now
# fixed to use the same matcher, so they agree on either file type.
extract_basis_block <- function(lines) {
  lines <- strip_input_card_prefix(lines)
  get_gamess_block(lines, "BASIS")
}


# ---- Parse keywords from block ----
parse_basis_keywords <- function(block_lines) {
  if (is.null(block_lines)) return(NULL)
  
  text <- toupper(paste(block_lines, collapse = " "))
  
  get_val <- function(key) {
    pattern <- paste0(key, "\\s*=\\s*([A-Z0-9\\.]+)")
    m <- regexec(pattern, text, perl = TRUE)
    res <- regmatches(text, m)[[1]]
    
    if (length(res) < 2) return(NA)
    res[2]
  }
  
  list(
    gbasis = get_val("GBASIS"),
    ngauss = suppressWarnings(as.numeric(get_val("NGAUSS"))),
    ndfunc = suppressWarnings(as.numeric(get_val("NDFUNC"))),
    npfunc = suppressWarnings(as.numeric(get_val("NPFUNC"))),
    diffsp = get_val("DIFFSP"),
    diffs  = get_val("DIFFS")
  )
}


# ---- Interpret into human-readable basis name ----
interpret_basis <- function(b) {
  if (is.null(b) || is.na(b$gbasis)) return(NA_character_)
  
  # ---- Base mapping ----
  base <- switch(b$gbasis,
                 "STO"  = if (!is.na(b$ngauss)) paste0("STO-", b$ngauss, "G") else "STO",
                 "N21"  = "3-21G",
                 "N31"  = "6-31G",
                 "N311" = "6-311G",
                 "DZV"  = "DZV",
                 "TZV"  = "TZV",
                 "CC"   = "cc-pVXZ",
                 b$gbasis
  )
  
  # ---- Diffuse functions (+, ++) ----
  plus <- ""
  if (!is.na(b$diffsp) && b$diffsp == ".TRUE.") plus <- "+"
  if (!is.na(b$diffs)  && b$diffs  == ".TRUE.") plus <- paste0(plus, "+")
  
  # Special handling for Dunning sets
  if (b$gbasis == "CC" && plus != "") {
    base <- paste0("aug-", base)
    plus <- ""
  }
  
  # ---- Polarization (*, **) ----
  ndf <- ifelse(is.na(b$ndfunc), 0, b$ndfunc)
  npf <- ifelse(is.na(b$npfunc), 0, b$npfunc)
  
  star <- ""
  if (ndf > 0 && npf > 0) {
    star <- "**"
  } else if (ndf > 0 || npf > 0) {
    star <- "*"
  }
  
  paste0(base, plus, star)
}


#' Extract and interpret the basis set from a GAMESS input or output file
#'
#' Supports Pople basis sets (e.g. 6-31+G*, 6-311++G**), STO-nG, Dunning
#' (cc-pVXZ, aug-cc-pVXZ), and falls back to reporting a custom basis
#' where a $DATA-defined one is used instead of a named $BASIS keyword set.
#'
#' @param file Path to a GAMESS .inp or .log file.
#' @return The interpreted basis set name (character), "Custom ($DATA)"
#'   if no $BASIS keyword set was found but $DATA was, or NA if neither.
#' @export
extract_basis_name <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(path.expand(file), warn = FALSE)
  
  block <- extract_basis_block(lines)
  parsed <- parse_basis_keywords(block)
  name <- interpret_basis(parsed)
  
  if (!is.na(name)) return(name)
  
  # Fallback: custom basis
  if (any(grepl("^\\s*\\$DATA", lines, ignore.case = TRUE))) {
    return("Custom ($DATA)")
  }
  
  NA_character_
}


#' Extract basis sets for every GAMESS input file in a folder
#'
#' @param folder_path Directory containing .inp files.
#' @param pattern Filename pattern to match (default: .inp or .in).
#' @return A data.frame: file, basis.
#' @export
extract_basis_folder <- function(folder_path, pattern = "\\.(inp|in)$") {
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  data.frame(
    file = basename(files),
    basis = vapply(files, extract_basis_name, character(1)),
    stringsAsFactors = FALSE
  )
}
