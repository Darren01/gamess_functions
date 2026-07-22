#' Extract geometry constraints from a GAMESS $ZMAT block
#'
#' Parses IFZMAT/FVALUE pairs out of every $ZMAT...$END block in a GAMESS
#' .inp or .log file. Pure extraction - no ontology knowledge, no ex:/gc:
#' prefixes, matching the same separation used by extract_ir_spectrum()/
#' extract_thermochemistry() (this file just gets the raw numbers;
#' constraints_to_templates() does the ontology mapping).
#'
#' Moved here from ont_mm/scripts/process_contraints.R, where this exact
#' parsing logic used to live duplicated rather than in the one place
#' that owns GAMESS file parsing - flagged as an architectural gap from
#' very early in this project. Uses the shared get_gamess_blocks() (from
#' gamess_input_utils.R) rather than its own separate block-matching
#' regex, and works on either .inp or .log via strip_input_card_prefix(),
#' same as every other extractor here.
#'
#' GAMESS's IFZMAT constraint type codes: 1 = distance (2 atoms),
#' 2 = angle (3 atoms), 3 = dihedral (4 atoms).
#'
#' @param file Path to a GAMESS .inp or .log file.
#' @return A data.frame: type ("distance"/"angle"/"dihedral"), atom1,
#'   atom2, atom3 (NA for distance), atom4 (NA for distance/angle),
#'   value (the constrained target value - Angstrom for distance,
#'   degrees for angle/dihedral, matching GAMESS's own convention).
#'   Zero rows (not an error) if the file has no $ZMAT block or no
#'   IFZMAT constraints in it.
#' @export
extract_constraints <- function(file) {

  lines <- strip_input_card_prefix(readLines(file, warn = FALSE))

  zmat_blocks <- get_gamess_blocks(lines, "ZMAT")

  empty <- data.frame(type = character(0), atom1 = integer(0),
                       atom2 = integer(0), atom3 = integer(0),
                       atom4 = integer(0), value = numeric(0),
                       stringsAsFactors = FALSE)

  if (length(zmat_blocks) == 0) return(empty)

  clean_numbers <- function(x) {
    nums <- unlist(strsplit(x, ","))
    nums <- gsub("[^0-9\\.]", "", nums)
    nums <- nums[nums != ""]
    as.numeric(nums)
  }

  type_names <- c(`1` = "distance", `2` = "angle", `3` = "dihedral")
  n_atoms    <- c(`1` = 2, `2` = 3, `3` = 4)

  rows <- list()

  for (block_lines in zmat_blocks) {
    block <- paste(block_lines, collapse = " ")
    if (!grepl("IFZMAT", block)) next

    ifzmat_text <- sub(".*IFZMAT\\(1\\)=([^\\$]+?)FVALUE.*", "\\1", block)
    fvalue_text <- sub(".*FVALUE\\(1\\)=([^\\$]+?)\\$END.*", "\\1", block)

    ifzmat_nums <- clean_numbers(ifzmat_text)
    fvalues     <- clean_numbers(fvalue_text)

    if (length(ifzmat_nums) == 0 || length(fvalues) == 0) next

    i <- 1
    constraint_index <- 1

    while (i <= length(ifzmat_nums)) {
      type <- ifzmat_nums[i]

      if (!type %in% c(1, 2, 3)) break

      k <- n_atoms[[as.character(type)]]
      atoms <- ifzmat_nums[(i + 1):(i + k)]
      value <- fvalues[constraint_index]
      if (is.na(value)) value <- 0

      row <- list(
        type  = type_names[[as.character(type)]],
        atom1 = atoms[1],
        atom2 = atoms[2],
        atom3 = if (k >= 3) atoms[3] else NA_real_,
        atom4 = if (k >= 4) atoms[4] else NA_real_,
        value = value
      )
      rows[[length(rows) + 1]] <- row

      i <- i + 1 + k
      constraint_index <- constraint_index + 1
    }
  }

  if (length(rows) == 0) return(empty)

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}
