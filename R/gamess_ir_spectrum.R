# gamess_ir_spectrum.R
#
# Parse harmonic frequencies and IR intensities from a GAMESS (US) log file
# and build a broadened IR spectrum.
#
# Base R only (no package dependencies).
#
# Functions
#   parse_gamess_frequencies()  log file -> data frame of modes
#   build_ir_spectrum()         modes + scale factor -> broadened spectrum
#   plot_ir_spectrum()          base-graphics plot of a spectrum
#   gamess_ir_spectrum()        convenience wrapper: log file -> spectrum
#
# Design notes
#   * The frequency scale factor has NO default. It depends on the functional
#     and basis set (and on what the spectrum is compared with), so it must be
#     looked up and passed explicitly. GAMESS prints "ALL FREQUENCIES ARE SCALED
#     BY 1.00000", i.e. the log values are unscaled.
#   * Imaginary modes (transition states) stop the function unless the caller
#     says explicitly that they should be excluded.
#   * Malformed frequency tables stop with an error rather than returning a
#     partial result.

# 1 D^2/(amu A^2) expressed in km/mol:
#   N_A * pi / (3 c^2) / (4 pi eps0) * (1 D)^2 / (1 A)^2 / (1 amu)
GAMESS_IR_TO_KM_MOL <- 42.25606


#' Parse frequencies and IR intensities from a GAMESS log file
#'
#' @param log_file Path to a GAMESS .log/.out file.
#' @param block Which vibrational analysis to use when the log contains more
#'   than one (e.g. repeated Hessians during an optimisation): "last" or
#'   "first".
#' @return A data frame with one row per mode:
#'   mode, frequency_cm1 (unscaled; negative for imaginary modes), imaginary
#'   (logical), ir_intensity_debye2_amu_ang2 (as printed by GAMESS) and
#'   ir_intensity_km_mol.
parse_gamess_frequencies <- function(log_file, block = c("last", "first")) {
  block <- match.arg(block)
  if (!file.exists(log_file)) stop("File not found: ", log_file)
  lines <- readLines(log_file, warn = FALSE)

  starts <- grep("FREQUENCIES IN CM\\*\\*-1, IR INTENSITIES", lines)
  if (length(starts) == 0) {
    stop("No frequency table found in ", log_file,
         " (was this a HESSIAN/OPTIMIZE/SADPOINT run that reached the Hessian?)")
  }
  start <- if (block == "last") starts[length(starts)] else starts[1]

  # The table ends at the normal-mode orthonormality note, or at the next
  # frequency header, or at end of file.
  later <- c(starts[starts > start],
             grep("NOTE - THE MODES J,K ARE ORTHONORMALIZED", lines))
  later <- later[later > start]
  end <- if (length(later)) min(later) else length(lines)
  tbl <- lines[start:end]

  freq_lines <- grep("^\\s*FREQUENCY:", tbl, value = TRUE)
  ir_lines   <- grep("^\\s*IR INTENSITY:", tbl, value = TRUE)
  if (length(freq_lines) == 0) stop("Frequency header found but no FREQUENCY: rows in ", log_file)
  if (length(freq_lines) != length(ir_lines)) {
    stop("Found ", length(freq_lines), " FREQUENCY rows but ", length(ir_lines),
         " IR INTENSITY rows in ", log_file)
  }

  freq <- numeric(0)
  imag <- logical(0)
  ir   <- numeric(0)
  for (i in seq_along(freq_lines)) {
    f_tok <- strsplit(trimws(sub("^\\s*FREQUENCY:", "", freq_lines[i])), "\\s+")[[1]]
    i_tok <- strsplit(trimws(sub("^\\s*IR INTENSITY:", "", ir_lines[i])), "\\s+")[[1]]

    # An imaginary mode is printed as "1156.17 I": a lone "I" token after the value.
    is_i  <- f_tok == "I"
    vals  <- f_tok[!is_i]
    flags <- logical(length(vals))
    if (any(is_i)) {
      pos <- which(is_i)
      if (pos[1] == 1) stop("Malformed FREQUENCY row: ", freq_lines[i])
      # position of the numeric token that each "I" follows, in the "vals" indexing
      flags[pos - seq_along(pos)] <- TRUE
    }
    f_num <- suppressWarnings(as.numeric(vals))
    i_num <- suppressWarnings(as.numeric(i_tok))
    if (anyNA(f_num) || anyNA(i_num) || length(f_num) != length(i_num)) {
      stop("Could not parse frequency/intensity row pair:\n", freq_lines[i], "\n", ir_lines[i])
    }
    freq <- c(freq, ifelse(flags, -f_num, f_num))
    imag <- c(imag, flags)
    ir   <- c(ir, i_num)
  }

  data.frame(
    mode = seq_along(freq),
    frequency_cm1 = freq,
    imaginary = imag,
    ir_intensity_debye2_amu_ang2 = ir,
    ir_intensity_km_mol = ir * GAMESS_IR_TO_KM_MOL,
    stringsAsFactors = FALSE
  )
}


#' Build a broadened IR spectrum from a table of modes
#'
#' Each mode becomes a band of unit-area shape multiplied by its intensity, so
#' the area under each band equals its IR intensity (km/mol).
#'
#' The calculation yields only stick positions and intensities. The band shape
#' and FWHM are presentation choices meant to resemble an experimental spectrum;
#' nothing in the calculation determines them.
#' Physical background: a Lorentzian is the profile of homogeneous (lifetime,
#' collisional, dephasing) broadening and is a reasonable starting point for
#' solution-phase bands, which is what PCM/SMD calculations model. A Gaussian
#' is the profile of inhomogeneous broadening (e.g. a spread of local
#' environments). Real condensed-phase bands are usually a mixture (Voigt), for
#' which "pseudo-voigt" is a practical stand-in. Lorentzian tails fall off
#' slowly (1/x^2) and raise the baseline between bands more than real spectra
#' do. None of these shapes describes a gas-phase band, whose width is
#' dominated by unresolved rotational (P/Q/R) structure; for gas-phase
#' comparisons set the FWHM from the experimental resolution instead.
#'
#' @param modes Data frame from parse_gamess_frequencies().
#' @param scale_factor Frequency scale factor. Required; no default.
#' @param fwhm Full width at half maximum of each band, cm^-1.
#' @param lineshape "lorentzian" (default), "gaussian" or "pseudo-voigt".
#' @param eta Lorentzian fraction (0-1) of the pseudo-Voigt profile, which is
#'   eta * Lorentzian + (1 - eta) * Gaussian, both of the same FWHM. Used only
#'   when lineshape = "pseudo-voigt"; default 0.5.
#' @param x_range Wavenumber range of the output grid, cm^-1.
#' @param step Grid spacing, cm^-1.
#' @param imaginary "error" (default) stops if imaginary modes are present;
#'   "exclude" drops them.
#' @param min_frequency Modes with scaled frequency <= this value are dropped
#'   (default 0). This removes the zeroed translation/rotation modes written by
#'   GAMESS when PURIFY is used; raise it if residual low modes are present.
#' @return A data frame with columns wavenumber_cm1 and intensity_km_mol
#'   (absorbance-like; plot_ir_spectrum() rescales it). The stick positions are
#'   attached as attribute "sticks".
build_ir_spectrum <- function(modes, scale_factor,
                              fwhm = 20,
                              lineshape = c("lorentzian", "gaussian", "pseudo-voigt"),
                              eta = 0.5,
                              x_range = c(400, 4000),
                              step = 1,
                              imaginary = c("error", "exclude"),
                              min_frequency = 0) {
  lineshape <- match.arg(lineshape)
  imaginary <- match.arg(imaginary)

  if (missing(scale_factor) || !is.numeric(scale_factor) ||
      length(scale_factor) != 1 || is.na(scale_factor)) {
    stop("scale_factor must be supplied as a single number (look it up for ",
         "your functional/basis set; it is not defaulted)")
  }
  if (scale_factor < 0.7 || scale_factor > 1.1) {
    stop("scale_factor = ", scale_factor, " is outside the plausible range 0.7-1.1")
  }
  needed <- c("frequency_cm1", "imaginary", "ir_intensity_km_mol")
  if (!all(needed %in% names(modes))) {
    stop("modes must contain columns: ", paste(needed, collapse = ", "))
  }
  if (!is.numeric(eta) || length(eta) != 1 || is.na(eta) || eta < 0 || eta > 1) {
    stop("eta must be a single number between 0 and 1")
  }
  if (fwhm <= 0 || step <= 0 || x_range[1] >= x_range[2]) stop("Invalid fwhm, step or x_range")

  if (any(modes$imaginary)) {
    if (imaginary == "error") {
      stop(sum(modes$imaginary), " imaginary mode(s) present (",
           paste(round(abs(modes$frequency_cm1[modes$imaginary]), 1), collapse = ", "),
           " cm-1 i). This is a saddle point, not a minimum. ",
           "Use imaginary = \"exclude\" to drop them explicitly.")
    }
    modes <- modes[!modes$imaginary, ]
  }

  sticks <- data.frame(
    wavenumber_cm1 = modes$frequency_cm1 * scale_factor,
    intensity_km_mol = modes$ir_intensity_km_mol
  )
  sticks <- sticks[sticks$wavenumber_cm1 > min_frequency, ]

  x <- seq(x_range[1], x_range[2], by = step)
  y <- numeric(length(x))
  for (k in seq_len(nrow(sticks))) {
    d <- x - sticks$wavenumber_cm1[k]
    g <- fwhm / 2
    sg <- fwhm / (2 * sqrt(2 * log(2)))
    lor <- (g / pi) / (d^2 + g^2)
    gau <- exp(-d^2 / (2 * sg^2)) / (sg * sqrt(2 * pi))
    shape <- switch(lineshape,
      "lorentzian" = lor,
      "gaussian" = gau,
      "pseudo-voigt" = eta * lor + (1 - eta) * gau)
    y <- y + sticks$intensity_km_mol[k] * shape
  }

  out <- data.frame(wavenumber_cm1 = x, intensity_km_mol = y)
  attr(out, "sticks") <- sticks
  attr(out, "scale_factor") <- scale_factor
  attr(out, "fwhm") <- fwhm
  attr(out, "lineshape") <- if (lineshape == "pseudo-voigt") {
    sprintf("pseudo-voigt (eta %.2f)", eta)
  } else lineshape
  out
}


#' Pick peaks from a broadened spectrum
#'
#' Local maxima of the broadened curve at or above a relative-intensity
#' threshold. Bands that overlap into one maximum are reported once.
#'
#' @param spectrum Output of build_ir_spectrum() or combine_ir_spectra().
#' @param threshold Fraction (0-1) of the tallest peak; default 0.5.
#' @return Data frame with wavenumber_cm1, intensity_km_mol and
#'   relative_intensity, ordered by decreasing wavenumber. Zero rows if none.
pick_ir_peaks <- function(spectrum, threshold = 0.5) {
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    stop("threshold must be a single number between 0 and 1")
  }
  y <- spectrum$intensity_km_mol
  n <- length(y)
  if (n < 3 || max(y) <= 0) stop("Spectrum is empty; no peaks to pick")
  is_max <- c(FALSE, y[-1] > y[-n] & c(y[-c(1, n)] >= y[-c(1:2)], FALSE)[seq_len(n - 1)])
  is_max[1] <- FALSE; is_max[n] <- FALSE
  rel <- y / max(y)
  keep <- which(is_max & rel >= threshold)
  out <- data.frame(wavenumber_cm1 = spectrum$wavenumber_cm1[keep],
                    intensity_km_mol = y[keep],
                    relative_intensity = rel[keep])
  out[order(-out$wavenumber_cm1), ]
}


#' Combine spectra of several components on a molar basis
#'
#' Intensities are per mole of absorber (km/mol), so the combined spectrum is
#' the sum of each component spectrum multiplied by its amount (mole fraction
#' or moles; only the ratios matter for the shape). All spectra must share
#' exactly the same wavenumber grid.
#'
#' @param spectra Named list of spectra from build_ir_spectrum().
#' @param amounts Named numeric vector of amounts, same names as spectra.
#' @return Data frame: wavenumber_cm1, intensity_km_mol (the weighted total),
#'   and one weighted column per component. Weighted sticks are attached as
#'   attribute "sticks" (with a component column).
combine_ir_spectra <- function(spectra, amounts) {
  if (!is.list(spectra) || is.null(names(spectra)) || any(names(spectra) == "")) {
    stop("spectra must be a named list")
  }
  if (!is.numeric(amounts) || is.null(names(amounts)) ||
      !setequal(names(amounts), names(spectra))) {
    stop("amounts must be a named numeric vector with the same names as spectra")
  }
  if (any(is.na(amounts)) || any(amounts < 0) || sum(amounts) <= 0) {
    stop("amounts must be non-negative with a positive sum")
  }
  x <- spectra[[1]]$wavenumber_cm1
  for (nm in names(spectra)) {
    if (!identical(spectra[[nm]]$wavenumber_cm1, x)) {
      stop("Spectrum '", nm, "' is on a different wavenumber grid; rebuild all ",
           "with the same x_range and step")
    }
  }
  out <- data.frame(wavenumber_cm1 = x)
  total <- numeric(length(x))
  sticks <- list()
  for (nm in names(spectra)) {
    w <- amounts[[nm]]
    out[[nm]] <- w * spectra[[nm]]$intensity_km_mol
    total <- total + out[[nm]]
    st <- attr(spectra[[nm]], "sticks")
    if (!is.null(st)) {
      st$intensity_km_mol <- st$intensity_km_mol * w
      st$component <- nm
      sticks[[nm]] <- st
    }
  }
  res <- cbind(out[1], intensity_km_mol = total, out[-1])
  attr(res, "sticks") <- do.call(rbind, sticks)
  attr(res, "components") <- names(spectra)
  attr(res, "amounts") <- amounts
  res
}


#' Plot an IR spectrum (base graphics)
#'
#' @param spectrum Output of build_ir_spectrum() or combine_ir_spectra().
#'   Combined spectra also show each component as a coloured line.
#' @param display "transmittance" (default; bands point down, the usual IR
#'   convention) or "absorbance" (bands point up). Both are on a relative
#'   scale with the tallest band spanning the full height.
#' @param show_sticks Draw the unbroadened bands.
#' @param reverse_x Plot wavenumber decreasing left to right (IR convention).
#' @param peaks Optional output of pick_ir_peaks(); labels are drawn at the peaks.
#' @param main Plot title.
plot_ir_spectrum <- function(spectrum, display = c("transmittance", "absorbance"),
                             show_sticks = TRUE, reverse_x = TRUE, peaks = NULL,
                             main = "Calculated IR spectrum") {
  display <- match.arg(display)
  y <- spectrum$intensity_km_mol
  ymax <- max(y)
  if (!is.finite(ymax) || ymax <= 0) stop("Spectrum is empty; nothing to plot")
  flip <- function(r) if (display == "transmittance") 1 - r else r
  x <- spectrum$wavenumber_cm1
  xlim <- range(x)
  if (reverse_x) xlim <- rev(xlim)
  comps <- attr(spectrum, "components")
  sticks <- attr(spectrum, "sticks")

  ylab <- if (display == "transmittance") "Relative transmittance" else "Relative absorbance"
  sub <- if (is.null(comps)) {
    sprintf("scale factor %.3f, %s, FWHM %g cm-1", attr(spectrum, "scale_factor"),
            attr(spectrum, "lineshape"), attr(spectrum, "fwhm"))
  } else {
    paste("molar mixture:", paste(sprintf("%s %g", comps, attr(spectrum, "amounts")[comps]),
                                  collapse = ", "))
  }
  plot(x, flip(y / ymax), type = "l", xlim = xlim, ylim = c(0, 1),
       xlab = expression("Wavenumber / " * cm^-1), ylab = ylab, main = main, sub = sub)
  if (!is.null(comps)) {
    cols <- seq_along(comps) + 1
    for (i in seq_along(comps)) lines(x, flip(spectrum[[comps[i]]] / ymax), col = cols[i], lty = 2)
    legend("bottomleft", legend = c("total", comps), col = c(1, cols),
           lty = c(1, rep(2, length(comps))), bty = "n")
  }
  if (show_sticks && !is.null(sticks) && nrow(sticks) > 0) {
    keep <- sticks$wavenumber_cm1 >= min(xlim) & sticks$wavenumber_cm1 <= max(xlim)
    r <- sticks$intensity_km_mol[keep] / max(sticks$intensity_km_mol)
    base <- if (display == "transmittance") 1 else 0
    segments(sticks$wavenumber_cm1[keep], base, sticks$wavenumber_cm1[keep],
             if (display == "transmittance") 1 - r else r, col = "grey60")
  }
  if (!is.null(peaks) && nrow(peaks) > 0) {
    py <- flip(peaks$intensity_km_mol / ymax)
    text(peaks$wavenumber_cm1, py, labels = sprintf("%.0f", peaks$wavenumber_cm1),
         pos = if (display == "transmittance") 1 else 3, cex = 0.7)
  }
  invisible(spectrum)
}


#' Log file -> broadened IR spectrum in one call
#'
#' Returns the spectrum (invisibly when plotting) and, by default, draws it.
#'
#' @inheritParams parse_gamess_frequencies
#' @inheritParams build_ir_spectrum
#' @param plot Draw the spectrum with plot_ir_spectrum() (default TRUE).
#' @param peak_threshold Label peaks at or above this fraction (0-1) of the
#'   tallest peak, as in pick_ir_peaks(). NULL draws no peak labels.
#' @param display "transmittance" or "absorbance"; passed to plot_ir_spectrum().
#' @param ... Further arguments passed to build_ir_spectrum().
#' @return The spectrum data frame from build_ir_spectrum().
gamess_ir_spectrum <- function(log_file, scale_factor, block = "last",
                               plot = TRUE, peak_threshold = 0.5,
                               display = "transmittance", ...) {
  modes <- parse_gamess_frequencies(log_file, block = block)
  spec <- build_ir_spectrum(modes, scale_factor = scale_factor, ...)
  if (plot) {
    pk <- if (is.null(peak_threshold)) NULL else pick_ir_peaks(spec, peak_threshold)
    plot_ir_spectrum(spec, display = display, peaks = pk, main = basename(log_file))
    return(invisible(spec))
  }
  spec
}
