#' Correcting instrumental mass bias.
#'
#' The function corrects Pb isotope ratio for mass bias using the standard bracketing approach.
#'
#' The standard bracketing techniques uses the average mass bias factor measured in standard
#' solutions with known isotope ratio values to correct the isotope ratios measured in samples. As
#' mass bias factor is influenced by temporal random fluctuations, standard solutions must be
#' analysed just before and after the samples for which the correction will be applied.
#'
#' @import data.table
#'
#' @param data A data.table produced by the calc.ratios function and containing average Pb isotope
#'   data. The dataset must have a column \code{sample} containing the sample ID, the columns
#'   \code{Pb208207}, \code{Pb206207}, \code{Pb208206} and \code{Pb207206} containing the measured
#'   Pb isotope ratio values for 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb,
#'   respectively. For each ratio also a columns \code{Pb20x20y.se}, containing standard errors are
#'   required. Bracketing standards must be labelled as “SRM981” and are assumed to be diluted
#'   solutions of NIST SRM 981. Certificate values for these ratios are reported at
#'   \url{https://www-s.nist.gov/srmors/certificates/view_certPDF.cfm?certificate=981}. The function
#'   will evaluate data from the first to the last \code{SRM} and samples outside these range will
#'   be ignored.
#'
#' @return A data.table containing the Pb isotope ratios corrected for the mass bias using the
#'   standard bracketing technique. Extended uncertainty (\eqn{U = k u}, \eqn{k = 2}) is also
#'   provided for each ratio and it is stored in \code{Pb20x20y.U} columns. Uncertainty budget take
#'   into account the relative standard errors for the sample, the mass bias factor and the
#'   certified values for NIST SRM 981.
#'
#' @examples
#' file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract_data(file.long, report = "long")
#' spmnya.2012avg <- calc_ratios(spmnya.2012)
#' spmnya.2012cor <- corr_mbf(spmnya.2012avg)
#'
#' @seealso \code{\link{extract_data}} \code{\link{calc_ratios}}
#'
#' @export
#'
corr_mbf <- function(data) {

  id <- grep("SRM", data$sample)
  data <- data[min(id):max(id)]

  # Calculating the number of samples between consecutive SRMs
  srms <-
    which(grepl("SRM", data$sample) & !grepl("test", data$sample))
  samples <- diff(srms) - 1

  # Splitting the dataset into SRMs and samples
  dt.srms <- data[srms][, id := 1:.N]
  dt.samples <- data[-srms]

  # Generating a vector of ids for the first and second SRMs for bracketing
  std1.id <- rep(dt.srms$id[-length(dt.srms$id)], samples)
  std2.id <- std1.id + 1

  dt.samples <- dt.samples[, ':=' (std1 = std1.id,
                                   std2 = std2.id)]

  # Calculating the mass bias factor
  dt.srms[, ':='(
    Pb208207.mbf = Pb208207 / 2.37043,
    Pb206207.mbf = Pb206207 / 1.09332,
    Pb208206.mbf = Pb208206 / 2.16810,
    Pb207206.mbf = Pb207206 / 0.914642
  )]

  # Calculating corrected Pb isotope ratios
  ratio.names <- c("Pb208207", "Pb206207", "Pb208206", "Pb207206")
  ratio.mbf <- paste0(ratio.names, ".mbf")
  ratio.unc <- paste0(ratio.names, ".U")
  ratio.se  <- paste0(ratio.names, ".se")

  # Certified Pb ratios for NIST SRM 981
  cert.ratio <- c(2.37043, 1.09332, 2.16810, 0914642)

  # Uncertainty for the NIST SRM 981 Pb ratios
  cert.unc <- c(1.02 * 10 ^ -4, 1.16 * 10 ^ -4, 1.28 * 10 ^ -4, 1.16 * 10 ^ -4)

  for (i in seq_along(ratio.names)) {
  # Corrected ratios
    dt.samples[, ratio.names[i]] <- dt.samples[,
                 ratio.names[i], with = FALSE] /
                      ((dt.srms[dt.samples$std1, ratio.mbf[i], with = FALSE] +
                        dt.srms[dt.samples$std2, ratio.mbf[i], with = FALSE]) / 2)

 # Extended Uncertainty
    dt.samples[, ratio.unc[i]] <- 2 * dt.samples[, ratio.names[i], with = FALSE] *
      sqrt(
  # Contribution to uncertainty from sample standard error
              (dt.samples[, ratio.se[i], with = FALSE] /
               dt.samples[, ratio.names[i], with = FALSE]) ^ 2 +

  # Contribution to uncertainty from bracketing standards
              ((dt.srms[dt.samples$std1, ratio.se[i], with = FALSE] +
                dt.srms[dt.samples$std2, ratio.se[i], with = FALSE]) /
               (dt.srms[dt.samples$std1, ratio.names[i], with = FALSE] +
                dt.srms[dt.samples$std2, ratio.names[i], with = FALSE])) ^ 2 +

  # Contribution to uncertainty from NIST SRM 981
                cert.unc[i] ^ 2)
  }

  # Reorder the dataset
  dcols <-
            c(
              "sample",
              "Pb208207",
              "Pb208207.U",
              "Pb206207",
              "Pb206207.U",
              "Pb208206",
              "Pb208206.U",
              "Pb207206",
              "Pb207206.U"
              )

  dt.samples <- dt.samples[, .SD, .SDcols = dcols]
}
# End of file
