#' Correcting for instrumental mass bias.
#'
#' The function corrects Pb isotope ratio for mass bias using the standard bracketing
#' approach.
#'
#' @import data.table
#'
#' @param data A data.table produced by the calc.ratios function, containing average Pb
#'     isotope data. The dataset must have a column \code{sample} containing the sample
#'     name, the columns \code{Pb208207}, \code{Pb206207}, \code{Pb208206} and
#'     \code{Pb207206} containing the measured Pb isotope ratio values for 208Pb/207Pb,
#'     206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb, respectively. For each ratio also a
#'     column \code{Pb20x20y.se} containing the standard error is requested.
#'     The data.table should start with a \code{SRM} sample, however, as it is considered
#'     good practice to start the experimental sequence with the measurement of a blank
#'     sample, the removal of such sample has to be performed manually before running
#'     the \code{corr.mbf} function. This can be achieved
#'     by using
#'     \itemize{
#'     \item{\code{data <- data[-grep("NO3$", sample), ]}}{ for blank samples labelled as
#'     \code{"1\%HNO3"};}
#'     \item{\code{data <- data[-grep("BK$", sample), ]}}{ for blank samples labelled as
#'     \code{"BK"}.}
#'     }
#'
#' @return a data.table containing the Pb isotope ratios corrected for the mass bias.
#'   The correction method is the bracketing technique. Bracketing standards should be
#'   labelled as \code{SRM} followed by a progressive integer (the calc.ratios function
#'   will rename the SRM samples accordingly). Each Pb isotope ratios is also corredated
#'   with the extended uncertainty. These values are stored in the columns
#'   \code{Pb20x20y.U} and are calculated by error propagation taking into account the
#'   sample standard error, the mass bias factor standard error and the uncertainty in
#'   the certified values for NIST SRM 981.
#'   \bold{Attention:} no warning must be produced by the function.
#'
#' @examples
#' file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract.data(file.long, report = "long")
#' spmnya.2012avg <- calc.ratios(spmnya.2012)
#' spmnya.2012cor <- corr.mbf(spmnya.2012avg)
#'
#' @seealso
#' \code{\link{extract.data}}
#' \code{\link{calc.ratios}}
#'
#' @export
#'
corr.mbf <- function(data) {

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
  ratio.names <-
    c("Pb208207", "Pb206207", "Pb208206", "Pb207206")
  ratio.mbf <- paste0(ratio.names, ".mbf")
  ratio.unc <- paste0(ratio.names, ".U")
  ratio.se <- paste0(ratio.names, ".se")

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

  dt.final <- dt.samples[, .SD, .SDcols = dcols]
}
# End of file
