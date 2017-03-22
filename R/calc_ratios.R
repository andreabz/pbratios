#' Removing outliers and summarising Pb isotope ratio
#'
#' The function removes replicates outside the \eqn{median ± 2.5 mad} range from raw Pb isotope
#' ratios and calculates the average and standard error for each sample.
#'
#' @import data.table
#'
#' @param data A data.table produced by the \code{extract.data} function, containing raw Pb isotope
#'   data. The dataset must have a column "sample" containing the sample name and the columns
#'   \code{Pb208207}, \code{Pb206207}, \code{Pb208206}, \code{Pb207206} containing the measured Pb
#'   isotope ratio values for 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb, respectively.
#'
#' @return A data.table with average Pb isotope ratio values for each sample. The number of
#'   replicates outside the \eqn{median ± 2.5 mad} range for each sample are printed on screen.
#'   Additionally, the standard errors for the measured Pb isotope ratios are stored in
#'   \code{Pb20x20y.se} columns.
#'
#' @examples
#' file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract_data(file.long, report = "long")
#' spmnya.2012avg <- calc_ratios(spmnya.2012)
#'
#' @seealso \code{\link{extract_data}}
#'
#' @export
calc_ratios <- function(data) {

  # Calculate the raw ratios
  dt.ratio <- data[,
                   # Selecting the column to use in next steps
                   .(sample,
                     Pb208207,
                     Pb206207,
                     Pb208206,
                     Pb207206)]

  # Removal of values outside the median +- 2.5 mad range
  dt.ratio <-
    dt.ratio[,  ':='(
                      Pb208207.out = out_flag(Pb208207),
                      Pb206207.out = out_flag(Pb206207),
                      Pb208206.out = out_flag(Pb208206),
                      Pb207206.out = out_flag(Pb207206)
                    ),
                      by = sample]

  dt.ratio <- dt.ratio[Pb208207.out == TRUE,
                       Pb208207 := NA][
                       Pb206207.out == TRUE,
                       Pb206207 := NA][
                       Pb208206.out == TRUE,
                       Pb208206 := NA][
                       Pb207206.out == TRUE,
                       Pb207206 := NA][, sample:Pb207206]

  # Print the number of outliers for each sample
  print(dt.ratio[, lapply(.SD, function (x) sum(is.na(x))), by = sample], nrows = 300)

  # Summarising the dataset by sample name

  dcols <-
            c(
              "sample",
              "Pb208207",
              "Pb208207.se",
              "Pb206207",
              "Pb206207.se",
              "Pb208206",
              "Pb208206.se",
              "Pb207206",
              "Pb207206.se"
            )

  dt.ratio <- dt.ratio[, ':='
  # Calculating the number of replicates
                   (
                     Pb208207.n = sum(!is.na(Pb208207)),
                     Pb206207.n = sum(!is.na(Pb206207)),
                     Pb208206.n = sum(!is.na(Pb208206)),
                     Pb207206.n = sum(!is.na(Pb207206))
                   ), by = sample][, ':='
  # Calculating the standard error
                   (
                     Pb208207.se = sd(Pb208207, na.rm = TRUE) / sqrt(Pb208207.n),
                     Pb206207.se = sd(Pb206207, na.rm = TRUE) / sqrt(Pb206207.n),
                     Pb208206.se = sd(Pb208206, na.rm = TRUE) / sqrt(Pb208206.n),
                     Pb207206.se = sd(Pb207206, na.rm = TRUE) / sqrt(Pb207206.n)
                    ), by = sample][,
  # Summarising the results by sample name
                     lapply(.SD, mean, na.rm = T), by = sample][,
  # Ordering the columns
                     .SD, .SDcols = dcols]

}
# End of file
