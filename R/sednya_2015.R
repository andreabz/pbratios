#' Raw csv dataset with Pb isotope ratio data.
#'
#' This raw dataset was obtained saving as .csv the original .rep file produced by
#' the mass spectrometer Perkin Elmer ELAN ICP-MS. This file is an example of \bold{short}
#' report.
#'
#' Field separator are "," and decimal separator are ".". Standards with known Pb
#' isotope ratios are used to correct the instrumental mass bias by the bracketing
#' standard technique and are labelled as \code{SRM}. QC data are BCR CRM 482 and are
#' labelled as \code{CRM}. Sample data refer to size fractionated sediment samples
#' collected in the coastal marine enviroment of Kongsfjorden (Svalbards, Norvegian Arctic)
#' in 2012.
#'
#' @name sednya_2012
#' @format The file has 1568 rows and 7 columns.
#' @author Andrea Bazzano, Francesco Soggia and Marco Grotti.
#' @source A selection of the extracted data were submitted as:
#'   Grotti, Soggia, Ardini, Bazzano, Moroni, Vivani, Cappelletti and Misic,
#'   Trace elements in surface sediments from Kongsfjorden, Svalbard: occurrence,
#'   sources and bioavailability.
#'   International Journal of Environmental Analytical Chemistry, 2017, submitted.
#'
#' @examples
#' file.short <- system.file("extdata", "sednya_2015.csv", package = "pbratios")
#' sednya.2015 <- extract.data(file.short, report = "short")
#'
#' @seealso
#' \code{\link{extract.data}}
#'
NULL