#' Raw csv dataset with Pb isotope ratio data.
#'
#' This raw dataset was obtained saving as .csv the original .rep file produced by
#' the mass spectrometer Perkin Elmer ELAN ICP-MS. This file is an example of \bold{long}
#' report.
#'
#' Field separator are "," and decimal separator are ".". Standards with known Pb
#' isotope ratios are used to correct the instrumental mass bias by the bracketing
#' standard technique and are labelled as \code{SRM}. QC data are BCR CRM 482 and are
#' labelled as \code{CRM}. Sample data refer to marine particulate matter collected in
#' the coastal marine enviroment of Kongsfjorden (Svalbards, Norvegian Arctic) in
#' 2012.
#'
#' @name spmnya_2012
#' @format The file has 5110 rows and 7 columns.
#' @author Andrea Bazzano and Marco Grotti.
#' @source Extracted data were published in
#'   Bazzano, Rivaro, Soggia, Ardini and Grotti, Anthropogenic and natural sources
#'   of particulate trace elements in the coastal marine environment of Kongsfjorden,
#'   Svalbard. Marine Chemistry, 2014, 163, 28-35.
#'
#' @examples
#' file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract_data(file.long, report = "long")
#'
#' @seealso
#' \code{\link{extract_data}}
#'
NULL
