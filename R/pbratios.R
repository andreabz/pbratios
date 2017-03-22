#' Processing Pb isotope ratio data.
#'
#' The package provides some functions to efficiently extract and process Pb isotope ratio data
#' acquired with Perkin Elmer ELAN ICP-MS.
#'
#' The principal functions are \describe{ \item{\code{extract_data}}{ Extracts data from the csv
#' report produced by the mass spectrometer. The only ratios taken into account are 208Pb/207Pb,
#' 206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb ratios.} \item{\code{calc_ratios}}{ Removes possible
#' outliers from the dataset and calculate the average Pb isotope ratio for each sample.}
#' \item{\code{corr_mbf}}{ Corrects the Pb isotope ratio values for instrumental mass bias using the
#' bracketing standard technique.} } Other functions are provided for inspecting possible outliers,
#' evaluating QC samples, saving the final dataset and produce three-isotope plots.
#'
#' @author Andrea Bazzano <andrea.bazzano86@gmail.com>
#' @docType package
#' @name pbratios
#'
#' @seealso \code{\link{extract_data}} \code{\link{calc_ratios}} \code{\link{corr_mbf}}
NULL
