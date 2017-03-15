#' Processing Pb isotope ratio data.
#'
#' The package provides some functions to efficiently extract and process Pb isotope ratio
#' data acquired with a Perkin Elmer ELAN ICP-MS working in isotope ratio mode.
#'
#' The principal functions are
#' \describe{
#' \item{\code{extract.data}}{ Extracts data from the csv report produced by the mass
#'   spectrometer. 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb ratios
#'   will be taken into account.}
#' \item{\code{calc.ratios}}{ Remove outliers from the dataset and summarise the
#'   replicates.}
#' \item{\code{corr.mbf}}{ Corrects the Pb isotope ratio values for instrumental mass
#'   fractionation usingh the bracketing standard technique.}
#' }
#' Other functions are then provided for inspecting possible outliers, evaluating QC
#' samples, saving the final dataset and produce some three-isotope plots.
#'
#' @author Andrea Bazzano <andrea.bazzano86@gmail.com>
#' @docType package
#' @name pbratios
#'
#' @seealso
#'   \code{\link{extract.data}}
#'   \code{\link{calc.ratios}}
#'   \code{\link{corr.mbf}}
NULL
