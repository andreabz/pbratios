#' Dataset with Pb isotope ratio data.
#'
#' This dataset was obtained processing a raw csv datafile with the functions
#' provided by the package \code{pbratios}.
#'
#' Pb isotope ratios in this dataset were inspected for outliers and corrected for
#' mass bias using the standard bracketing technique.
#' The dataset comprises PM10 samples collected from 2010 to 2014 at Ny-Ålensund
#' (Svalbards, Norwegian Arctic). Additionally, some QC control samples labelled as
#' \code{CRM482} are included in the dataset. These QC samples are used for monitoring
#' the accuracy of the measurement.
#'
#' @format The file has 148 observations and 9 columns.
#'   Columns are labelled as
#'   \describe{
#'     \item{\code{sample}}{ Sample name. The first 2 letters refer to the sampling
#'     location (\code{GB} = Gruvebadet), the next two numbers are a progressive index and
#'     \code{-XX} refers to the year of sampling.}
#'     \item{\code{Pb20x20y}}{ Four columns reporting Pb isotope ratio values for
#'       the ratios 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb, 207Pb/206Pb.}
#'     \item{\code{Pb20x20y.U}}{ Extended uncertainty (\eqn{U = k u} with \eqn{k = 2} for
#'     the 20xPb/20yPb ratio.}
#'   }
#' @author Andrea Bazzano and Marco Grotti.
#' @source Data were published in
#'   Bazzano, Cappelletti, Udisti and Grotti, Long-range transport of atmospheric lead
#'   reaching Ny-Ålesund: Inter-annual and seasonal variations of potential source areas.
#'   Atmospheric Environment, 2016, 139, 11-19.
#'
#' @examples
#' data(pm10nya)
#' summary(pm10nya)
#'
"pm10nya"
