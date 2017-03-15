#' Identifies possible outliers.
#'
#' Identifies possible outliers based on the \eqn{median ± 2.5 mad} range
#' criterion. This function is used internally by the \code{calc.ratios} function.
#'
#' @param x A vector of numeric data.
#'
#' @return A logical vector that is \code{TRUE} if the values is outside the
#'    \eqn{median ± 2.5 mad} range.
#'
#' @examples
#' a <- rnorm(100)
#' b <- out.flag(a)
#'
#' @seealso
#' \code{\link{calc.ratios}}
#'
#' @export
#'
out.flag <- function(x) {

  median.data <- median(x, na.rm = TRUE)
  mad.data <- mad(x, na.rm = TRUE)
  outliers <-
    x <= median.data - 2.5 * mad.data |
    x >= median.data + 2.5 * mad.data
}
# End of file
