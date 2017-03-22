#' Identifying possible outliers.
#'
#' The function identifies possible outliers based on the \eqn{median ± 2.5 mad} range criterion.
#' This function is used internally by the \code{calc.ratios} function.
#'
#' @param x A vector of numeric data.
#'
#' @return A logical vector. \code{TRUE} indicates that the corresponding value is outside the
#'   \eqn{median ± 2.5 mad} range.
#'
#' @examples
#' a <- rnorm(100)
#' b <- out_flag(a)
#'
#' @seealso \code{\link{calc_ratios}}
#'
#' @export
#'
out_flag <- function(x) {

  median.data <- median(x, na.rm = TRUE)
  mad.data <- mad(x, na.rm = TRUE)
  outliers <-
    x <= median.data - 2.5 * mad.data |
    x >= median.data + 2.5 * mad.data
}
# End of file
