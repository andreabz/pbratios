#' Checking quality control criteria.
#'
#' The function checks quality control criteria for Pb isotope ratios measured in BCR CRM
#' 482 QC samples.
#'
#' @import data.table
#' @import ggplot2
#'
#' @param data a dataset of Pb isotope ratio values obtained by the function
#'         \code{corr.mbf}.
#'         The dataset must contain at least one sample labelled as \code{CRM}.
#'         The CRM will be assumed as BCR CRM 482 and values reported by
#'         Cloquet, C., Carignan, J., Libourel, G., 2006. Atmospheric pollutant
#'         dispersion around an urban area using trace metal concentrations and Pb
#'         isotopic compositions in epiphytic lichens. Atmos. Environ. 40, 574–587
#'         will be considered as reference values.
#' @return The function returns a list containing
#'    \itemize{
#'    \item{\code{data}}{ Corrected Pb isotope ratios measured for BCR CRM 482 QC
#'      samples.}
#'    \item{\code{summary}}{ Summary statistics from "data".}
#'    \item{\code{error}}{ Summary statistics for experimental data vs reference values.
#'       Errors are reported on a permil scale.}
#'    Additionally, the function plots the datapoints or boxplots (for \eqn{N > 10}).
#' }
#'
#' @examples
#' data(pm10nya)
#' print(qc.check(pm10nya))
#'
#' @seealso
#'   \code{\link{corr.mbf}}
#'
#' @export
#'
qc.check <- function(data) {

  # Extracting CRM from dataset obtained by calc.ratios function
  dt.qc <- data[grep("CRM", sample)]

  # Creating a plot for Pb208207 and Pb206207
  ylabels <- c(
    bquote(phantom(.) ^ 208 * Pb ~ "/" * phantom(.) ^ 207 * Pb),
    bquote(phantom(.) ^ 206 * Pb ~ "/" * phantom(.) ^ 207 * Pb),
    bquote(phantom(.) ^ 208 * Pb ~ "/" * phantom(.) ^ 206 * Pb),
    bquote(phantom(.) ^ 207 * Pb ~ "/" * phantom(.) ^ 206 * Pb)
  )

  crm.values <- data.frame(
    "Pb208207" = 2.40783,
    "Pb206207" = 1.13108,
    "Pb208206" = 2.12879,
    "Pb207206" = 0.88411
  )

  ydata <- names(dt.qc)[c(2, 4, 6, 8)]

  plots <- list()  # new empty list

  for (i in seq_along(ydata)) {
    p1 <-
      ggplot(data = dt.qc, aes_string(x = factor(0), y = ydata[i])) +
      geom_hline(data = crm.values, aes_string(yintercept = ydata[i])) +
      ylab(ylabels[[i]]) +
      xlab("") +
      theme_bw() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    plots[[i]] <- p1  # add each plot into plot list

  }

  plots <- lapply(plots, function(x) {
    if (dt.qc[, .N] >= 10) {
      x + geom_boxplot()
    } else {
      x + geom_jitter()
    }
  }
  )

  multiplot(plotlist = plots, cols = 2)

  # Calculating summary statistics
  my.summary <- function(x, error = FALSE) {
    if (error == FALSE){
      result <- c(
        Mean   = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD     = sd(x, na.rm = TRUE),
        CI95   = qt(0.975, length(x) - 1) * sd(x, na.rm = TRUE) / length(x),
        RSD    = 100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
        Min    = min(x, na.rm = TRUE),
        Max    = max(x, na.rm = TRUE),
        N      = length(x)
      )
    } else {
      result <- c(
        Mean   = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD     = sd(x, na.rm = TRUE),
        CI95   = qt(0.975, length(x) - 1) * sd(x, na.rm = TRUE) / length(x),
        RMSE   = sqrt(mean((x - mean(x, na.rm = TRUE))^2)),
        Min    = min(x, na.rm = TRUE),
        Max    = max(x, na.rm = TRUE)
      )
    }
  }

  dt.error <- data.table(10^3 * (sweep(dt.qc[, ydata, with = FALSE], 2,
                                       t(crm.values), `/`) - 1))

  qc.fin <- list(
    data    = dt.qc[, .(Pb208207, Pb208207.U, Pb206207, Pb206207.U,
                        Pb208206, Pb208206.U, Pb207206, Pb207206.U)],
    summary = dt.qc[, sapply(.SD, my.summary), .SDcols = ydata],
    error   = dt.error[, sapply(.SD, my.summary, error = TRUE)]
  )
}
# End of file
