#' Inspecting Pb isotope ratio replicates.
#'
#' The function provides visual inspection of raw Pb isotope ratio replicates for each sample.
#' Possible outliers can be easily identified.
#'
#' @import data.table
#' @import ggplot2
#'
#' @param data A data.table produced by the function \code{extract.data}. The dataset should contain
#'   a \code{sample} column with sample names, \code{Pb20x} columns (\eqn{x = 6, 7, 8}) with the
#'   measured intensities for each isotope, \code{Pb20x20y} columns storing 20xPb/20yPb raw isotope
#'   ratios (\eqn{x, y = 6, 7, 8}).
#'
#' @param idname A string identical to a sample name. (i.e. \code{"P40 I5"}).
#'
#' @return The function returns a list containing \itemize{ \item{\code{intensities}}{ Raw
#'   intensities for the monitored isotopes.} \item{\code{ratios}}{ Raw Pb isotope ratios with flag
#'   = \code{TRUE} if the value falls outside the \eqn{median ± 2.5 mad} range}. } Additionally, the
#'   function returns a plot with a panel for each Pb isotope ratio showing data points for each
#'   replicates. The green line is the median, yellow lines represent \eqn{median ± 2 mad} and red
#'   lines \eqn{median ± 3 mad}. Values inside the \eqn{median ± 2.5 mad} range are represented as
#'   points, possible outliers as *.
#'
#' @examples
#' file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract_data(file.long, report = "long")
#' check_outliers(spmnya.2012, "II.B.50m")
#'
#' @seealso \code{\link{calc_ratios}}
#'
#' @export
check_outliers <- function(data, idname){

  # Subsetting the data
  idname <- paste0("^", idname, "$")
  dt.ext <- data[grep(idname, sample)]

  # Outliers are marked as TRUE
  dt.out <- dt.ext[,  ':='(
                            Pb208207.out = out_flag(Pb208207),
                            Pb206207.out = out_flag(Pb206207),
                            Pb208206.out = out_flag(Pb208206),
                            Pb207206.out = out_flag(Pb207206))][,
                            .(sample, Pb208207, Pb208207.out, Pb206207, Pb206207.out,
                            Pb208206, Pb208206.out, Pb207206, Pb207206.out)]

  #Plots with data, median, median +- 2 mad and median +- 3 mad
  ylabels <- c(bquote(phantom(.) ^ 208 * Pb ~ "/" * phantom(.) ^ 207 * Pb),
               bquote(phantom(.) ^ 206 * Pb ~ "/" * phantom(.) ^ 207 * Pb),
               bquote(phantom(.) ^ 208 * Pb ~ "/" * phantom(.) ^ 206 * Pb),
               bquote(phantom(.) ^ 207 * Pb ~ "/" * phantom(.) ^ 206 * Pb))

  ydata <- names(dt.out)[c(2, 4, 6, 8)]
  ydata.out <- paste0(ydata, ".out")

  h <- c(0,-2, 2,-3, 3)

  hcol <- c("#009E73", "#E69F00", "#E69F00", "red", "red")

  hline.values <- dt.out[, lapply(.SD, function(x) median(x) + h * mad(x)),
                         .SDcols = ydata]

  plots <- list()  # new empty list

  for (i in seq_along(ydata)) {
    p1 <- ggplot(dt.out, aes_string(x = seq(1, dt.out[, .N]), y = ydata[i])) +
          geom_point(aes_string(shape = ydata.out[i])) +
          scale_shape_manual(values = c(19, 8)) +
          geom_hline(data = hline.values,
                 aes_string(yintercept = ydata[i]),
                 color = hcol) +
      # Axis labels and theme
          xlab("Replicate") +
          ylab(ylabels[[i]]) +
          scale_x_continuous(breaks = seq(1, dt.out[, .N])) +
          theme_bw(base_size = 10) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
                )

        plots[[i]] <- p1  # add each plot into plot list

  }

  # Output
  multiplot(plotlist = plots, cols = 2)

  list(intensities = dt.ext[, .(sample, Pb208, Pb207, Pb206)],
       ratios = dt.out[,!"sample", with = FALSE])
}

# End of file
