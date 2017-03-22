#' Displaying multiple ggplot2 objects.
#'
#' The function plots multiple ggplot2 object sin a single page. Original code was provided by R
#' Graphics Cookbook, Practical Recipes for Visualizing Data by Winston Chang, Publisher: O'Reilly
#' Media, Release Date: December 2012, Pages: 416.
#' \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#'
#' @import grid
#'
#' @param ... ggplot objects to be plotted. If \code{plotlist} is provided, the argument is ignored.
#'
#' @param plotlist List of ggplot objects to be plotted. If \code{...} is provided, the argument is
#'   ignored. Default is \code{NULL}.
#'
#' @param file File path for the plot.
#'
#' @param cols Number of columns in the layout. Defaul is \code{1}.
#'
#' @param layout A matrix specifying the layout. If present, \code{cols} is ignored. For example
#'   \code{matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)} will produce one plot in the upper left,
#'   two plots in the upper right and three will go all the way across the bottom. Default is
#'   \code{NULL}.
#'
#' @return The function makes a plot with multiple ggplot objects in a single page.
#'
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <-
      matrix(seq(1, cols * ceiling(numPlots / cols)),
             ncol = cols, nrow = ceiling(numPlots / cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <-
        as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]],
            vp = viewport(
              layout.pos.row = matchidx$row,
              layout.pos.col = matchidx$col))
    }
  }
}
# End of file
