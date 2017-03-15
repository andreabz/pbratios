#' Write a dataset into a .csv file.
#'
#' The function write a dataset into a .csv file with locale IT-it or equivalent.
#'
#' @param data A dataset in the class data.frame or data.table.
#'
#' @return The function save a .csv file in the "output" folder. The filename is
#'   \code{data} without dots (".").
#'
#' @examples
#' save.table(pm10nya)  # save spmnya2012.csv in the "output" folder.
#'
#' @export
#'
save.table <- function(data) {

  if (dir.exists("output") == TRUE) {

  } else {
    dir.create("output")
  }
  write.csv2(data, file = paste0("output/", gsub("\\.", "", deparse(substitute(data))),
                                 ".csv"))
}
# End of file
