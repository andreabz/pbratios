#' Saving a data set as csv file.
#'
#' The function saves a data set into a .csv file with locale IT-it or equivalent.
#'
#' @param data A dataset in the class data.frame or data.table.
#'
#' @return The function saves a csv file in the "output" folder. The filename is \code{data} without
#'   dots (".").
#'
#' @examples
#' \dontrun{save_table(pm10nya)}  # save spmnya2012.csv in the "output" folder.
#'
#' @export
#'
save_table <- function(data) {

  if (dir.exists("output") == TRUE) {

  } else {
    dir.create("output")
  }
  write.csv2(data, file = paste0("output/", gsub("\\.", "", deparse(substitute(data))),
                                 ".csv"))
}
# End of file
