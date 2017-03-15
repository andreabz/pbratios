#' Script for generating the \code{pm10nya} dataset.
#'
#' This script produces the \code{pm10nya} dataset from the relative raw csv file.
#' Require the libraries \code{data.table}, \code{dplyr} and \code{devtools}.
#'
#' @import data.table
#' @import dplyr
#' @import devtools
#'
pm10nya <- "data-raw/pm10nya-raw.csv" %>%  # filename
              extract.data %>%  # extracting data
              calc.ratios %>%  # calculating mean ratios
              .[-grep("NO3$", sample),] %>%  # removing blanks at the beginning of each runs
              corr.mbf %>%  # correcting ratios for mass bias
              .[-grep("BR", sample)]  # removing blanks
use_data(pm10nya)  # saving the dataset as .rda file
# End of file
