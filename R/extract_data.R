#' Extracting data from csv files produced by Perkin Elmer Elan ICP-MS.
#'
#' Extracts 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb and 207Pb/206Pb ratio data from the
#' raw .csv file produced by the instrument Perkin Elmer Elan ICP-MS working in isotope
#' ratio mode.
#'
#' @import data.table
#'
#' @param filename a dataset saved in csv format with "," as field separator and "." as
#'     decimal separator. Such dataset is derived from .rep files provided by the
#'     instrument by opening it with spreadsheet editor and saving it as .csv file using
#'     a local setting = EN-US or equivalent.
#'     The .csv file must contain the sample name, a summary of the measuring conditions,
#'     the raw intensities for each monitored isotopes and a final summary with average
#'     signal intensities and isotope ratios. These pieces of information are repeated
#'     for each sample. Bracketing standards are assumed to be NIST SRM 981 and should
#'     be labelled as \code{SRM}. QC samples are assumed to be BCR CRM 482 and should be
#'     labbelled as \code{CRM}.
#'
#' @return a data.table containing raw intensities and raw Pb isotope ratios
#'     for each sample. Provided isotope ratios are 208Pb/207Pb, 206Pb/207Pb, 208Pb/206Pb
#'     and 207Pb/206Pb.
#'     Note that these ratios have still to be inspected for outliers, summarised and
#'     corrected for mass bias.
#'
#' @examples
#' file <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#' spmnya.2012 <- extract.data(file)
#'
#' @export
#'
extract.data <- function(filename) {

  # Loading the file
  data <- fread(filename, header = FALSE, na.strings = "")

  # Counting rows between two reports and rows occupied by the summary
  rows <- count.rows(data)

  # Extracting and cleaning the sample names

  # Extracting the sample name
  sampleid <- data[grepl("Sample ID", V1), .(V2)]

  # Fixing names for SRMs and CRMs
  srm_id <-
    grepl("SRM", sampleid$V2) & !grepl("test", sampleid$V2)
  crm_id <- grepl("CRM", sampleid$V2)

  sampleid[srm_id, V2 := paste("SRM981", 1:.N, sep = "_")]
  sampleid[crm_id, V2 := paste("CRM482", 1:.N, sep = "_")]

  # Cleaning the dataset

  # Setting the sample name and removing summary
  data <-
    data[, V1 := rep(sampleid$V2, rows$report.rows)][-rows$fullsummary]

  # Removing empty and useless rows
  data <- data[!is.na(V3), V1:V4][-grep("Analyte", V2)]

  # Rename the columns
  data <- setnames(
    data,
    old = c("V1", "V2", "V3", "V4"),
    new = c("sample", "element", "isotope", "intensity")
  )

  data <- data[, nuclide := paste0(element, isotope)][,
                                                      .(sample, nuclide, intensity)]

  # Assigning each column to its right class
  data.factor <- c("sample", "nuclide")
  data[, (data.factor) := lapply(.SD, as.factor),
       .SDcols = data.factor][, intensity := as.double(intensity)] # S values <- NAs

  # Put the dataset in wide format
  lvls <- as.character(unique(data$sample))
  data.clean <- dcast(data[, id := 1:.N, by = .(sample, nuclide)],
                      id + sample ~ nuclide, value.var = 'intensity')[,
                                                                      sample := factor(sample, levels = lvls)][order(sample)]

  # Calculate the raw ratios
  data.clean[, c("Pb208207",
                 "Pb206207",
                 "Pb208206",
                 "Pb207206") :=
               .(Pb208 / Pb207,
                 Pb206 / Pb207,
                 Pb208 / Pb206,
                 Pb207 / Pb206)]
}
# End of function

#' Counting the rows of each report in the raw .csv file
#'
#' The function counts the rows between two consecutive "Isotope Ratio Report" and the
#' rows required by the final "Summary" for each sample. This function is used internally
#' by extract.data.
#'
#' @param data a dataset saved in csv format with "," as field separator and "." as
#'     decimal separator. Such dataset is derived from .rep files provided by the
#'     instrument by opening it with spreadsheet editor and saving it as .csv file
#'     using a local setting = EN-US or equivalent.
#'     The .csv file must contain the sample name, a summary of the measuring conditions,
#'     the raw intensities for each monitored isotopes and a final summary with average
#'     signal intensities and isotope ratios. These pieces of information are repeated
#'     for each sample.
#'     The report start with "Isotope Ratio Report", whereas the final summary is
#'     announced by "Summary".
#'
#' @return a list containing
#'    \itemize{
#'    \item{report.rows}{ number of rows occupied by each report;}
#'    \item{fullsummary}{ number of rows occupied by the summary.}
#'    }
#'
count.rows <- function(data = data) {

  data.rows <- data[, .N]

  id <- grep("Report", data$V1)
  summary.rows <- grep("Summary", data$V1)

  id.rows <- length(id)
  id[id.rows + 1] <- data.rows + 1

  id.adjusted <- id
  id.adjusted[id.rows + 1] <- data.rows

  n.rows <- diff(id)

  fullsummary.rows <- do.call(c, mapply(seq, summary.rows, id.adjusted[-1] - 1))
  fullsummary.rows.length <- length(fullsummary.rows)
  fullsummary.rows[fullsummary.rows.length + 1] <- data.rows

  list(report.rows = n.rows, fullsummary = fullsummary.rows)
}
# End of file
