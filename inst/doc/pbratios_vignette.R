## ----installation, eval=FALSE--------------------------------------------
#  devtools::install_github("andreabz/pbratios", build_vignettes = TRUE)

## ----file.short----------------------------------------------------------
file.short <- system.file("extdata", "sednya_2015.csv", package = "pbratios")

## ----file.long-----------------------------------------------------------
file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")

## ----extrac_data, message=FALSE------------------------------------------
library(pbratios)
spmnya <- extract_data(file.long, report = "long")
head(spmnya)

## ----calc_ratios, results='hide'-----------------------------------------
spmnya.ratios <- calc_ratios(spmnya)
head(spmnya.ratios)

## ----spmnya table, echo=FALSE--------------------------------------------
head(spmnya.ratios)

## ----corr_mbf, results='asis'--------------------------------------------
spmnya.corr <- corr_mbf(spmnya.ratios)
knitr::kable(head(spmnya.corr), digits = 4, pad = 0)

## ----all_dplyr, eval=FALSE-----------------------------------------------
#  file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#  spmnya <- file.long %>%
#            extract_data(report = "long") %>%
#            calc_ratios %>%
#            corr_mbf
#  head(spmnya)

## ----all_ratios, eval=FALSE----------------------------------------------
#  file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#  spmnya <- all_ratios(file.long, report = "long", print = TRUE)

## ----outliers, fig.keep='all', fig.align='center', fig.width=4.5, results='hide'----
file.long %>%
  extract_data(report = "long") %>%
  check_outliers("II.B.50m")

## ----qc_check, fig.keep='all', fig.align = 'center', fig.width=4.5, results='hide'----
qc.short <- file.short %>%
  all_ratios(report = "short", print = TRUE) %>%
  qc_check

## ----qc_output, echo=FALSE-----------------------------------------------
knitr::kable(qc.short$data, digits = 4)
knitr::kable(qc.short$summary, digits = 4)
knitr::kable(qc.short$error, digits = 2)

## ----isoplots, fig.keep='all', fig.show='hold', results='hide'-----------
data(pm10nya)
# remove QC samples
pm10.smp <- pm10nya[-grep("CRM", sample)]
# generate a factor year from sample names
year <- paste0("20", gsub("[[:alpha:]]+[0-9]{1,2}." , "", pm10.smp$sample))
year[grep("20-10", year)] <- "2010"
year <- as.factor(year)
year <- droplevels(year)
pm10.smp[, year := year]

isoplot_generic(pm10.smp, x = "Pb208207", y = "Pb206207", factor = "year", regression = TRUE)
isoplot_pm10(pm10.smp, factor = "year")

## ----isoplot_filter, eval=FALSE------------------------------------------
#  isoplot_pm10(pm10.smp[Pb208206.U < 0.01], factor = "year")

## ----save_table----------------------------------------------------------
save_table(pm10.smp)

## ----raw_data------------------------------------------------------------
file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
file.short <- system.file("extdata", "sednya_2015.csv", package = "pbratios")

