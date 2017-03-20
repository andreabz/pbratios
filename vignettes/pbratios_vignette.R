## ------------------------------------------------------------------------
file.short <- system.file("extdata", "sednya_2015.csv", package = "pbratios")

## ------------------------------------------------------------------------
file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")

## ------------------------------------------------------------------------
library(pbratios)
spmnya <- extract_data(file.long, report = "long")
head(spmnya)

## ---- results='hide'-----------------------------------------------------
spmnya.ratios <- calc_ratios(spmnya)
head(spmnya.ratios)

## ---- echo=FALSE---------------------------------------------------------
head(spmnya.ratios)

## ------------------------------------------------------------------------
spmnya.corr <- corr_mbf(spmnya.ratios)
knitr::kable(head(spmnya.corr), digits = 4, pad = 0)

## ---- eval=FALSE---------------------------------------------------------
#  library(dplyr)
#  file.long <- system.file("extdata", "spmnya_2012.csv", package = "pbratios")
#  spmnya <- file.long %>%
#            extract_data(report = "long") %>%
#            calc_ratios %>%
#            corr_mbf
#  head(spmnya)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

