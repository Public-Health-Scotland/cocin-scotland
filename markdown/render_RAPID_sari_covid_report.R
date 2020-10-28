### 0 - Setup environment ----
source("extract-data/00_setup-environment.R")


### 1 - Prepare data ----

source("ad-hoc-analysis/rapid_sari_covid.R")


# Render report -----------------------------------------------------------

rmarkdown::render("markdown/RAPID_sari_covid_report.Rmd")