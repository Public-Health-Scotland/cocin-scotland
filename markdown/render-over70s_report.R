### 0 - Setup environment ----
source("extract-data/00_setup-environment.R")
library(rmarkdown)

if (latest_extract_date() < today()) {
  message("Getting new extract from REDCap")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

### 1 - Knit markdown report ----

render(
  input = "markdown/over70s_report.Rmd",
  output_dir = "output",
  output_format = "all",
  output_file = rep(str_glue("{date}_over70s_analysis",
    date = latest_extract_date()
  ), 3)
)
