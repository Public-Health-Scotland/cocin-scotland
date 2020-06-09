### 0 - Setup environment ----
# Run setup environment for other packages and functions
source("extract-data/00_setup-environment.R")
library(rmarkdown)

if (date(latest_extract_date()) < today()) {
  message("Getting new extract from REDCap")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

# Run the code to produce over70s analysis
source("ad-hoc-analysis/Over70s/over-70s-symptoms3.R")

# Run the code to produce the venn diagram(s)
source("ad-hoc-analysis/Over70s/over-70s-venn.R")

### 1 - Knit markdown report ----

render(
  input = "markdown/over70s_summary.Rmd",
  output_dir = "output",
  output_file = str_glue("DRAFT-{date}_over70s_summary",
    date = date(latest_extract_date())
  )
)
