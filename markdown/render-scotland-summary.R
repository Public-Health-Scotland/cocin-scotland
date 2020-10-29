###########################################################
# Name of script: knit-markdown.R
# Written by: Analysts working in HPS Enhanced Surveillance
#             Cell - Hospital/ICU Work Stream
#
# Type of script: Knit Scotland summary markdown report
# Written/run on: R Studio Desktop
# Version of R: 3.6.1
#
# Description: Knit Scotland summary markdown report
#              and save to output folder
###########################################################


### 0 - Setup environment ----

source("extract-data/00_setup-environment.R")


### 1 - Knit markdown report ----

rmarkdown::render(
  input = here("markdown", "scotland-summary.Rmd"),
  output_file = here(
    "output",
    paste0(
      str_sub(latest_extract_date(), 1, 10),
      "_scotland-summary.docx"
    )
  )
)


### END OF SCRIPT ###
