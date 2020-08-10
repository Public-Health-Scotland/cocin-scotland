###########################################################
# Name of script: 00_setup-environment.R
# Written by: Analysts working in HPS Enhanced Surveillance
#             Cell - Hospital/ICU Work Stream
#
# Type of script: Setup environment
# Written/run on: R Studio Desktop
# Version of R: 3.6.1
#
# Description: Load packages and create folders
###########################################################


### 1 - Load packages ----
# Use the if require pattern so the package will be installed if needed
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
if (!require(lubridate)) {
  install.packages("lubridate")
}
if (!require(finalfit)) {
  install.packages("finalfit")
}
if (!require(Hmisc)) {
  install.packages("Hmisc")
}
if (!require(tidylog)) {
  install.packages("tidylog")
}
if (!require(janitor)) {
  install.packages("janitor")
}
if (!require(magrittr)) {
  install.packages("magrittr")
}
if (!require(stringr)) {
  install.packages("stringr")
}
if (!require(here)) {
  install.packages("here")
}
if (!require(fs)) {
  install.packages("fs")
}
if (!require(REDCapR)) {
  install.packages("REDCapR")
}
if (!require(writexl)) {
  install.packages("writexl")
}
if (!require(scales)) {
  install.packages("scales")
}


### 2 - Load functions ---

purrr::walk(dir(here("functions"), full.names = TRUE), source)


### 3 - Create folders ---

dir_create(here("data"))

dir_create(here("output"))


server_dir <- path(stats_server_path(), "PHSCOVID19_Analysis/Hospital_linked_dataset/raw_data")
dir_create(server_dir)


### END OF SCRIPT ###
