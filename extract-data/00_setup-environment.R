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

library(tidyverse)
library(lubridate)
library(finalfit)
library(tidylog)
library(Hmisc)
library(janitor)
library(magrittr)
library(stringr)
library(here)
library(fs)
library(REDCapR)
library(writexl)
library(scales)


### 2 - Load functions ---

purrr::walk(dir(here::here("functions"), full.names = TRUE), source)


### 3 - Create folders ---

if(!is_dir(here("data"))){
  dir_create(here("data"))
}

if(!is_dir(here("output"))){
  dir_create(here("output"))
}


### END OF SCRIPT ###