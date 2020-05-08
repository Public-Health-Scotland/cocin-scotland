
library(fs)
library(here)

# 01_get-scottish-data
library(RCurl)
library(tidyverse)
library(lubridate)
library(finalfit)
library(tidylog)
library(Hmisc)

# 02_clean-data
library(tidyverse)
library(lubridate)
library(finalfit)

# Create data and output folders
if(!is_dir(here("data"))){
  dir_create(here("data"))
}

if(!is_dir(here("output"))){
  dir_create(here("output"))
}


### END OF SCRIPT ###