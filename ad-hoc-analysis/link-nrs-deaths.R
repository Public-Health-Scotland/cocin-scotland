source("ad-hoc-analysis/read_and_tidy_rapid_data.R")

# Set up the SMRA connection
SMRA_connection <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "SMRA",
  uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
  pwd = rstudioapi::askForPassword("SMRA Password:")
)

chi_numbers <- covid_admissions %>% 
  distinct(chi_number) %>% 
  copy_to(SMRA_connection, ., "covid_chis", overwrite = TRUE)

deaths <- tbl(SMRA_connection, dbplyr::in_schema("Analysis","GRO_DEATHS_C")) %>% 
  semi_join(chi_numbers, by = c("UPI_NUMBER" = "chi_number"))
                
                
deaths_weekly <- tbl(SMRA_connection, dbplyr::in_schema("Analysis","GRO_DEATHS_WEEKLY_C"))


# Monthly deaths table

deaths <- tbl_df(dbGetQuery(channel, statement = "SELECT 
                            UPI_NUMBER, DATE_OF_DEATH, UNDERLYING_CAUSE_OF_DEATH, 
                            CAUSE_OF_DEATH_CODE_0, CAUSE_OF_DEATH_CODE_1, 
                            CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3, 
                            CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, 
                            CAUSE_OF_DEATH_CODE_6, CAUSE_OF_DEATH_CODE_7, 
                            CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9, 
                            ORIG_CAUSE_OF_DEATH_TEXT_1A, 
                            ORIG_CAUSE_OF_DEATH_TEXT_1B, 
                            ORIG_CAUSE_OF_DEATH_TEXT_1C, 
                            ORIG_CAUSE_OF_DEATH_TEXT_1D
                            FROM ANALYSIS.GRO_DEATHS_C")) %>% 
  filter(UPI_NUMBER %in% chi_data$UPI_NUMBER)

# Get weekly deaths data

deaths_weekly <- tbl_df(dbGetQuery(channel, statement = "SELECT 
                                   DERIVED_CHI, DATE_OF_DEATH, 
                                   UNDERLYING_CAUSE_OF_DEATH, 
                                   CAUSE_OF_DEATH_CODE_0, CAUSE_OF_DEATH_CODE_1, 
                                   CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3, 
                                   CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, 
                                   CAUSE_OF_DEATH_CODE_6, CAUSE_OF_DEATH_CODE_7, 
                                   CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9
                                   FROM ANALYSIS.GRO_DEATHS_WEEKLY_C")) %>% 
  mutate(UPI_NUMBER = str_sub(DERIVED_CHI, 1, 10)) %>% 
  filter(UPI_NUMBER %in% chi_data$UPI_NUMBER) %>% 
  select(-DERIVED_CHI)

# Check for any UPIs that appear in both

mult_deaths <- deaths %>% filter(UPI_NUMBER %in% deaths_weekly$UPI_NUMBER)

# If any UPIs appear in both only keep the weekly death (most recent)
# Remove this UPIs record from the main deaths data

deaths %<>% 
  filter(!(UPI_NUMBER %in% mult_deaths$UPI_NUMBER))

# Some UPIs may appear multiple times in deaths
# If this happens select most recent date
# Remove any duplicates

deaths %<>%
  group_by(UPI_NUMBER) %>% 
  filter(DATE_OF_DEATH == max(DATE_OF_DEATH)) %>% 
  ungroup() %>% 
  distinct()

# Check if any UPI_NUMBERs still appear more than once

deaths %>% count(UPI_NUMBER) %>% filter(n > 1)

# Manually remove second death

deaths %<>%
  filter(!(UPI_NUMBER == "1110403224" & UNDERLYING_CAUSE_OF_DEATH == "J449"))

# Join the two deaths files together

deaths %<>% 
  bind_rows(deaths_weekly)

# Check number of deaths before 2020

deaths %>% filter(year(DATE_OF_DEATH) < 2020) %>% count()

                     
                     