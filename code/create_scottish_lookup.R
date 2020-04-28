# Create lookup for Scottish hospitals
# Lookup includes 
# hospid (Location code)
# Locname (Name of the hospital)
# Postcode
# HB2019 and HB2019Name (the code and name of the HB for the hospital)

scot_locations <- read_rds("//stats/cl-out/lookups/Unicode/National Reference Files/location.rds") %>%
  select(Location, Locname, Postcode)
hb_postcodes <- read_rds("//stats/cl-out/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds") %>%
  select(pc8, HB2019, HB2019Name)

scot_locations <- left_join(scot_locations, hb_postcodes, by = c("Postcode" = "pc8"))

write_csv(scot_locations, "lookups/scot_locations.csv")
