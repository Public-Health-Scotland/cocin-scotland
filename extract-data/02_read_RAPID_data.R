source("extract-data/00_setup-environment.R")

# Read the linked RAPID / ECOSS file, found here:
# \\stats\PHSCOVID19_Analysis\RAPID Reporting\Daily_extracts

# Set correct filepath for server or desktop
linked_file_path <- path(
  if_else(version$platform == "x86_64-pc-linux-gnu",
          "/conf",
          "//stats"
  ),
  "PHSCOVID19_Analysis", "RAPID Reporting", "Daily_extracts", "rapid_ecoss_joined.rds"
)

# Check the VPN is active (or that we're on the server) and if so try to update the file
if (vpn_active()) {
  message("Connection to NHS network is active")
  if (file_exists(linked_file_path)) {
    server_last_modified <- file_info(linked_file_path) %>%
      pull(modification_time)
    
    local_last_modified <- file_info("data/rapid_ecoss_joined.rds") %>%
      pull(modification_time)
    
    # If the server version is newer (or the local copy doesn't exist) copy it accross
    if (date(server_last_modified) > date(local_last_modified) | is.na(local_last_modified)) {
      message(str_glue("The server file looks newer (modified:{server_date}), so replacing local file (modified:{local_date})",
                       server_date = date(server_last_modified),
                       local_date = date(local_last_modified)
      ))
      file_copy(linked_file_path, "data/rapid_ecoss_joined.rds", overwrite = TRUE)
      
      local_last_modified <- server_last_modified
      
      message("New file coppied")
    } else {
      message(str_glue("Local file is current (modified:{local_date})",
                       local_date = date(local_last_modified)
      ))
    }
  } else {
    message("File 'rapid_ecoss_joined.rds' does not exist - contact Bob Taylor")
  }
} else {
  message("Not connected to NHS network so can't check for new file")
}

rm(linked_file_path, local_last_modified, server_last_modified)