### IMOVE Recode
source("linked_extracts/output_recode/IMOVE_Recode.R")

### Pseudonymise IMOVE data - currently drops anon ID and postcode, randomises rows
### Once IG is in place, need to revisit this section
source("linked_extracts/output_recode/pseudonymise_data.R")

# write dataset  - commented out for now as I don't think we need this file
# write_rds(IMOVE_data, path(here("output", str_glue("IMOVE_data_{today()}.rds"))),
#           compress = "gz")
# 
# # Copy to server
# imove_data_local <- path(here("output", str_glue("IMOVE_data_{today()}.rds")))
# 
# if (file_exists(imove_data_local)) {
#   file_copy(
#     path = imove_data_local,
#     new_path = path(server_dir_final, str_glue("IMOVE_data_{today()}.rds"))
#   )
# }

# Run and view checks
# TODO finish checks - also review this as currently produces an error
rmarkdown::render("linked_extracts/check_linked_data.Rmd")
rstudioapi::viewer("linked_extracts/check_linked_data.html")

### Genomics Recode - commented out for now as not sure this is still required
# source("linked_extracts/output_recode/Genomics_Recode.R")
# 
# write_rds(genomics_hospdata, path(here("output", str_glue("Genomics_hospdata_{today()}.rds"))),
#           compress = "gz")
# 
# # Copy to server
# genomics_data_local <- path(here("output", str_glue("Genomics_hospdata_{today()}.rds")))
# 
# if (file_exists(genomics_data_local)) {
#   file_copy(
#     path = genomics_data_local,
#     new_path = path(server_dir_final, str_glue("Genomics_hospdata_{today()}.rds"))
#   )
# }
