### IMOVE Recode
source("linked_extracts/output_recode/IMOVE_Recode.R")

# write dataset
write_rds(IMOVE_data, path(here("data", "IMOVE_data.rds")),
          compress = "gz"
)

### Genomics Recode
source("linked_extracts/output_recode/Genomics_Recode.R")

write_rds(genomics_hospdata, path(here("data", str_glue("Genomics_hospdata.rds"))),
          compress = "gz"
)