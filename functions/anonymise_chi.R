anonymise_chi <- function(data, chi, lookup_file, drop = TRUE) {
  
  # If the lookup doesn't exist yet then create an empty one so we can 'add' to it
  if (!fs::file_exists(lookup_file)) {
    empty_lookup <- tibble::tibble()
    readr::write_rds(empty_lookup, lookup_file)
  }
  
  # Use the SHA-1 algorithm to hash the CHI number
  # Place the anon_id variable after the chi
  data <- dplyr::mutate(data, anon_id = as.character(openssl::sha1({{chi}}), .after = {{chi}}))
  
  # Write the lookup to disk
  # Add all of the chis to the existing lookup
  readr::write_rds(dplyr::bind_rows(readr::read_rds(lookup_file),
                                    data) %>% 
                     # Remove any duplicates
                     dplyr::distinct({{chi}}, anon_id),
                   lookup_file,
                   compress = "gz")
  
  # Drop the chi variable as standard behaviour
  if (drop) {
    data <- dplyr::select(data, -{{chi}})
  }
  
  return(data)
}
