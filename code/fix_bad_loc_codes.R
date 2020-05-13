# Some fixes for hospital codes in the subjid which look obviously wrong

fix_bad_loc_codes <- function(data) {
  data <- data %>% 
    mutate(subjid = str_replace(subjid, "S341H(-\\d+)$", "S314H\\1"),
           subjid = str_replace(subjid, "N100H(-\\d+)$", "N101H\\1"),
           subjid = str_replace(subjid, "GN405(-\\d+)$", "G405H\\1"),
           subjid = str_replace(subjid, "SL116(-\\d+)$", "S116H\\1"))
  
  return(data)
}
