vpn_active <- function() {
  if (str_detect(version$os, "linux")) {
    return(TRUE)
  } else {
    return(any(str_detect(string = system("ipconfig", intern = TRUE), "nss\\.scot\\.nhs\\.uk")))
  }
}
