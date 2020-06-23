vpn_active <- function() {
  any(str_detect(string = system("ipconfig", intern = TRUE), "nss\\.scot\\.nhs\\.uk"))
}