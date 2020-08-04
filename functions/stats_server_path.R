stats_server_path <- function() {
  platform <- dplyr::if_else (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                    "x86_64-pc-linux-gnu (64-bit)"),
                       "server",
                       "desktop")

  stats_path <- fs::path(dplyr::if_else(platform == "server",
                             "/conf",
                             "//stats"))
  
  return(stats_path)
}
