stats_server_path <- function() {
  platform <- dplyr::if_else(
    version$platform %in% c(
      "x86_64-redhat-linux-gnu",
      "x86_64-pc-linux-gnu"
    ),
    "server",
    "desktop"
  )

  stats_path <- fs::path(dplyr::if_else(
    platform == "server",
    "/conf",
    "//stats"
  ))

  return(stats_path)
}
