library(data.table)

origDTthreads <- setDTthreads(2)

wantMoreTests <- isInteractive() || Sys.info()[["user"]] %in% c("emcintir")

if (wantMoreTests) { # this is for covr::package_coverage
  Sys.setenv(NOT_CRAN = "true")
  # Sys.setenv(SKIP_GAUTH = "true")
}

opts <- options(
  reproducible.runLargeFileTests = FALSE, # Set to TRUE to run the 2 long tests -- 20 minutes
  warnPartialMatchArgs = TRUE, # This gives false positives for `raster::stack`
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

if (Sys.info()["nodename"] %in% "W-VIC-A127585") {
  opts2 <- options(gargle_oauth_email = "eliotmcintire@gmail.com")
  if (isWindows())
    opts2 <- append(options(gargle_oauth_cache = "C:/Eliot/.secret"),
                    opts2)
  if (requireNamespace("googledrive"))
    googledrive::drive_auth()
  opts <- append(opts, opts2)
}

withr::defer(
  {
    if (wantMoreTests) {
      print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
      print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
      print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
      Sys.setenv(NOT_CRAN = "")
      Sys.setenv(SKIP_GAUTH = "")
    }
    options(opts)
    data.table::setDTthreads(origDTthreads)
    try(reproducible::clearCache(ask = FALSE, verbose = -1))
    try(unlink("CHECKSUMS.txt"), silent = TRUE) # comes from an unknown place
  },
  teardown_env()
)

if (wantMoreTests) {
  print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
  print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
  print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
}
