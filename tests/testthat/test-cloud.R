test_that("test Cache(useCloud=TRUE, ...)", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    c("googledrive", "terra"),
    tmpFileExt = c(".tif", ".grd"),
    needGoogleDriveAuth = TRUE,
    opts = list(
      "reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
      "reproducible.ask" = FALSE
    )
  )

  clearCache(x = tmpCache)
  testsForPkgs <- "testsForPkgs"
  if (isTRUE(tryCatch(googledrive::drive_ls(testsForPkgs), error = function(e) TRUE))) {
    testsForPkgsDir <- retry(quote(googledrive::drive_mkdir(name = testsForPkgs)))
  }
  newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = testsForPkgs)))
  cloudFolderID <- newDir
  on.exit(
    {
      retry(quote(googledrive::drive_rm(cloudFolderID)))
    },
    add = TRUE
  )

  #######################################
  # local absent, cloud absent
  #######################################
  mess1 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })
  expect_true(any(grepl("Uploading", mess1)))

  #######################################
  # local present, cloud present
  #######################################
  mess2 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })
  expect_true(any(grepl(.message$LoadedCacheResult(), mess2)))
  .message$LoadedCacheResult
  expect_false(all(grepl("uploaded", ignore.case = TRUE, mess2)))
  expect_false(all(grepl("download", mess2)))

  #######################################
  # local absent, cloud present
  #######################################
  # kkkk <<- 1

  clearCache(userTags = .robustDigest(1), x = tmpCache, useCloud = FALSE)
  mess3 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })
  expect_false(any(grepl(.message$LoadedCacheResult(), mess3)))
  expect_false(any(grepl("Uploaded", mess3)))
  expect_true(any(grepl("Downloading", mess3)))

  #######################################
  # local present, cloud absent
  #######################################
  clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
  a1 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache)
  mess4 <- capture_messages({
    a2 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })

  expect_true(any(grepl(.message$LoadedCacheResult(), mess4)))
  expect_true(any(grepl("Uploading", mess4)))
  expect_false(any(grepl("Download", mess4)))

  #######################################
  # cloudFolderID missing
  #######################################
  reproducible::clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)

  opts <- options("reproducible.cloudFolderID" = NULL)

  warn5 <- capture_warnings({
    mess5 <- capture_messages({
      a2 <- Cache(rnorm, 3, cachePath = tmpdir, useCloud = TRUE)
    })
  })

  expect_true(any(grepl("Uploading", mess5)))
  expect_false(any(grepl("Download", mess5)))

  warn6 <- capture_warnings({
    mess6 <- capture_messages({
      a2 <- Cache(rnorm, 3, cachePath = tmpCache, useCloud = TRUE)
    })
  })

  # expect_false(any(grepl("Folder created", mess6)))
  expect_true(any(grepl("Uploading", mess6)))
  expect_false(any(grepl("Download", mess6)))
  expect_false(any(grepl(.message$LoadedCacheResult(), mess6)))
  expect_true(isTRUE(all.equal(length(warn6), 0)))

  ########
  try(googledrive::drive_rm(newDir), silent = TRUE) # clear the original one
  cloudFolderID <- getOption("reproducible.cloudFolderID")
  clearCache(x = tmpCache, useCloud = TRUE) # , cloudFolderID = cloudFolderID)
  # Add 3 things to cloud and local -- then clear them all
  for (i in 1:3) {
    a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  }
  expect_silent({
    mess1 <- capture_messages(
      clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })

  expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

  # Add 3 things to local, only 2 to cloud -- clear them all, without an error
  for (i in 1:2) {
    a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  }
  a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = FALSE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })
  expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

  # Add 2 things to local and cloud -- clear only 1 of them, without an error
  Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, userTags = CacheDigest(rnorm(1))$outputHash, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })

  gdriveLs <- googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))
  expect_true(NROW(gdriveLs) == 2)
  expect_true(all(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]), gdriveLs$name)))
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    # tmpFileExt = c(".tif", ".grd"),
    needGoogleDriveAuth = TRUE,
    opts = list("reproducible.ask" = FALSE)
  )

  opts <- options("reproducible.cachePath" = tmpdir)
  suppressWarnings(rm(list = "aaa", envir = .GlobalEnv))

  on.exit(
    {
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
    },
    add = TRUE
  )
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = basename2(tmpdir), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
    type = "Raster"
  )

  retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
  cloudFolderID <- newDir

  # the 3 raster files include the .grd, .gri, and .grd.aux.xml
  testRasterInCloud(".grd",
    cloudFolderID = cloudFolderID, numRasterFiles = 3, tmpdir = tmpdir,
    type = "Raster"
  )
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- stack", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    # tmpFileExt = c(".tif", ".grd"),
    needGoogleDriveAuth = TRUE,
    opts = list("reproducible.ask" = FALSE)
  )


  # googledrive::drive_auth("predictiveecology@gmail.com")
  on.exit(
    {
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
    },
    add = TRUE
  )
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit(options(opts), add = TRUE)
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = basename2(tmpdir), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
    type = "Stack"
  )
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- brick", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    needGoogleDriveAuth = TRUE,
    opts = list("reproducible.ask" = FALSE)
  )

  opts <- options("reproducible.cachePath" = tmpdir)
  # googledrive::drive_auth("predictiveecology@gmail.com")
  on.exit(
    {
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
    },
    add = TRUE
  )
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = tempdir2(), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
    type = "Brick"
  )
})

test_that("prepInputs works with team drives", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    needGoogleDriveAuth = TRUE,
    "googledrive",
    opts = list(
      "reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
      "reproducible.ask" = FALSE
    )
  )

  # zipUrl <- "https://drive.google.com/file/d/1zRX2c55ebJbQtjijCErEfGxhsa7Ieph2" # Orig
  zipUrl <- "https://drive.google.com/file/d/1JpdvM8QiyCyNyQAvSaFU0rAY-1I3mcbp"

  # This will fail if it is hit too many times -- we don't want the test to report
  #  fail because of this
  opts <- options("reproducible.interactiveOnDownloadFail" = FALSE)
  on.exit(options(opts), add = TRUE)
  if (packageVersion("googledrive") < "2.0.0") {
    wb <- prepInputs(
      targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
      alsoExtract = "similar",
      team_drive = TRUE
    )
  } else {
    err <- capture_error(
      noisy <- capture.output(
        wb <- prepInputs(
          targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
          alsoExtract = "similar",
          shared_drive = TRUE
        )
      )
    )
    if (is.null(err)) {
      if (.requireNamespace("sf")) {
        expect_true(is(wb, "sf"))
      }
    }
  }
})
