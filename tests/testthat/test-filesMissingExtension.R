test_that("prepInputs works with NULL archive + file without extension, but originally a .zip", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/unknownExtension",
                                  alsoExtract = "similar", destinationPath = tempdir()))
  testthat::expect_is(object = ras, class = "RasterLayer")
})

test_that("prepInputs WORKS if the file is not originally a .zip, but archive is provided (only extension matters)", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/unknownTAR",
                                                         alsoExtract = "similar", archive = "unknownTAR.tar", destinationPath = tempdir()))
  testthat::expect_is(object = ras, class = "RasterLayer")
})

test_that("prepInputs WORKS if passing archive .zip", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/unknownExtension",
                                                           archive = "unknownExtension.zip",
                                                           alsoExtract = "similar", destinationPath = tempdir()))
  testthat::expect_is(object = ras, class = "RasterLayer")
})

test_that("prepInputs WORKS passing just targetFile that is NOT an archive", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/unknownTIF",
                                                           alsoExtract = "similar", targetFile = "unknownTIF.tif", destinationPath = tempdir()))
  testthat::expect_is(object = ras, class = "RasterLayer")
})

test_that("prepInputs WORKS passing archive + targetFile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/unknownExtension",
                                                           archive = "unknownExtension.zip", targetFile = "rasterTest.tif",
                                                           alsoExtract = "similar", destinationPath = tempdir()))
  testthat::expect_is(object = ras, class = "RasterLayer")
})