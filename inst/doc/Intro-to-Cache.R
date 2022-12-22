## ----function-level, echo=TRUE------------------------------------------------
library(raster)
library(reproducible)
library(data.table)
setDTthreads(1) # user can omit this with little effect

tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
dir.create(tmpDir, recursive = TRUE)

ras <- raster(extent(0, 1000, 0, 1000), vals = 1:1e6, res = 1)
crs(ras) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"

newCRS <- "+init=epsg:4326" # A longlat crs

# No Cache
system.time(suppressWarnings(map1 <- projectRaster(ras, crs = newCRS))) # Warnings due to new PROJ

# Try with memoise for this example -- for many simple cases, memoising will not be faster
opts <- options("reproducible.useMemoise" = TRUE)
# With Cache -- a little slower the first time because saving to disk
system.time({
  suppressWarnings({
    map1 <- Cache(projectRaster, ras, crs = newCRS, cachePath = tmpDir, notOlderThan = Sys.time())
  })
})

# faster the second time
system.time({
  map2 <- Cache(projectRaster, ras, crs = newCRS, cachePath = tmpDir)
})

options(opts)

all.equal(map1, map2) # TRUE

## -----------------------------------------------------------------------------
library(raster)
library(magrittr)

try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear

ranNumsA <- Cache(rnorm, 10, 16, cachePath = tmpDir)

# All same
ranNumsB <- Cache(rnorm, 10, 16, cachePath = tmpDir) # recovers cached copy
ranNumsD1 <- Cache(quote(rnorm(n = 10, 16)), cachePath = tmpDir) # recovers cached copy
ranNumsD2 <- Cache(rnorm(n = 10, 16), cachePath = tmpDir) # recovers cached copy

# Any minor change makes it different
ranNumsE <- Cache(rnorm, 10, 6, cachePath = tmpDir) # different

## ----tags---------------------------------------------------------------------
ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif(4), cachePath = tmpDir, userTags = "objectName:b")

showCache(tmpDir, userTags = c("objectName"))
showCache(tmpDir, userTags = c("^a$")) # regular expression ... "a" exactly
showCache(tmpDir, userTags = c("runif")) # show only cached objects made during runif call

clearCache(tmpDir, userTags = c("runif"), ask = FALSE) # remove only cached objects made during runif call
showCache(tmpDir) # only those made during rnorm call

clearCache(tmpDir, ask = FALSE)

## ----accessed-tag-------------------------------------------------------------
ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif, 4, cachePath = tmpDir, userTags = "objectName:b")

# access it again, from Cache
Sys.sleep(1)
ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
wholeCache <- showCache(tmpDir)

# keep only items accessed "recently" (i.e., only objectName:a)
onlyRecentlyAccessed <- showCache(tmpDir, userTags = max(wholeCache[tagKey == "accessed"]$tagValue))

# inverse join with 2 data.tables ... using: a[!b]
# i.e., return all of wholeCache that was not recently accessed
#   Note: the two different ways to access -- old way with "artifact" will be deprecated
toRemove <- unique(wholeCache[!onlyRecentlyAccessed, on = "cacheId"], by = "cacheId")$cacheId
clearCache(tmpDir, toRemove, ask = FALSE) # remove ones not recently accessed
showCache(tmpDir) # still has more recently accessed


## ----keepCache----------------------------------------------------------------
ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif(4), cachePath = tmpDir, userTags = "objectName:b")

# keep only those cached items from the last 24 hours
oneDay <- 60 * 60 * 24
keepCache(tmpDir, after = Sys.time() - oneDay, ask = FALSE)

# Keep all Cache items created with an rnorm() call
keepCache(tmpDir, userTags = "rnorm", ask = FALSE)

# Remove all Cache items that happened within a rnorm() call
clearCache(tmpDir, userTags = "rnorm", ask = FALSE)

showCache(tmpDir) ## empty

# Also, can set a time before caching happens and remove based on this
#  --> a useful, simple way to control Cache
ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
startTime <- Sys.time()
Sys.sleep(1)
ranNumsB <- Cache(rnorm, 5, cachePath = tmpDir, userTags = "objectName:b")
keepCache(tmpDir, after = startTime, ask = FALSE) # keep only those newer than startTime

clearCache(tmpDir, ask = FALSE)


## ----searching-within-cache---------------------------------------------------
# default userTags is "and" matching; for "or" matching use |
ranNumsA <- Cache(runif, 4, cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:b")

# show all objects (runif and rnorm in this case)
showCache(tmpDir)

# show objects that are both runif and rnorm
# (i.e., none in this case, because objecs are either or, not both)
showCache(tmpDir, userTags = c("runif", "rnorm")) ## empty

# show objects that are either runif or rnorm ("or" search)
showCache(tmpDir, userTags = "runif|rnorm")

# keep only objects that are either runif or rnorm ("or" search)
keepCache(tmpDir, userTags = "runif|rnorm", ask = FALSE)

clearCache(tmpDir, ask = FALSE)

## ----expensive-computations---------------------------------------------------
ras <- raster(extent(0, 5, 0, 5), res = 1,
              vals = sample(1:5, replace = TRUE, size = 25),
              crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")

# A slow operation, like GIS operation
notCached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  projectRaster(ras, crs = crs(ras), res = 5, cachePath = tmpDir)
)

cached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  # using quote works also
  Cache(projectRaster, ras, crs = crs(ras), res = 5, cachePath = tmpDir)
)

# second time is much faster
reRun <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  Cache(projectRaster, ras, crs = crs(ras), res = 5, cachePath = tmpDir)
)

# recovered cached version is same as non-cached version
all.equal(notCached, reRun) ## TRUE

## ----nested-------------------------------------------------------------------
##########################
## Nested Caching
# Make 2 functions
inner <- function(mean) {
  d <- 1
  Cache(rnorm, n = 3, mean = mean)
}
outer <- function(n) {
  Cache(inner, 0.1, cachePath = tmpdir2)
}

# make 2 different cache paths
tmpdir1 <- file.path(tempdir(), "first")
tmpdir2 <- file.path(tempdir(), "second")

# Run the Cache ... notOlderThan propagates to all 3 Cache calls,
#   but cachePath is tmpdir1 in top level Cache and all nested
#   Cache calls, unless individually overridden ... here inner
#   uses tmpdir2 repository
Cache(outer, n = 2, cachePath = tmpdir1, notOlderThan = Sys.time())

showCache(tmpdir1) # 2 function calls
showCache(tmpdir2) # 1 function call

# userTags get appended
# all items have the outer tag propagate, plus inner ones only have inner ones
clearCache(tmpdir1, ask = FALSE)
outerTag <- "outerTag"
innerTag <- "innerTag"
inner <- function(mean) {
  d <- 1
  Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5, userTags = innerTag)
}
outer <- function(n) {
  Cache(inner, 0.1)
}
aa <- Cache(outer, n = 2, cachePath = tmpdir1, userTags = outerTag)
showCache(tmpdir1) # rnorm function has outerTag and innerTag, inner and outer only have outerTag


## ----selective-cacheId--------------------------------------------------------
### cacheId
set.seed(1)
Cache(rnorm, 1, cachePath = tmpdir1)
# manually look at output attribute which shows cacheId: 7072c305d8c69df0
Cache(rnorm, 1, cachePath = tmpdir1, cacheId = "7072c305d8c69df0") # same value
# override even with different inputs:
Cache(rnorm, 2, cachePath = tmpdir1, cacheId = "7072c305d8c69df0")


## ----manual-cache-------------------------------------------------------------
# As of reproducible version 1.0, there is a new backend directly using DBI
mapHash <- unique(showCache(tmpDir, userTags = "projectRaster")$cacheId)
map <- loadFromCache(mapHash[1], cachePath = tmpDir)
plot(map)

## ----cleanup------------------------------------------------------------------
## cleanup
unlink(dirname(tmpDir), recursive = TRUE)

