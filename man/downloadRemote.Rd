% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{downloadRemote}
\alias{downloadRemote}
\title{Download a remote file}
\usage{
downloadRemote(
  url,
  archive,
  targetFile,
  checkSums,
  dlFun = NULL,
  fileToDownload,
  messSkipDownload,
  destinationPath,
  overwrite,
  needChecksums,
  .tempPath,
  preDigest,
  verbose = getOption("reproducible.verbose", 1),
  ...
)
}
\arguments{
\item{url}{Optional character string indicating the URL to download from.
If not specified, then no download will be attempted. If not entry
exists in the \code{CHECKSUMS.txt} (in \code{destinationPath}), an entry
will be created or appended to. This \code{CHECKSUMS.txt} entry will be used
in subsequent calls to
\code{prepInputs} or \code{preProcess}, comparing the file on hand with the ad hoc
\code{CHECKSUMS.txt}. See table in \code{\link[=preProcess]{preProcess()}}.}

\item{archive}{Optional character string giving the path of an archive
containing \code{targetFile}, or a vector giving a set of nested archives
(e.g., \code{c("xxx.tar", "inner.zip", "inner.rar")}). If there is/are (an) inner
archive(s), but they are unknown, the function will try all until it finds
the \code{targetFile}. See table in \code{\link[=preProcess]{preProcess()}}. If it is \code{NA},
then it will \emph{not} attempt to see it as an archive, even if it has archive-like
file extension (e.g., \code{.zip}). This may be useful when an R function
is expecting an archive directly.}

\item{targetFile}{Character string giving the filename (without relative or
absolute path) to the eventual file
(raster, shapefile, csv, etc.) after downloading and extracting from a zip
or tar archive. This is the file \emph{before} it is passed to
\code{postProcess}. The internal checksumming does not checksum
the file after it is \code{postProcess}ed (e.g., cropped/reprojected/masked).
Using \code{Cache} around \code{prepInputs} will do a sufficient job in these cases.
See table in \code{\link[=preProcess]{preProcess()}}.}

\item{checkSums}{TODO}

\item{dlFun}{Optional "download function" name, such as \code{"raster::getData"}, which does
custom downloading, in addition to loading into R. Still experimental.}

\item{fileToDownload}{TODO}

\item{messSkipDownload}{The character string text to pass to messaging if download skipped}

\item{destinationPath}{Character string of a directory in which to download
and save the file that comes from \code{url} and is also where the function
will look for \code{archive} or \code{targetFile}. NOTE (still experimental):
To prevent repeated downloads in different locations, the user can also set
\code{options("reproducible.inputPaths")} to one or more local file paths to
search for the file before attempting to download. Default for that option is
\code{NULL} meaning do not search locally.}

\item{overwrite}{Logical. Should downloading and all the other actions occur
even if they pass the checksums or the files are all there.}

\item{needChecksums}{Logical indicating whether to generate checksums. ## TODO: add overwrite arg to the function?}

\item{.tempPath}{Optional temporary path for internal file intermediate steps.
Will be cleared on.exit from this function.}

\item{preDigest}{The list of \code{preDigest} that comes from \code{CacheDigest} of an object}

\item{verbose}{Numeric, -1 silent (where possible), 0 being very quiet,
1 showing more messaging, 2 being more messaging, etc.
Default is 1. Above 3 will output much more information about the internals of
Caching, which may help diagnose Caching challenges. Can set globally with an
option, e.g., \verb{options('reproducible.verbose' = 0) to reduce to minimal}}

\item{...}{Additional arguments passed to
\code{\link[=postProcess]{postProcess()}} and \code{\link[=Cache]{Cache()}}.
Since \code{...} is passed to \code{\link[=postProcess]{postProcess()}}, these will
\code{...} will also be passed into the inner
functions, e.g., \code{\link[=cropInputs]{cropInputs()}}. Possibly useful other arguments include
\code{dlFun} which is passed to \code{preProcess}. See details and examples.}
}
\description{
Download a remote file
}
