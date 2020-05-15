# reproducible

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/reproducible)](https://cran.r-project.org/package=reproducible)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/reproducible)](https://cran.r-project.org/package=reproducible)

A set of tools for R that enhance reproducibility beyond package management.
Built on top of `git2r` and `archivist`, this package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible and reusable content in R.
This extends beyond the package management utilities of `packrat` and `checkpoint` by including tools for caching and accessing GitHub repositories.

## News

See updates from latest [CRAN](https://cran.r-project.org/package=reproducible) and [development](https://github.com/PredictiveEcology/reproducible/blob/development/NEWS.md) versions. Note that versions 1.0.0 and later are not compatible with previous versions "out of the box". However, setting the `options("reproducible.useDBI" = FALSE)` will return the behaviour to prior versions and so *should* be compatible. The new version is much faster and creates smaller repository files and allows for different (e.g., `RPostgres` backends for the database -- not the saved files, however; these are still saved locally).

## Installation

### Current release (on CRAN)

[![Build Status](https://travis-ci.org/PredictiveEcology/reproducible.svg?branch=master)](https://travis-ci.org/PredictiveEcology/reproducible)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/reproducible/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/reproducible/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/reproducible?branch=master)

**Install from CRAN:**

```r
install.packages("reproducible")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", dependencies = TRUE) 
```

### Development version

[![Build Status](https://travis-ci.org/PredictiveEcology/reproducible.svg?branch=development)](https://travis-ci.org/PredictiveEcology/reproducible)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/development?svg=true)](https://ci.appveyor.com/project/achubaty/reproducible/branch/development)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/reproducible/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/reproducible?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/reproducible", ref = "development", dependencies = TRUE) 
```

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
