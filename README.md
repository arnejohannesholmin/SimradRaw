SimradRaw R package
=====

This R package provides functions for reading, writing, splitting and other processing of Simrad raw files (raw0 (echosounder) and raw1 (fishery sonar)). The code is based on the Matlab library written by dr. Rick Towler, NOAA Alaska Fisheries Science Center. The functions use the TSD package for efficient reading of the data (writes to a temporary TSD file and reads afterwards to avoid appending in memory).

Version: 1.1
Required R version: 3.3.3

Installation
=====

``` r
# Install the packages that SimradRaw depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,"Package"]), repos="http://cran.us.r-project.org") instead:
dep.pck <- c("pbapply", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org")

# Install SimradRaw and also the packages that SimradRaw depends on which are on GitHub (by Holmin):
# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.

dep.pck.git <- c("arnejohannesholmin/TSD", "arnejohannesholmin/SimradRaw")
# If you want to install the lastest development versions, run devtools::install_github(dep.pck.git, ref="develop") instead:
devtools::install_github(dep.pck.git)

```

# For changes log see https://github.com/arnejohannesholmin/SimradRaw/NEWS

Examples
=====

``` r
# Write some data to a TSD file (all variable must have 4 character names):
echoSounderFile <- file.path(system.file("extdata", package="SimradRaw"), "RedSlip-D20160915-T120914.raw")
dat <- readEKRaw(echoSounderFile, t="all")
str(dat)
# Convert to Sv, volume backscattering strength:
Sv <- 10 * log10(readEKRaw_power2sv.TSD(dat))
plot(Sv[,1,1], type="l")




exampleFile <- "/Volumes/Acoustics/S20160915_RedslipAustevoll/Events/E001/EK60/raw/RedSlip-D20160915-T120914.raw"
dat <- readEKRaw(exampleFile, t=1:100)
echoSounderFile <- "~/Documents/Produktivt/Prosjekt/R-packages/SimradRaw/inst/extdata/RedSlip-D20160915-T120914.raw"
writeEKRaw(dat, echoSounderFile)
```

License
=====

The SimradRaw package is licensed under the LGPL-3.)

