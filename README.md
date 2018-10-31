SimradRaw R package
=====

This R package provides functions for reading, writing, splitting and other processing of Simrad raw files (raw0 (echosounder) and raw1 (fishery sonar)). The code is based on the Matlab library written by dr. Rick Towler, NOAA Alaska Fisheries Science Center. The sonR package (https://github.com/arnejohannesholmin/sonR) can be used for converting raw files to the binary TSD format (using the TSD package https://github.com/arnejohannesholmin/TSD) for faster reading.

<<<<<<< HEAD
Version: 1.3
Required R version: 3.4
=======
Version: 1.2
Required R version: 3.3.3
>>>>>>> master

Installation
=====

``` r
# Install the packages that SimradRaw depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,"Package"]), repos="http://cran.us.r-project.org") instead:
dep.pck <- c("devtools", "data.table", "pbapply", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install SimradRaw and also the packages that SimradRaw depends on which are on GitHub (by Holmin):
# On Windows you will need Rtools to complete the installations.
# Check whether you have Rtools by running Sys.getenv('PATH'),
#   and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.
# Be sure to check the box "Add rstools to system PATH" when installing Rtools.
# Note that if you need to run R as administrator due to security settings,
#   it is advised to install the pakcages in plain R, and NOT using Rstudio.
# Close Rstudio, open R and run the installation, and reopen Rstudio.

dep.pck.git <- c("arnejohannesholmin/TSD", "arnejohannesholmin/SimradRaw")
# If you want to install the lastest development versions, run devtools::install_github(dep.pck.git, ref="develop") instead:
devtools::install_github(dep.pck.git)

```

# For changes log see https://github.com/arnejohannesholmin/SimradRaw/blob/master/NEWS

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

