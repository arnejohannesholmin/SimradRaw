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
