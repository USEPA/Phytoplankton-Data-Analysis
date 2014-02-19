### master file used to run all low level scripts.
rm(list = ls() )

homeDir <- "/Users/mattpocernich/repos/epa_2013/2013/TO 0016/analysis/Phytoplankton-Data-Analysis"
setwd(homeDir)

source("rscripts/helperFunctions.R")
WRITE<- TRUE

OUT   <- read.table("output/reducedFileSurvey.0214.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO <- read.table("output/reducedFileList.0214.csv", sep = ",", as.is = TRUE, header = TRUE)

source("rscripts/readDirG.R")
print(sum(OUT$processed))

source("rscripts/readDirK.R")
print(sum(OUT$processed))

source("rscripts/readDirQ.R")
print(sum(OUT$processed))

source("rscripts/readDASLER.R")
print(sum(OUT$processed))

source("rscripts/readCyano.R")
print(sum(OUT$processed))
