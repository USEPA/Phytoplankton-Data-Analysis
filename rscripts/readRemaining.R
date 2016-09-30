################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and excludes
#### the water quality data not processed at this point
################


## This script used to read in water quality data. Now it just crosses these
## files off the list.
## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset data
id <- grepl("Drew data", OUT$full_file_name) & !OUT$processed
OUT$processed[id] <- TRUE
OUT$script[id] <- "readRemaining.R"


## Change working directory back
setwd(homeDir)

