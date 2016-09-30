################
#### Will Barnett, August 2016
################


################
#### This is the master file used to run all low level scripts.
#### After creating a list of all of the sheets in the data files,
#### this script reads in all of the data. It also sources a script
#### that cleans the algae data set.
################
rm(list = ls() )


## Working directory should be set by the .Rproj file
homeDir <- getwd()
# homeDir <- "/Users/wbarnett/Documents/Neptune/svn/EPA/trunk/2015/TO 57/analysis/Phytoplankton"
setwd(homeDir)

## Libraries and helper functions
source("rscripts/helperFunctions.R")
library(openxlsx)
library(readxl)
library(plyr)
library(reshape2)
#source("rscripts/chunck_check.R")



options(stringsAsFactors=FALSE)
WRITE<- TRUE

OUT   <- read.table("output/reducedFileSurvey_2016-09-27.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO <- read.table("output/reducedFileList_2016-09-27.csv", sep = ",", as.is = TRUE, header = TRUE)


## Several sheets are not processed, for various reasons.

OUT$skip   <- NA ## comments on scripts not processed
OUT$script <-NA ## identify the script used for each file

id <- OUT$sheet == "Sample Details"
OUT$skip[id] <- "redundant"
#OUT$processed[id] <- TRUE

id <- OUT$sheet == "Sample Scores"
OUT$skip[id] <- "redundant"
OUT$processed[id] <- TRUE

id <- OUT$file == "WQ Sampling Status 2012.xlsx"
OUT$skip[id] <- "Cost Details."
OUT$processed[id] <- TRUE

id <- OUT$file == "FY13 Lab Analysis Details.xlsx"
OUT$skip[id] <- "Cost Details., some location lat lon"
OUT$processed[id] <- TRUE

id <- OUT$file == "FY13 Lab Analysis Estimate.xlsx"
OUT$skip[id] <- "Cost Details., some location lat lon"
OUT$processed[id] <- TRUE

id<- OUT$file %in%c("EFR-2001-Run1.xls" , "EFR-2001-Run3.xls" )
OUT$skip[id] <- "superceded"
OUT$processed[id] <- TRUE

id<- grepl( "Jade", OUT$file  )
OUT$skip[id] <- "Odd format, contains taxa list and metrics. Not imported"
OUT$processed[id] <- TRUE

id<- grepl( "All Lakes Phyto Data pre-1993.xlsx", OUT$file  )
id2<- grepl( "Harsha Phyto Data pre-1993.xlsx", OUT$file  )


OUT$skip[id | id2] <- "Not imported, contains no taxa descriptions"
OUT$processed[id | id2] <- TRUE

id <- OUT$sheet %in% c("LRN Phytoplankton Details ", "LRN Phytoplankton Scores")
OUT$skip[id] <- "Do not contain data, only sample meta data"
OUT$processed[id] <- TRUE

id2 <- grepl("graph", OUT$full_file_name,ignore.case=TRUE)
# id3 <- grepl("organized", OUT$full_file_name,ignore.case=TRUE)
id4 <- grepl("Data Q&A", OUT$full_file_name,ignore.case=TRUE) #### do not include because all files are parsed, split up and contain figures.

OUT$skip[id2|  id4 ] <- "Contained duplicate or processed data"
OUT$processed[id2| id4] <- TRUE

# id <- grepl("jade0424a", OUT$full_file_name) ## analyze new files separately
# OUT$skip[id] <- "jade0424 files"
# OUT$processed[id] <- TRUE

## Skip empty sheets
id <- OUT$nrow == 0
OUT$skip[id] <- "empty"
OUT$processed[id] <- TRUE

#id <- grepl("^HAB", OUT$file)
#OUT$skip[id ] <- "Contained column names for density and biovolume, but seem to be missing results"
#OUT$processed[id] <- TRUE


####
id<- OUT$file %in%c("EFR phyto 8-23-2011.xls" )
OUT$skip[id] <- "duplicated in Drew d/"
OUT$processed[id] <- TRUE

id <- OUT$sheet %in% c("Parameter_Code")
OUT$skip[id] <- "Contains STORET Parameter Codes"
OUT$processed[id] <- TRUE

id <- OUT$sheet %in% c("Filtered Locations")
OUT$skip[id] <- "Filter information"
OUT$processed[id] <- TRUE

id <- OUT$file == "EFR 1994 Phyto (2).xlsx" & OUT$sheet == "Original"
OUT$skip[id] <- "Data processed with DASLER import, duplicate"
OUT$processed[id] <- TRUE

id <- OUT$file == "PHYTO94.xls" 
OUT$skip[id] <- "Confirm how to calculate density and biovolume with these fields"
OUT$processed[id] <- TRUE


id <- OUT$file == "2007 (2).xls"
OUT$skip[id] <- "Files have no headers; Confirm how to calculate density and biovolume with these fields"
OUT$processed[id] <- TRUE

OUT$script[OUT$processed] <- "Not Processed"


## Source each script for different chunks of data.
## jade042414/ directory
source("rscripts/readJade.R") # Missing data - look into it
print(sum(OUT$processed))

## jade0623/ directory
source("rscripts/readJade0623.R")
print(sum(OUT$processed))

## Drew data / g /
source("rscripts/readDirG.R")
print(sum(OUT$processed))

## Drew data / k /
source("rscripts/readDirK.R")
print(sum(OUT$processed))

## Drew data / q / 
## All water quality data
source("rscripts/readDirQ.R")
print(sum(OUT$processed))

## Read DASLER files
source("rscripts/readDASLER.R")
print(sum(OUT$processed))

## Files with 'Cyano' in the name
source("rscripts/readCyano.R")
print(sum(OUT$processed))

## Just water quality data
source("rscripts/readEFRWQ.R")
print(sum(OUT$processed))

## Sheets with 'Algal'
source("rscripts/readAlgal.R")
print(sum(OUT$processed))

## Sheets with 'EDD' in them - water quality data.
## Not read in.
source("rscripts/readEDD.R")
print(sum(OUT$processed))

### Mostly Drew data / l /
source("rscripts/readDirL.R")
print(sum(OUT$processed))

## Read 'Files with complications / 92Rawdat.xlsx'
source("rscripts/readRawData.R")
print(sum(OUT$processed))

## Reads sheets with 'Phytoplankton' in the name
source("rscripts/readSample.R")
print(sum(OUT$processed))

### EFR data
source("rscripts/readEFR.R")
print(sum(OUT$processed))

### Three different keywords in sheet names - all algae
source("rscripts/readMisc.R")
print(sum(OUT$processed))

## Two DBF files in Drew data / a
source("rscripts/readDBF.R")
print(sum(OUT$processed))

## HAB files
source("rscripts/readHAB.R")
print(sum(OUT$processed))

## Old script that reads lots of water quality data
## None of it is read now.
source("rscripts/readRemaining.R")
print(sum(OUT$processed))

## Read CTL files
source("rscripts/readCTL.R")
print(sum(OUT$processed))

## Read 2014 / 2014 / 1988 files
source("rscripts/readNewPhyto.R")
print(sum(OUT$processed))

## Read the Thomason DASLER files
source("rscripts/readThomasonDASLER.R")
print(sum(OUT$processed))

## Make sure no files remain
xxx <- subset(OUT, ! processed)
View(xxx)


## Write summary status file.
write.table(OUT, paste("processed_data/summaryStatus_", format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names = FALSE, sep = ",")
xx <- paste("processed_data/algae_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
## Copy algae file. Still needs to be QC'd in further script.
cmd <- paste('cp processed_data/algae.csv', xx )
system( cmd)


## Next, run algaeCheck.R , which writes the 'clean' algae file
source("cleanAlgaeFile.R")
