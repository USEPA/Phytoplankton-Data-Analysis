### master file used to run all low level scripts.
rm(list = ls() )

homeDir <- "/Users/mattpocernich/repos/epa_2013/2013/TO 0016/analysis/Phytoplankton-Data-Analysis"
setwd(homeDir)

source("rscripts/helperFunctions.R")
source("rscripts/chunck_check.R")
library(XLConnect)

options(stringsAsFactors=FALSE)
WRITE<- TRUE

OUTold   <- read.table("output/reducedFileSurvey.0501.csv", sep = ",", header = TRUE, as.is = TRUE)
OUT   <- read.table("output/reducedFileSurvey.0501.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO <- read.table("output/reducedFileList.0501.csv", sep = ",", as.is = TRUE, header = TRUE)

### sheets not processed
OUT$skip   <- NA ## comments on scripts not processed
OUT$script <-NA ## identify the script used for each file

id <- OUT$sheet == "Sample Details"
OUT$skip[id] <- "redundant"
OUT$processed[id] <- TRUE

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

### empty sheets
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

## contains wq data
source("rscripts/readDirG.R")
print(sum(OUT$processed))

## contains wq data
source("rscripts/readDirK.R")
print(sum(OUT$processed))

### contains wq data.
source("rscripts/readDirQ.R")
print(sum(OUT$processed))

### no wq
source("rscripts/readDASLER.R")
print(sum(OUT$processed))

## no wq
source("rscripts/readCyano.R")
print(sum(OUT$processed))

## just water quality data
 source("rscripts/readEFRWQ.R")
 print(sum(OUT$processed))

## no water quality
source("rscripts/readAlgal.R")
print(sum(OUT$processed))

### originally not written to file or saved.
source("rscripts/readEDD.R")
print(sum(OUT$processed))

### no wq
source("rscripts/readDirL.R")
print(sum(OUT$processed))

## no wq
source("rscripts/readRawData.R")
print(sum(OUT$processed))

## no wq
source("rscripts/readSample.R")
print(sum(OUT$processed))

### just algae
source("rscripts/readEFR.R")
print(sum(OUT$processed))

### all algae
source("rscripts/readMisc.R")
print(sum(OUT$processed))

source("rscripts/readDBF.R")
print(sum(OUT$processed))

source("rscripts/readHAB.R")
print(sum(OUT$processed))

xxx <- subset(OUT, ! processed)
View(xxx)

write.table(OUT, paste("processed_data/summaryStatus_", format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names = FALSE, sep = ",")
xx <- paste("processed_data/algae_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
cmd <- paste('cp processed_data/algae.csv', xx )

system( cmd)
cmd <- paste("chmod a-w", xx)
system( cmd )
