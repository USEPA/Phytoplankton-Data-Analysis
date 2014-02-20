### master file used to run all low level scripts.
rm(list = ls() )

homeDir <- "/Users/mattpocernich/repos/epa_2013/2013/TO 0016/analysis/Phytoplankton-Data-Analysis"
setwd(homeDir)

source("rscripts/helperFunctions.R")
WRITE<- TRUE

OUT   <- read.table("output/reducedFileSurvey.0214.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO <- read.table("output/reducedFileList.0214.csv", sep = ",", as.is = TRUE, header = TRUE)

### sheets not processed
OUT$skip <- NA
id <- OUT$sheet == "Sample Details"
OUT$skip[id] <- "redundant"
OUT$processed[id] <- TRUE

id <- OUT$sheet == "Sample Scores"
OUT$skip[id] <- "redundant"
OUT$processed[id] <- TRUE

id<- OUT$file %in%c("EFR-2001-Run1.xls" , "EFR-2001-Run3.xls" )
OUT$skip[id] <- "superceded"
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

####
id<- OUT$file %in%c("EFR phyto 8-23-2011.xls" )
OUT$skip[id] <- "duplicated in Drew d/"
OUT$processed[id] <- TRUE



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

source("rscripts/readEFRWQ.R")
print(sum(OUT$processed))

source("rscripts/readAlgal.R")
print(sum(OUT$processed))


source("rscripts/readEDD.R")
print(sum(OUT$processed))

source("rscripts/readDirL.R")
print(sum(OUT$processed))

write.table(OUT, "output/summary.status.csv", row.names = FALSE)

