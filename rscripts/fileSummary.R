################
#### Will Barnett, August 2016
################


################
#### This script enumerates all of the files and worksheets
#### within the files that might contain algae data.
################
rm(list = ls() )


## Libraries and functions
library(readxl)
library(openxlsx)
source("rscripts/helperFunctions.R")
homeDir <- getwd()
datDir <- "originalData/algae/EFR Phytoplankton Data"
setwd(datDir)


## List all directories and files
# Includes  directories and files
filesAll <- files  <- dir(recursive=TRUE,include.dirs=TRUE)

# Just filepaths
files  <- files[grepl("xls",x=files,ignore.case=TRUE)]  ## full path

# Empty vector of just file names
file_name <- NULL

# Some trickery to fill in the file names at different depths
xx <- strsplit(files, "/")
yy <- do.call("rbind", xx)
for(i in 1:nrow(yy)){
  ii <- min( grep("xls",  yy[i,],ignore.case=TRUE  ) )  ## odd way to separate the filename from path with different dephs
  file_name[i] <- yy[i,ii]
  
}

# Get file information, mostly file size.
INFO   <- file.info(files )
INFO <- data.frame(full_file_name = files, file_name =file_name, size = INFO[,1])
INFO$file_name <- as.character(INFO$file_name)


## Search for duplicate files in different directories or with slightly
## different names, such as with the naming scheme blahblah (2).xls, 
## or 'Copy of' blahblah.xls
INFO$cleanFileName <- INFO$file_name
INFO$cleanFileName <- sub( pattern=" \\([23456789]\\)", replacement="", INFO$cleanFileName)
INFO$cleanFileName <- sub( pattern="Copy of ", replacement="", INFO$cleanFileName)

# Combine cleanFileName and size to infer if a file is unique
INFO$uniqueID <- as.numeric(as.factor( paste(INFO$cleanFileName, INFO$size)))
INFOorig <- INFO <- INFO[order(INFO$cleanFileName), ]

# Select only one unique cleanFileName
id <- match( unique(INFO$uniqueID), INFO$uniqueID )  
INFO <- INFO[id, ]
INFO <- factor_2_character(INFO)


## Get information about each worksheet, like the name, 
## number of rows and columns, etc.
## Memory can be an issue with rbind(), so we 'flush' the memory
## and write a temporary file each 10 iterations.
ERR <- OUT <- NULL
first <- TRUE
tempfile <- "tempfile.csv"
for(i in 1:nrow(INFO)){
  if( !i %% 10 ) { 
    print( paste(i, "; flushing to disk...", sep = ""))
    write.table( OUT, file=paste("../../../output/",tempfile,sep=""), sep=",", append = !first, col.names=first,
                 row.names = FALSE)
    first <- FALSE
    OUT <- NULL   # nuke it
  }
  # i = 1
#for(i in 1:69){     
  err <-    try( excel_sheets(INFO$full_file_name[i]) )
  if(class(err) == "try-error"){ 
    temp <- data.frame(i = i, file = INFO$full_file_name[i])
    ERR <- rbind(ERR, temp )
    next
  }
  wb <- INFO$full_file_name[i]
  sheets <- excel_sheets(wb)
  for(j in 1:length(sheets)){
    err <- try(read_excel(wb, sheet=sheets[j]))
    if(class(err) != "try-error"){ 
      x      <- read_excel(wb, sheet=sheets[j])
      size   <- dim(x)
      xx     <- names(x)
      nms <- paste0(xx,collapse= "; ")
      firstRow <- paste0(x[1,], collapse= "; ")
      temp <- data.frame( full_file_name = INFO$full_file_name[i], file = INFO$file_name[i], size = INFO$size[i],
                          sheet = sheets[j], ncol = size[2], nrow = size[1], sheetNames = nms , firstRow = firstRow)
      OUT <- rbind(OUT, temp)
    }
    # xlcFreeMemory()
  }
  print(i)
}
print( "Loop done; flushing to disk and re-reading entire data set..." )
write.table( OUT, file=paste("../../../output/",tempfile,sep=""), sep=",", append=TRUE, col.names=FALSE,
             row.names = FALSE)
OUT <- read.table( file=paste("../../../output/", tempfile,sep=""),
                   sep=",", header = TRUE)


## Set working directory back to home
setwd(homeDir)

## Some columns to use later
OUT$sheet_id   <- 1:nrow(OUT)
OUT$processed <- FALSE
INFO$file_id   <- 1:nrow(INFO)
INFO$drop    <- NA
INFO$comment <- NA ## Fields to denote file to skip and reasons to skip them.
INFOorig$file_origID <- 1:nrow(INFOorig)

## Grab date
thisDate <- Sys.Date()

## Write data frames
# - INFOorig contains info on every file, including 
# what we have flagged as duplicate files.
# - INFO contains the non-duplicated files in INFOorig
# - OUT is the survey of sheets in every file within INFO
write.table( OUT, paste("output/reducedFileSurvey_",thisDate,".csv", sep=""), sep = ",", row.names=FALSE)
write.table( INFO, paste("output/reducedFileList_",thisDate,".csv", sep=""), sep = ",", row.names=FALSE)
write.table( INFOorig, paste("output/fullFileList_",thisDate,".csv", sep=""), sep = ",", row.names=FALSE)
