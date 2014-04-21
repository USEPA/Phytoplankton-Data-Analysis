## summarize files by directory
library(XLConnect)
## 
rm(list = ls() )

### expand memory
options(java.parameters = "-Xmx8g" )
source("rscripts/helperFunctions.R")
### mjp home Directory
homeDir <- "/Users/mattpocernich/repos/epa_2013/2013/TO 0016/analysis/Phytoplankton-Data-Analysis"
#  homeDir <- getwd()

setwd("originalData/algae/EFR Phytoplankton Data/")

filesAll <- files  <- dir( recursive=TRUE,include.dirs=TRUE)
files  <- files[grepl("xls",x=files,ignore.case=TRUE)]  ## full path
file_name <- NULL

files.txt  <- files[grepl("txt$",x=filesAll,ignore.case=TRUE)]  ## full path


xx <- strsplit(files, "/")
yy <- do.call("rbind", xx)

for(i in 1:nrow(yy)){
  ii <- min( grep("xls",  yy[i,],ignore.case=TRUE  ) )  ## odd way to separate the filename from path with different dephs
  file_name[i] <- yy[i,ii]
  
}


INFO   <- file.info(files )
INFO <- data.frame(full_file_name = files, file_name =file_name, size = INFO[,1])

INFO$file_name <- as.character(INFO$file_name)

INFO$cleanFileName <- INFO$file_name
INFO$cleanFileName <- sub( pattern=" \\([234]\\)", replacement="", INFO$cleanFileName)
INFO$cleanFileName <- sub( pattern="Copy of ", replacement="", INFO$cleanFileName)

### combine cleanFileName and size to infer if a file is unique

INFO$uniqueID <- as.numeric(as.factor( paste(INFO$cleanFileName, INFO$size)))

INFOorig <- INFO <- INFO[order(INFO$cleanFileName), ]

id <- match( unique(INFO$uniqueID), INFO$uniqueID )  ## selects only one unique cleanFileName

INFO <- INFO[id, ]

INFO <- factor_2_character(INFO)
ERR <- OUT <- NULL

for(i in 1:nrow(INFO)){
#for(i in 1:10){     
  err <-    try( wb     <- loadWorkbook(INFO$full_file_name[i]) )
  if(class(err) == "try-error"){ 
       temp <- data.frame(i = i, file = INFO$full_file_name[i])
       ERR <- rbind(ERR, temp )
       next}
     sheets <- getSheets(wb)
     for( j in 1:length(sheets)){
     x      <- readWorksheet(wb, sheet=sheets[j])
     size   <- dim(x)
     xx     <- names(x)
     nms <- paste0(xx,collapse= "; ")
     firstRow <- paste0(x[1,], collapse= "; ")
     temp <- data.frame( full_file_name = INFO$full_file_name[i], file = INFO$file_name[i], size = INFO$size[i], 
                         sheet = sheets[j], ncol = size[2], nrow = size[1], sheetNames = nms , firstRow = firstRow)
     OUT <- rbind(OUT, temp)
     xlcFreeMemory()
     }
     
}

setwd(homeDir)

OUT$sheet_id   <- 1:nrow(OUT)
OUT$processed <- FALSE
INFO$file_id   <- 1:nrow(INFO)
INFO$drop    <- NA
INFO$comment <- NA ## Fields to denote file to skip and reasons to skip them.

INFOorig$file_origID <- 1:nrow(INFOorig)

write.table( OUT, "output/reducedFileSurvey.0307.csv", sep = ",", row.names=FALSE)
write.table( INFO, "output/reducedFileList.0307.csv", sep = ",", row.names=FALSE)
write.table( INFOorig, "output/fullFileList.0307.csv", sep = ",", row.names=FALSE)
