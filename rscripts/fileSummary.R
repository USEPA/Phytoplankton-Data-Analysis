## summarize files by directory
library(XLConnect)

### expand memory
options(java.parameters = "-Xmx8g" )


homeDir <- getwd()

setwd("originalData/algae/EFR Phytoplankton Data/Drew data/")

files  <- dir( recursive=TRUE,include.dirs=TRUE)

files  <- files[grepl("xls",x=files)]
INFO   <- file.info(files )
INFO <- data.frame(files, size = INFO[,1])
INFO$files <- as.character(INFO$files)

xx <- strsplit(INFO$files, "/")
yy <- do.call("rbind", xx)

INFO$cleanFileName <- yy[,2]
INFO$cleanFileName <- sub( pattern=" \\([234]\\)", replacement="", INFO$cleanFileName)
INFO$cleanFileName <- sub( pattern="Copy of ", replacement="", INFO$cleanFileName)


### combine cleanFileName and size to infer if a file is unique
INFO$uniqueID <- as.numeric(as.factor( paste(INFO$cleanFileName, INFO$size)))

INFOorig <- INFO <- INFO[order(INFO$cleanFileName), ]

id <- match( unique(INFO$uniqueID), INFO$uniqueID )  ## selects only one unique cleanFileName
INFO <- INFO[id, ]

### parse and get file name, drop  (2)


ERR <- OUT <- NULL

for(i in 1:nrow(INFO)){
#for(i in 1:10){     
  err <-    try( wb     <- loadWorkbook(INFO$files[i]) )
  if(class(err) == "try-error"){ 
       temp <- data.frame(i = i, file = INFO$files[i])
       ERR <- rbind(ERR, temp )
       next}
     sheets <- getSheets(wb)
     for( j in 1:length(sheets)){
     x      <- readWorksheet(wb, sheet=sheets[j])
     size   <- dim(x)
     xx     <- names(x)
     nms <- paste0(xx,collapse= "; ")
     firstRow <- paste0(x[1,], collapse= "; ")
     temp <- data.frame( file = INFO$files[i], size = INFO$size[i], sheet = sheets[j], ncol = size[2], nrow = size[1], sheetNames = nms , firstRow = firstRow)
     OUT <- rbind(OUT, temp)
     xlcFreeMemory()
     }
     
}

# write.table(OUT, "../../../../output/reducedFileSurvey.csv", sep = ",", row.names=FALSE)
# write.table(INFO, "../../../../output/reducedFileList.csv", sep = ",", row.names=FALSE)


OUT <- read.table("../../../../output/reducedFileSurvey.csv", sep = ",", header = TRUE)
INFO <- read.table("../../../../output/reducedFileList.csv", sep = ",", header = TRUE)

OUT <- factor_2_character(OUT)

##### based on sheets with identical or similar headers;  read data; merge and indicate on OUT that sheet data has been read.

OUT$read <- FALSE
temp <- NULL

id <- grepl("^Sample.Description", OUT$sheetNames)
shortList <- OUT[id, ]

### problem files

problems <- c("q/EFR2011 Data Q&A.xlsx")

shortList <- subset(shortList, !shortList$file %in% problems)

## use first sheet as a template
i <- 1
err <-    try( wb     <- loadWorkbook(shortList$file[i]) )
if(class(err) == "try-error") next
temp      <- readWorksheet(wb, sheet=shortList$sheet[i])


for(i in 2:nrow(shortList) ){
#for(i in 2:6 ){
print(i)     
     err <-    try( wb     <- loadWorkbook(shortList$file[i]) )
if(class(err) == "try-error") next
x      <- readWorksheet(wb, sheet=shortList$sheet[i])
temp <- merge(temp, x, all = TRUE)

xlcFreeMemory()
}

### strip less than sign

id <- !is.na(temp$Result) & !is.na(temp$Result.1) & (temp$Result != temp$Result.1)
sum(id) ## all values in which there are entries in both column, have the same entry in both columns

id <- is.na(temp$Result) & !is.na(temp$Result.1)  ## there are no results only found in Result.1 column, therefore delete column

### identify columns with both < and > symbols
temp$Result_numeric <- as.numeric(temp$Result)
temp$Result_numeric2 <- temp$Qual_symbol <- NA

id <- grepl(pattern="<",x=temp$Result ) & grepl(pattern=">",x=temp$Result )
temp$Qualifiers[id] <- "<>"


sub <- temp[id, ]

sub$Result <- sub(pattern=",", replacement="", sub$Result)

sub <- sub(temp0[,1],pattern=">", replacement="" )
temp0 <- strsplit(sub$Result, "<")
temp0 <- do.call("rbind", temp0 )

temp0[,1] <- sub(temp0[,1],pattern=">", replacement="" )
temp$Result_numeric[id] <- as.numeric(temp0[,1])
temp$Result_numeric2[id] <- as.numeric(temp0[,2])

### remove values from Result so that qualifiers aren't recoded again
temp$Result[id] <- NA

id <- grepl(pattern="<",x=temp$Result ) 
temp$Qualifiers[id] <- "<"
##
id <-  grepl(pattern=">",x=temp$Result )
temp$Qualifiers[id] <- ">"
## 




