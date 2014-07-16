### second try, same directory.

setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)
id <- grepl(pattern="jade0424a", OUT$full_file_name)

### subset of jade0424a files
OUTsub2 <- OUT[id  & !OUT$processed, ]

## commented out for processing work.
# bkup <- OUTsub2
# bkupOUT <- OUT

jade <- bkup
OUT <- bkupOUT

### change OUT df
OUT$script[OUT$full_file_name %in% jade$full_file_name] <- "readJade0424a.R"


###
### id for full list


### don't read sheets starting with 121.
iii <- grep("^121", jade$sheet )
jade$processed[iii] <- TRUE
jade$script[iii]    <- "readJade0424a.R"
jade$skip[iii]      <- "Not processed, data read by merging sample results and details."

sum(jade$processed)
####  manually read data from 2 files

xxx <- NULL

files <- "jade0424a/2GRR20120912 No Biovolume Phytoplankton.xlsx"

for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" )
  tempDetails <- readWorksheet(wb, sheet="Sample Details" )
  tempBoth <- merge(tempResults, tempDetails, all.x = TRUE )
  tempBoth$sheet_id <- subset(OUT, sheet == "Sample Results" & full_file_name == files[j] )$sheet_id
  if(j == 1){
    xxx <- tempBoth
  }else { xxx <- merge(xxx, tempBoth, all = TRUE)}
}


i <- OUT$full_file_name %in% files
OUT$processed[i] <- TRUE

groupA <- xxx
####
jade$processed[jade$full_file_name == "jade0424a/2GRR20120912 No Biovolume Phytoplankton.xlsx"] <-TRUE

files <- "jade0424a/2BRR20120911 Phytoplankton.xls" 
### this file contains meta information on the sheet with info on "Sample Details"
for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" ) 
  wsheets <-   wsheets[grep("^121",wsheets)]
  xxx<- NULL
  for( i in 1:length(wsheets) ){
    tempSheet <- readWorksheet(wb, sheet=wsheets[i], startRow=9 ) 
    tempSheet <- tempSheet[!is.na(tempSheet$Genus.species), ]
    tempSheet$Sample.ID <- wsheets[i]
    
    xxx <- merge(xxx, tempSheet, all = TRUE)
  }
  tempBoth <- merge(tempResults, xxx, all.x = TRUE ) 
}

groupAA <- tempBoth
###

tempDetails <- readWorksheet(wb, sheet="Sample Details" ) 

groupAAA <- merge(groupAA, tempDetails, all.x = TRUE)

jade$processed[jade$full_file_name == "jade0424a/2BRR20120911 Phytoplankton.xls"] <-TRUE

sum(jade$processed)

### drop files with 3 digit sheet names

i <- nchar(jade$sheet) == 3
jade$processed[i] <- TRUE
jade$skip <- "Not processed, data read by merging sample results and details."

sum(jade$processed)

i <- jade$nrow == 1 ## in sequence files with one row have numeric sheets with algae data, without volume
files <- unique(jade$full_file_name[i])
xxx <- NULL
for(j in 1:4){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  ws <- wsheets[!grepl("Sam", wsheets)]
  tempResults <- readWorksheet(wb, sheet="Sample Details" ) 
  tempData <- readWorksheet(wb, sheet = ws, startRow=9)
  tempData <- tempData[!is.na(tempData$Division),]
  tempData$Location <- tempResults$Location
  tempData$Sample.Date <- tempResults$Sample.Date
  tempData$Depth <- tempResults$Sample.Depth
  tempData$Time <- tempResults$Sample.Time
  xxx <- merge(xxx, tempData, all = TRUE)
}

### a little cleanup

i <- nchar(xxx$Location) == 24
xxx$Time[i] <- substr(xxx$Location[i], 18,21)

###

groupB <- xxx


### grab sample results, merge with sample details

i <- jade$sheet == "Sample Results"  & !jade$processed ## in sequence files with one row have numeric sheets with algae data, without volume

files <- unique(jade$full_file_name[i])


for(j in 1:41){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
 
#  tempDetails <- readWorksheet(wb, sheet="Sample Details" ) 
  tempResults <- readWorksheet(wb, sheet = "Sample Results")

ind <- grepl("^Col", names(tempResults) )
tempResults <- tempResults[,!ind]

names(tempResults) <- gsub("\\.", replacement="", names(tempResults))

ii <- is.na(tempResults$Location)


tempResults <- tempResults[!ii, ]
if(j == 1){xxx <- tempResults} else{
  xxx <- merge(xxx, tempResults, all = TRUE) }

print(dim(xxx))

}

jade$processed[jade$full_file_name %in% files] <- TRUE
