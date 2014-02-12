##### based on sheets with identical or similar headers;  read data; merge and indicate on OUT that sheet data has been read.

rm(list = ls())

library(XLConnect)
source("rscripts/helperFunctions.R")
homeDir <- getwd()


OUT   <- read.table("output/reducedFileSurvey.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO  <- read.table("output/reducedFileList.csv", sep = ",", header = TRUE, as.is = TRUE)


OUT$batch <- NA  ## identify files which have been read.

setwd("originalData/algae/EFR Phytoplankton Data/")


## picking a list of a large number of similar structured sheets
### BATCH 1 ; may need to think of numbering system
id <- grepl("^Sample.Description", OUT$sheetNames)

### problem files; force skip
problems <- c("EFR2011 Data Q&A.xlsx")
id.Problem <- OUT$full_file_name == "Drew data/q/EFR2011 Data Q&A.xlsx"

shortList <- OUT[id & ! id.Problem, ]

OUT$batch[id & ! id.Problem ] <- "batch0"
OUT$batch[id.Problem ] <- "problem"

batch0 <- readSelectSheets(shortList)

write.table(batch0, "../../../output/step1/batch0.csv", sep = ",",row.names=FALSE)

### Batch1

if(FALSE){
id <- grepl("^Location", OUT$sheetNames)
shortList <- OUT[id, ]

OUT$batch[id] <- "batch1"

temp <- readSelectSheets(shortList)

### the following deals with cleaning data.  Should likely be moved 
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

batch1 <- temp
write.table(batch1, "../../../output/step1/batch1.csv", sep = ",",row.names=FALSE)

}
# ####  help pick large groups with common names
# id <- grepl("^Location", OUT$sheetNames)
# shortList <- OUT[id, ]
# unique(shortList$sheetNames)
# table(shortList$sheetNames)


## BATCH 2
## use first sheet as a template, skip files that won't open

txt <- "Location; Sample_Date; Sample_Time; Sample_Depth; LRL_Tag_Num; Analyte_Name; Analyte_Code; Result; Units; Qualifiers; Detect_Limit; Report_Limit; Lab_Id; Lab_Sample_Num; Prep_Method; Test_Method; DF; Analysis_Date; Imported"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]
OUT$batch[id] <- "batch2"

batch2 <- readSelectSheets(shortList)
dim(batch2)
write.table(batch2, "../../../output/step1/batch2.csv", sep = ",",row.names=FALSE)

###
txt <- "Location; Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Chlorophyll; Collected.By; Disposition; Collection.Method; Preparation.Method"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]
dim(shortList)
OUT$batch[id] <- "batch3"

batch3 <- readSelectSheets(shortList)
dim(batch3)
write.table(batch3, "../../../output/step1/batch3.csv", sep = ",",row.names=FALSE)

##

txt <- "Location; Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Index.Name; Index.Score"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]
OUT$batch[id] <- "batch4"

batch4 <- readSelectSheets(shortList)
write.table(batch4, "../../../output/step1/batch4.csv", sep = ",",row.names=FALSE)

###  Batch 5 - use explicit loop; process outside of fuction because of Data.Analyzed

txt <- "Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Date.Analyzed; Analytical.Method; Lab; Data.Confidence; Taxonomist; Division; Taxa; Reference; Cells.liter; Relative.....abundance; Mean.Biovolume..µm3.; Total.biovolume..µm3.L.; Relative.....biovolume"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]
OUT$batch[id] <- "batch5"

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") next  
temp      <- readWorksheet(wb, sheet=shortList$sheet[i])
temp$Date.Analyzed <- as.character(temp$Date.Analyzed)

for(i in 2:nrow(shortList) ){
  #for(i in 2:6 ){
  print(i)     
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next
  x      <- readWorksheet(wb, sheet=shortList$sheet[i])
  x$Date.Analyzed <- as.character(x$Date.Analyzed)
  x$sheet_id <- shortList$sheet_id[i]
  # temp <- merge(temp, x, all = TRUE)
  temp <- rbind(temp, x)
  print(dim(temp) )
  xlcFreeMemory()
}

batch5 <- temp

write.table(batch5, "../../../output/step1/batch5.csv", sep = ",",row.names=FALSE)

#### multiple sheets; same file
txt <- "d/EFR phyto 8-23-2011"
id <- grepl(txt, OUT$full_file_name)
OUT$batch[id] <- "batch6"

shortList <- OUT[id, ]

#######

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") print( "File Missing") 

sheets <- getSheets(wb)
batch6 <- NULL

for(i in 1:20){

  temp <- readWorksheet(wb, sheet=sheets[i], startRow=9)
temp <- temp[!is.na(temp$Division), ]

temp$sample_id <- unlist(readWorksheet(wb, sheet=sheets[i], 
              startCol=2,endCol=2,startRow=5, endRow=5, header = FALSE) )
temp$sheet_id <- shortList$sheet_id[i]
### When column names don't match after i = 10, force them to match
if(i > 1){
  if( sum(names(temp) %in% names(batch6))!=9 ) { names(temp) <- names(batch6) }
}
batch6 <- rbind( batch6, temp)
}

}

write.table(batch6, "../../../output/step1/batch6.csv", sep = ",",row.names=FALSE)

#####  batch7 

txt <- "Cyanobacterial.Analysis.Report; Sample.ID.; Col3"
id <- grepl(txt, OUT$sheetNames)
OUT$batch[id] <- "batch7"

shortList <- OUT[id, ]

batch7 <- NULL
for(i in 1:nrow(shortList)){
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") next  

temp <- readWorksheet(wb, shortList$sheet[i], startRow=8)
temp <- temp[!is.na(temp$Division), ]
if(nrow(temp)==0){
  print("No bugs found?")
  next}
temp$samp_id       <- unlist(readWorksheet(wb, sheet=shortList$sheet[i], startRow = 3, endRow = 3, 
                               startCol = 1, endCol = 1, header = FALSE) )
temp$sheet_id <- shortList$sheet_id[i]
batch7 <- rbind(batch7, temp)
}



write.table(batch7, "../../../output/step1/batch7.csv", sep = ",",row.names=FALSE)

#### batch 8

txt <- "Sample.Description; Lab; PREP; Method; UNITS; Result; LOD; Reporting.Limit; Qualifiers; Analyte; Sample"
id <- grepl(txt, OUT$sheetNames)
### files to explicitly skip first go around.
id2 <- grepl("graph", OUT$full_file_name,ignore.case=TRUE)
id3 <- grepl("organized", OUT$full_file_name,ignore.case=TRUE)

OUT$batch[id2] <- "graphs"
OUT$batch[id3] <- "organized"

shortListA <- OUT[id & !(id2 | id3), ]


OUT$batch[id & !(id2 | id3)] <- "batch8"

### break into groups based on ncol
shortList <- subset(shortListA, ncol == 11)

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") next  

batch8 <- readWorksheet(wb, shortList$sheet[i])
batch8$sheet_id <- shortList$sheet_id[i]

for(i in 2:nrow(shortList)){
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next  
  
  temp <- readWorksheet(wb, shortList$sheet[i])
  temp$sheet_id <- shortList$sheet_id[i]
  xlcFreeMemory()
  print(dim(temp))
   batch8 <- rbind(batch8, temp)
  
}

batch8_ncol11 <- batch8

shortList <- subset(shortListA, ncol == 14)

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") next  

batch8 <- readWorksheet(wb, shortList$sheet[i])
batch8$sheet_id <- shortList$sheet_id[i]

for(i in 2:nrow(shortList)){
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next  
  
  temp <- readWorksheet(wb, shortList$sheet[i])
  temp$sheet_id <- shortList$sheet_id[i]
  xlcFreeMemory()
  print(dim(temp))
  batch8 <- rbind(batch8, temp)
  
}

batch8_ncol14 <- batch8

shortList <- subset(shortListA, ncol == 15)

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") next  

batch8_ncol15 <- readWorksheet(wb, shortList$sheet[i])
batch8_ncol15$sheet_id <- shortList$sheet_id[i]



