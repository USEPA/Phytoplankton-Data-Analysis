##### based on sheets with identical or similar headers;  read data; merge and indicate on OUT that sheet data has been read.

rm(list = ls())


OUT <- read.table("output/reducedFileSurvey.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO <- read.table("output/reducedFileList.csv", sep = ",", header = TRUE, as.is = TRUE)
OUT$read <- FALSE

xx <- aggregate( size ~ sheetNames  , OUT , length)
xx <- xx[sort(xx$size), ]

temp <- NULL
## picking a list of a large number of similar structured sheets
### BATCH 1 ; may need to think of numbering system
id <- grepl("^Sample.Description", OUT$sheetNames)
shortList <- OUT[id, ]

### problem files; force skip
problems <- c("q/EFR2011 Data Q&A.xlsx")

shortList <- subset(shortList, !shortList$file %in% problems)

## use first sheet as a template, skip files that won't open

temp <- readSelectSheets(shortList)
### strip less than sign
id <- grepl("^Location", OUT$sheetNames)
shortList <- OUT[id, ]

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

####  help pick large groups with common names
id <- grepl("^Location", OUT$sheetNames)
shortList <- OUT[id, ]
unique(shortList$sheetNames)
table(shortList$sheetNames)


## BATCH 2
## use first sheet as a template, skip files that won't open
setwd("originalData/algae/EFR Phytoplankton Data/Drew data/")

txt <- "Location; Sample_Date; Sample_Time; Sample_Depth; LRL_Tag_Num; Analyte_Name; Analyte_Code; Result; Units; Qualifiers; Detect_Limit; Report_Limit; Lab_Id; Lab_Sample_Num; Prep_Method; Test_Method; DF; Analysis_Date; Imported"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]

batch2 <- readSelectSheets(shortList)
dim(batch2)

###
txt <- "Location; Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Chlorophyll; Collected.By; Disposition; Collection.Method; Preparation.Method"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]
dim(shortList)

batch3 <- readSelectSheets(shortList)
dim(batch3)
##

txt <- "Location; Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Index.Name; Index.Score"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]

batch4 <- readSelectSheets(shortList)
###

txt <- "Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Date.Analyzed; Analytical.Method; Lab; Data.Confidence; Taxonomist; Division; Taxa; Reference; Cells.liter; Relative.....abundance; Mean.Biovolume..µm3.; Total.biovolume..µm3.L.; Relative.....biovolume"
id <- grepl(txt, OUT$sheetNames)
shortList <- OUT[id, ]

batch5 <- readSelectSheets(shortList)

#### multiple sheets; same file
txt <- "d/EFR phyto 8-23-2011"
id <- grepl(txt, OUT$file)

shortList <- OUT[id, ]

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$file[i]) )
if(class(err) == "try-error") next  

sheets <- getSheets(wb)
batch6 <- NULL
for(i in 1:20){
temp <- readWorksheet(wb, sheet=sheets[i], startRow=9)
temp <- temp[!is.na(temp$Division), ]

temp$sample_id <- unlist(readWorksheet(wb, sheet=sheets[i], 
              startCol=2,endCol=2,startRow=5, endRow=5, header = FALSE) )

### When column names don't match after i = 10, force them to match
if(i > 1){
  if( sum(names(temp) %in% names(batch6))!=9 ) { names(temp) <- names(batch6) }
}
batch6 <- rbind( batch6, temp)
}

}

#####

txt <- "Cyanobacterial.Analysis.Report; Sample.ID.; Col3"
id <- grepl(txt, OUT$sheetNames)

shortList <- OUT[id, ]
batch7 <- NULL
for(i in 1:nrow(shortList)){
err <-    try( wb     <- loadWorkbook(shortList$file[i]) )
if(class(err) == "try-error") next  

temp <- readWorksheet(wb, shortList$sheet[i], startRow=8)
temp <- temp[!is.na(temp$Division), ]

temp$samp_id       <- unlist(readWorksheet(wb, sheet=shortList$sheet[i], startRow = 3, endRow = 3, 
                               startCol = 1, endCol = 1, header = FALSE) )
batch7 <- rbind(batch7, temp)

}


