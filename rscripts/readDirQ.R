library(XLConnect)

OUT   <- read.table("output/reducedFileSurvey.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO  <- read.table("output/reducedFileList.csv", sep = ",", header = TRUE, as.is = TRUE)

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl(pattern="/q/", OUT$full_file_name)
OUTsub <- OUT[id, ]

OUTsub2 <- subset(OUTsub, file == "EFR2011 CT Data organized.xlsx")  ## one file
OUTsub2 <- subset(OUTsub2, ! sheet %in% c( "ALL", "Sheet1", "Sheet2") )


err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

AAA  <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE)
dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.

for( i in 2:nrow(OUTsub2)){
    
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp <- temp[, names(temp) != "Analyte.1"]
  
  dimCheck <- dimCheck + nrow(temp)
  
 
  temp$sheet_id <- OUTsub2$sheet_id[i]
  AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  
}

if(dimCheck != nrow(AAA)) warning("Check your merge.")


ii <- !unique(BBB$Sample.Description) %in% unique(AAA$Sample.Description)
unique(BBB$Sample.Description) [ii]

