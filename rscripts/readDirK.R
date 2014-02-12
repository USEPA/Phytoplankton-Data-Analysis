setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)

id <- grepl(pattern="/k/", OUT$full_file_name)
OUTsub <- OUT[id, ]

OUTsub2 <- subset(OUTsub, file == "92RAWDAT.xls")

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

sheets <- getSheets(wb)
WQ1 <- NULL
for( i in 1:length(sheets)){
  
  temp <- readWorksheet(wb, sheet=sheets[i], startRow=6,header=FALSE)
  
  WQ1 <- rbind(WQ1, temp)
}

WQ1 <- subset(WQ1, !is.na(Col1) )

### 
headr <- readWorksheet(wb, sheet=sheets[i], startRow=3, endRow=3,header=FALSE)
names(WQ1)<- headr
