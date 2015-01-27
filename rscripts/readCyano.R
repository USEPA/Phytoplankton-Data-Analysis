### 
### check 0307
## to keep the out processed filed 

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl(pattern="^Cyano", OUT$sheetNames)

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readCyano.R"

OUTsub2$hab <- grepl("HAB", OUTsub2$full_file_name, ignore.case=TRUE)

dimCheck <- 0
for(i in 1:nrow(OUTsub2)){
  
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
#  print(i)
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], startRow=8)
  temp <- temp[!is.na(temp$Division), ]
  if(nrow(temp)==0){ 
  temp<- data.frame( Division = "Cynophycota", Genus = "Not Found", "Concentration..cells.mL." = NA)
  }
  temp$sample_id <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                         startCol=1,endCol=1,startRow=3, endRow=3, header = FALSE) )
  
  temp$sample_date <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                           startCol=3,endCol=3,startRow=3, endRow=3, header = FALSE) )
  
  temp$hab <- OUTsub2$hab[i]
  temp$sheet_id <- OUTsub2$sheet_id[i]
  dimCheck <- nrow(temp) + dimCheck
if(i == 1){AAA <- temp} else{
  AAA <- merge(AAA, temp, all = TRUE)
}
}

#### check ID
AAA <- factor_2_character(AAA)
ii <- nchar(AAA$sample_id) != 24

## ISSUE REPORTED FOR CLARIFICATION

#Drew data/f/31tab Fall HAB Data EFR20121031,SRR20121107,TAR20121106 alg.xls

AAA <- AAA[!ii, ]

algae <- data.frame(ID = AAA$sample_id,
                     lake = substr(AAA$sample_id, 2,4),
                     station = substr(AAA$sample_id, 5, 9),
                     depth_ft = substr(AAA$sample_id, 22, 24),
                     date = substr(AAA$sample_id, start=10, stop=17),
                     taxa = AAA$Genus,
                     cell_per_l = AAA$Concentration..cells.mL.,
                     BV.um3.L = NA,  ## how can I be certain about these units?
                     class = NA,
                     hab = TRUE,   ### hard coded HAB, see issue 
                     sheet_id = AAA$sheet_id)

algae$cell_per_l <- gsub(pattern=",",replacement="", algae$cell_per_l) 

## fix date
ii <- algae$ID == "2TARALG01020121161540000"
algae$date[ii] <- "20121106"

algae$cell_per_l <- as.numeric(algae$cell_per_l)
algae$BV.um3.L <- NA
setwd(homeDir)
algae$taxa <- gsub(pattern='"', replacement = "", algae$taxa)
algae$taxa <- gsub(pattern="'", replacement = "", algae$taxa)

chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}