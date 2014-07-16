### read CTL files not processed earlier


setwd("originalData/algae/EFR Phytoplankton Data/")

### subset of jade0424a files
OUTsub <- OUT[!OUT$processed, ]

OUTsub <- subset(OUTsub, sheet == "Sample Results")
files <- unique(OUTsub$full_file_name)

for( i in 1:length(files)){
  
  wb <- loadWorkbook(files[i])
  temp <- readWorksheet(wb, sheet="Sample Results", endCol = 19)
  temp <- temp[, names(temp)!= "Date.Analyzed" ]
  temp$sheet_id <- OUTsub$sheet_id[i]
  if(i == 1){ xxx <- temp }else{
    xxx<- rbind(xxx, temp)
  }
}



xxx$Location <- gsub("NONAME5", "99999", xxx$Location)

ID <- paste(xxx$Location, 
            xxx$Sample.Date, 
            formatC(as.numeric(xxx$Sample.Time), flag = "0", digits=4, width=4), 
            formatC(as.numeric(xxx$Sample.Depth), flag = "0", digits=3, width=3), sep = "")
i <- nchar(ID) != 24  # fix sample date

xxx <- xxx[!i,]
ID <- ID[!i]

algae <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = xxx$Taxa,
                     cell_per_l = xxx$Cells.liter,
                     BV.um3.L = xxx$Total.biovolume..µm3.L.,  ## 0 or NA
                     class = NA,
                     hab = TRUE,   ### hard coded HAB, see issue 
                     sheet_id = xxx$sheet_id)
#### combine




yy <- as.Date(as.character(algae$date), format = "%Y%m%d")
i <- is.na(yy)
algae$date[i] <- paste(substr(algae$date[i], 5,8),
                       substr(algae$date[i], 1,4),
                       sep = "")
OUT$processed[OUT$full_file_name %in% unique(OUTsub$full_file_name)] <- TRUE
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)            
}
