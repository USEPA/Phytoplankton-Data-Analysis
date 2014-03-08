### check 0307
### Read EFF Phyto.

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("^EFR", OUT$file, ignore.case=FALSE) & OUT$ncol == 9

### files to explicitly skip first go around.

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readEFR.R"



shortList <- OUTsub2
#######

dimCheck <- 1

dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.
print(dimCheck)
for( i in 1:nrow(OUTsub2)){

  xlcFreeMemory()
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp$iCheck <- i
  #print(dim(temp))
  
  dimCheck <- dimCheck + nrow(temp)
  temp$sheet_id <- OUTsub2$sheet_id[i]
  if(i == 1){ AAA <- temp}else{
  AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  }
}





date <- paste("19", AAA$Date..yymmdd., sep = "")
depth <- formatC(AAA$Depth.ft, flag = "0", width = 3)

ID <- paste(AAA$Station, date, "9999", depth, sep = "")

AAA <- AAA[nchar(ID) == 24, ]
ID <- ID[nchar(ID) == 24]

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Species,
                    cell_per_l = AAA$Cells.ml * 1000, ### convert to liters.
                    BV.um3.L = -9999,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          
  
}

