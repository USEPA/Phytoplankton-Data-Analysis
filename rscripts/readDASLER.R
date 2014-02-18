setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("DASLER", OUT$sheet)

OUTsub2 <- OUT[id & OUT$processed == FALSE, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

AAA  <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE)
AAA$iCheck <- 1
dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.
print(dimCheck)
for( i in 2:nrow(OUTsub2)){
  xlcFreeMemory()
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp$iCheck <- i
  print(dim(temp))
  temp <- temp[, names(temp) != "Analyte.1"]
#  print(tail(temp))
  dimCheck <- dimCheck + nrow(temp)
  
  temp$sheet_id <- OUTsub2$sheet_id[i]
  AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
 
}

if(dimCheck != nrow(AAA)) warning("Check your merge.")

ID <- paste(AAA$Location, format(AAA$Sample.Date, "%Y%m%d"), 
            formatC(AAA$Sample.Time, width = 4, flag = "0"),
            formatC(AAA$Sample.Depth, width = 3, flag = "0"), sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5,9) ,
                    depth_ft = substr(ID,22,24 ),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Taxa,
                    cell_per_l = AAA$Density....l.,
                    BV.um3.L = AAA$Biovolume..um3.L.,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

if(WRITE){
  write.table(algae, "../../../processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          
 
}