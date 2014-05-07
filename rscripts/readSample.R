## check 0307

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("Phytoplankton", OUT$file) & grepl("Sample Results", OUT$sheet)
id1 <- grepl("jade0424a", OUT$full_file_name) ## analyze new files separately

OUTsub2 <- OUT[(id & !id1) & OUT$processed == FALSE, ]

#OUTsub2 <- OUT[(id & !id1) , ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readSample.R"


dimCheck <- 1
err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

AAA  <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE)
AAA$iCheck <- 1
AAA$sheet_id <- OUTsub2$sheet_id[1]
dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.
print(dimCheck)
for( i in 2:nrow(OUTsub2)){
  xlcFreeMemory()
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp$iCheck <- i
  #print(dim(temp))
  temp <- temp[, names(temp) != "Analyte.1"]
  #  print(tail(temp))
  dimCheck <- dimCheck + nrow(temp)
  temp$sheet_id <- OUTsub2$sheet_id[i]
if(!  is.numeric(temp$Date.Analyzed) ){
  
  temp$Date.Analyzed <- as.numeric(temp$Date.Analyzed)
}
  
  AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  
}

if(dimCheck != nrow(AAA)) warning("Check your merge.")

### rearrange digts
ii <- !grepl("^2", AAA$Sample.Date)

AAA$Sample.Date[ii] <- paste(substr(AAA$Sample.Date[ii] , 5,8) ,
                             substr(AAA$Sample.Date[ii] , 1,2),
                             substr(AAA$Sample.Date[ii] , 3,4),
                             sep = "")

ID <- paste(AAA$Location, AAA$Sample.Date, 
            formatC(AAA$Sample.Time, width = 4, flag = "0"),
            formatC(AAA$Sample.Depth, width = 3, flag = "0"), sep = "")

ii <- nchar(ID)== 24
ID <- ID[ii] 
AAA  <- AAA[ii, ]

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5,9) ,
                    depth_ft = substr(ID,22,24 ),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Taxa,
                    cell_per_l = AAA$Cells.liter,
                    BV.um3.L = AAA$Total.biovolume..µm3.L.,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)


chunck_check(algae)
setwd(homeDir)


if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}

