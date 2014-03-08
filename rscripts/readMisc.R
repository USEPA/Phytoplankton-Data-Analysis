### new Misc

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- OUT$sheetNames == "LAKE; STATION; I_D__; DATE; DEPTH; TAXA; CELLS_L; BV_UM3_L"
### Read EFF Phyto.


### files to explicitly skip first go around.

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"

shortList <- OUTsub2
#######


dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.
print(dimCheck)
AAA <- NULL
for( i in 1:nrow(OUTsub2)){
  
  xlcFreeMemory()
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp$iCheck <- i
  
  if(max( class(temp$DATE) == "POSIXct" ) ) temp$DATE <- as.character(temp$DATE)
  #  print(tail(temp))
  dimCheck <- dimCheck + nrow(temp)
  temp$sheet_id <- OUTsub2$sheet_id[i]
  if(i == 1){ AAA <- temp}else{
    AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  }
}

depth <- as.numeric(gsub(pattern="'", "", AAA$DEPTH ) )
temp <- substr(AAA$DATE, 1, 10)
date <- gsub(pattern="-", "", temp )


ID <- paste("2", AAA$LAKE, AAA$STATION, date, "9999", 
            formatC(depth, flag = "0", width=3), sep = "")

####

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$TAXA,
                    cell_per_l = AAA$CELLS_L, ### convert to liters.
                    BV.um3.L = AAA$BV_UM3_L,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

setwd(homeDir)




if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",",  append=TRUE)      
  
}  

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("^Lake", OUT$sheetNames, ignore.case=TRUE)


OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"


shortList <- OUTsub2
#######
dimCheck <- 0
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.
print(dimCheck)
AAA <- NULL
for( i in 1:nrow(OUTsub2)){
  
  xlcFreeMemory()
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  nms <- names(temp)
  nms <- gsub(  "_", ".", nms )
  nms <- tolower(nms)
  nms[nms=="bv.l"] <- "bv.um3.l"
  
  names(temp) <- nms
  
  if(ncol(temp)==9){ temp <- temp[, ! names(temp)%in% c("col7", "sp.")] }
  temp$iCheck <- i
  
  if(max( class(temp$DATE) == "POSIXct" ) ) temp$DATE <- as.character(temp$DATE)
  #  print(tail(temp))
  dimCheck <- dimCheck + nrow(temp)
  temp$sheet_id <- OUTsub2$sheet_id[i]
  if(i == 1){ AAA <- temp}else{
    AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  }
}

depth <- AAA$depth
depth[depth == "comb"] <- "999"
depth <- gsub(pattern="'", "", depth ) 
depth <- gsub(pattern="0-", "", depth ) 
depth <- formatC(as.numeric( depth), width = 3, flag = "0")           

date <- gsub("-","", AAA$date)

station <- substr(AAA$station, 1,5)

ID <- paste("2", AAA$lake, station, date, "9999", depth, sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = AAA$station,
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$taxa,
                    cell_per_l = AAA$cells.l, ### convert to liters.
                    BV.um3.L = AAA$bv.um3.l,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

setwd(homeDir)




if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append=TRUE)      
  
}  

######
setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("Drew data/j/EFR Data Phytoplankton zlw.xlsx", OUT$full_file_name, ignore.case=TRUE)


OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook("Drew data/j/EFR Data Phytoplankton zlw.xlsx" ) )
               
if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet="LRN Phytoplankton Results ", header=TRUE)

temp <- temp[!is.na(temp$Location), ]


date <- gsub("-", "", temp$Sample.Date)
depth <- formatC( temp$Sample.Depth, width = 3, flag = "0")
ID <- paste(temp$Location, date, temp$Sample.Time, depth, sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station =  substr(ID, 5,9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = temp$Taxa,
                    cell_per_l = temp$Density....mL. * 1000, ### convert to liters.
                    BV.um3.L = temp$Biovolume..um3.mL.,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = OUTsub2$sheet_id[1])

setwd(homeDir)




if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append=TRUE, col.names=FALSE)      
  
}  


