### check 0307
## to keep the out processed filed 

setwd("originalData/algae/EFR Phytoplankton Data/")

id1 <- grepl(pattern="^Phyto", OUT$file)
id2 <- grepl(pattern="/l/", OUT$full_file_name)
id <- id1 & id2

OUTsub <- OUT[id, ]
idSheets <- nchar(OUT$sheet) == 3  ## only use three letter sheets

OUTsub2 <- OUT[id  & !OUT$processed &  idSheets, ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirL.R"

OUT$skip[id  & !OUT$processed & !idSheets ] <- "Summary sheets with graphs"

files <- unique(OUTsub2$full_file_name)

dimCheck <- 0
for( j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  # 
  OUTsub3 <- subset(OUTsub2, full_file_name == files[j] )
  
for(i in 1:nrow(OUTsub3)){
  #  print(i)
  temp <- readWorksheet(wb, sheet=OUTsub3$sheet[i], header = FALSE,startCol=1, endCol=9, useCachedValues=TRUE  )
  temp <- temp[!is.na(temp[,1]), ]
  
   temp$sheet_id <- OUTsub3$sheet_id[i]
  dimCheck <- nrow(temp) + dimCheck
if(i == 1 & j == 1){AAA <- temp} else{
  AAA <- merge(AAA, temp, all = TRUE)
   }} 
} 

### 
temp_Depth <- as.numeric(gsub("\'","", AAA$Col5)  )
ID <- paste("2", AAA$Col1, substr(AAA$Col2, 1,5) , format(AAA$Col4, "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")

#AAA <- cbind(AAA, split_sampleID(ID) )


algae <- data.frame(ID = ID,
                     lake = AAA$Col1,
                     station = AAA$Col2,
                     depth_ft = temp_Depth,
                     date = format(AAA$Col4, "%Y%m%d" ),
                     taxa = AAA$Col6,
                     cell_per_l = AAA$Col8,
                     BV.um3.L = AAA$Col9,  ## how can I be certain about these units?
                     class = NA,
                     hab = FALSE,
                     sheet_id = AAA$sheet_id )


#### pick up other files in directory, copied elsewhere
ii <- as.Date(algae$date, "%Y%m%d") > "2014-03-23"
# algae$date[ii] <- "20070820"; drop - incorrect date
algae <- algae[!ii, ]

id <- OUT$full_file_name == "Drew data/h/phytodata2005.xls"
OUT$processed[id] <- TRUE
OUT$script[id] <- "readDirL.R"

err <-    try( wb     <- loadWorkbook(OUT$full_file_name[id] ) )
if(class(err) == "try-error") print( "File Missing") 
temp <- readWorksheet(wb,sheet=1)

temp$sheet_id <- OUT$sheet_id[id]
temp_Depth <- as.numeric(gsub("\'","", temp$DEPTH)  )

ID <- paste("2", temp$LAKE, temp$STATION, format(temp$DATE, "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")


algae1 <- data.frame(ID = ID,
                    lake = temp$LAKE,
                    station = temp$STATION,
                    depth_ft = temp_Depth,
                    date = format(temp$DATE, "%Y%m%d" ),
                    taxa = temp$TAXA,
                    cell_per_l = temp$CELLS_L,
                    BV.um3.L = temp$BV_UM3_L,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = temp$sheet_id )
#####

id <- OUT$full_file_name == "Drew data/g/Copy of 2007.xls"
OUT$processed[id] <- TRUE
OUT$script[id] <- "readDirL.R"

err <-    try( wb     <- loadWorkbook(OUT$full_file_name[id] ) )
if(class(err) == "try-error") print( "File Missing") 
temp <- readWorksheet(wb,sheet=1, header = FALSE)
temp$sheet_id <- OUT$sheet_id[id]

temp_Depth <- as.numeric(gsub("\'","", temp$Col5)  )
ID <- paste("2", temp$Col1, substr(temp$Col2, 1,5) , format(temp$Col4, "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")
### note some stations have extra letter.

algae2 <- data.frame(ID = ID,
                     lake = temp$Col1,
                     station = temp$Col2,
                     depth_ft = temp_Depth,
                     date = format(temp$Col4, "%Y%m%d" ),
                     taxa = temp$Col6,
                     cell_per_l = temp$Col7,
                     BV.um3.L = temp$Col8,  ## how can I be certain about these units?
                     class = NA,
                     hab = FALSE,
                     sheet_id = temp$sheet_id )


ii <- as.Date(algae2$date, "%Y%m%d") > "2014-03-23"
# algae$date[ii] <- "20070820"; drop - incorrect date
algae2 <- algae2[!ii, ]

chunck_check(algae)
chunck_check(algae1)
chunck_check(algae2)

algae0 <- rbind(algae2, algae1, algae)

grep(pattern="'", algae$taxa)
setwd(homeDir)

if(WRITE){
  write.table(algae0, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}

