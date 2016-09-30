################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with 'Algal'Phyto' in the name, located in Drew data / l / 
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
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


## Read the data
dimCheck <- 0
for( j in 1:length(files)){ # j = 1
  err <-    try( excel_sheets(files[j]) )
  if(class(err) == "try-error"){print( "File Missing")}else {
    wb <- files[j]
    OUTsub3 <- subset(OUTsub2, full_file_name == wb )
    for(i in 1:nrow(OUTsub3)){ # i = 1
      #  print(i)
      temp <- read_excel(wb, sheet=OUTsub3$sheet[i], col_names = FALSE)
      temp <- temp[!is.na(temp[,1]), ]
      temp$sheet_id <- OUTsub3$sheet_id[i]
      dimCheck <- nrow(temp) + dimCheck
      if(i == 1 & j == 1){AAA <- temp} else{
        AAA <- merge(AAA, temp, all = TRUE)
      }} # end i loop 
  } # end else 
} # end j loop


## Fix depths and make ID vector
temp_Depth <- as.numeric(gsub("\'","", AAA[,5])  )
ID <- paste("2", AAA[,1], substr(AAA[,2], 1,5) , format(AAA[,4], "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")


algae <- data.frame(ID = ID,
                     lake = AAA[,1],
                     station = AAA[,2],
                     depth_ft = temp_Depth,
                     date = format(AAA[,4], "%Y%m%d" ),
                     taxa = AAA[,6],
                     cell_per_l = AAA[,8],
                     BV.um3.L = AAA[,9],
                     class = NA,
                     hab = FALSE,
                     sheet_id = AAA$sheet_id )


## Drop a few records - Matt indicated these are picked up in other files.
ii <- as.Date(algae$date, "%Y%m%d") > "2014-03-23"
# algae$date[ii] <- "20070820"; drop - incorrect date
algae <- algae[!ii, ]


## Other files of interest
id <- OUT$full_file_name == "Drew data/h/phytodata2005.xls"
OUT$processed[id] <- TRUE
OUT$script[id] <- "readDirL.R"

err <- try( excel_sheets(OUT$full_file_name[id] ) )
if(class(err) == "try-error"){print( "File Missing") }else {
  wb <- OUT$full_file_name[id]
  temp <- read_excel(wb, sheet=1)
  temp$sheet_id <- OUT$sheet_id[id]
  temp_Depth <- as.numeric(gsub("\'","", temp$DEPTH)  )
}

ID <- paste("2", temp$LAKE, temp$STATION, format(temp$DATE, "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")

algae1 <- data.frame(ID = ID,
                    lake = temp$LAKE,
                    station = temp$STATION,
                    depth_ft = temp_Depth,
                    date = format(temp$DATE, "%Y%m%d" ),
                    taxa = temp$TAXA,
                    cell_per_l = temp$CELLS_L,
                    BV.um3.L = temp$BV_UM3_L,
                    class = NA,
                    hab = FALSE,
                    sheet_id = temp$sheet_id )


## Another file
id <- OUT$full_file_name == "Drew data/g/Copy of 2007.xls"
OUT$processed[id] <- TRUE
OUT$script[id] <- "readDirL.R"

err <-    try( excel_sheets(OUT$full_file_name[id] ) )
if(class(err) == "try-error"){print( "File Missing") }else {
  wb <- OUT$full_file_name[id]
  temp <- read_excel(wb, sheet=1, col_names = FALSE)
  temp$sheet_id <- OUT$sheet_id[id]
  temp_Depth <- as.numeric(gsub("\'","", temp[,5]))
}
ID <- paste("2", temp[,1], substr(temp[,2], 1,5) , format(temp[,4], "%Y%m%d" ), 9999, formatC( temp_Depth, width = 3, flag = "0"), sep = "")


algae2 <- data.frame(ID = ID,
                     lake = temp[,1],
                     station = temp[,2],
                     depth_ft = temp_Depth,
                     date = format(temp[,4], "%Y%m%d" ),
                     taxa = temp[,6],
                     cell_per_l = temp[,7],
                     BV.um3.L = temp[,8],
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

setwd(homeDir)

if(WRITE){
  write.table(algae0, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
}

