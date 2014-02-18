### use objects from readData

### directory G
setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)
id  <- grepl(pattern="/g/", OUT$full_file_name)

OUTsub <- OUT[id, ]
OUTsub2 <- subset(OUTsub, file == "CELRL CT Water Quality Chemical Data 2012.xls")
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}
 
sheets <- getSheets(wb)
WQ1 <- NULL
for( i in 1:4){
  
  temp <- readWorksheet(wb, sheet=sheets[i], header=TRUE)
  names(temp) <- tolower(names(temp) )
  names(temp)[names(temp)== "lab_s_num"] <- "lab_sample_number"
  names(temp)[names(temp)== "analyte_name"] <- "analyte"
  names(temp)[names(temp)== "lab_sample_num"] <- "lab_sample_number"
  temp$sheet_id <- OUTsub2$sheet_id[i]
  WQ1 <- rbind(WQ1, temp)
}


### 

OUTsub2 <- subset(OUTsub, file == "CELRL RTI WQ Chemical Data 2012.xls")
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

sheets <- getSheets(wb)

WQ2 <- NULL
for( i in 1:4){
  
  temp <- readWorksheet(wb, sheet=sheets[i], header=TRUE)
  names(temp) <- tolower(names(temp) )
  names(temp)[names(temp)== "lab_s_num"] <- "lab_sample_number"
  names(temp)[names(temp)== "analyte_name"] <- "analyte"
  names(temp)[names(temp)== "lab_sample_num"] <- "lab_sample_number"
  temp$sheet_id <- OUTsub2$sheet_id[i]
  WQ2 <- rbind(WQ2, temp)
}

#### merge WQ1 and WQ2
WQ_all <- merge(WQ1, WQ2, all = TRUE )
WQ_all <- cbind(WQ_all, result_convert(WQ_all$result))

WQ_all <- subset(WQ_all, !is.na(result))
WQ_all$ID <- paste(WQ_all$location, WQ_all$sample_date, WQ_all$sample_time, WQ_all$sample_depth, sep = "")
### table WB


#### look at HAB files

OUTsub2 <- OUTsub[grepl("HAB", OUTsub$file) & grepl("Cyanobacterial.Analysis.Report", OUTsub$sheetNames),  ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE


AAA <- NULL

for(i in 1:nrow(OUTsub2)){
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], startRow=8)
  temp <- temp[!is.na(temp$Division), ]
  
  temp$sample_id <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                         startCol=1,endCol=1,startRow=3, endRow=3, header = FALSE) )

  temp$sample_date <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                         startCol=3,endCol=3,startRow=3, endRow=3, header = FALSE) )
  
  temp$sheet_id <- OUTsub2$sheet_id[i]
  AAA <- rbind( AAA, temp)
}


AAA$hab <- TRUE


algae0 <- data.frame(ID = AAA$sample_id,
                     lake = substr(AAA$sample_id, 2,4),
                     station = substr(AAA$sample_id, 5, 9),
                     depth_ft = substr(AAA$sample_id, 22, 24),
                     date = substr(AAA$sample_id, start=10, stop=17),
                     taxa = AAA$Genus,
                     cell_per_l = AAA$Concentration..cells.mL.,
                     BV.um3.L = NA,  ## how can I be certain about these units?
                     class = NA,
                     hab = TRUE,
                     sheet_id = AAA$sheet_id)

##

OUTsub2 <- OUTsub[grepl("^efr", OUTsub$file) & grepl("^Lake", OUTsub$sheetNames),  ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE


BBB <- NULL

for(i in 1:nrow(OUTsub2)){
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i])
  temp$sheet_id <- OUTsub2$sheet_id[i]
  BBB <- merge(BBB, temp, all = TRUE)
}


BBB$Depth<- as.numeric(gsub("\\'", "", BBB$Depth) )
BBB$ID <- paste("2",## assuming 2
                BBB$Lake, BBB$Station,                           
                format(BBB$Date, "%Y%m%d"), 
                "9999",
                formatC(BBB$Depth, width=3, flag = "0"), sep = "")      

##




algae1 <- data.frame(ID = BBB$ID,
                    lake = BBB$Lake,
                    station = BBB$Station,
                    depth_ft = BBB$Depth,
                    date = substr(BBB$ID, start=10, stop=17),
                    taxa = BBB$Taxa,
                    cell_per_l = BBB$Cells.L,
                    BV.um3.L = BBB$BV.L,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = BBB$sheet_id)

algae <- rbind(algae1, algae0)

setwd(homeDir)
if(WRITE){
write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",")                    
write.table(WQ_all, "processed_data/water_quality.csv", sep = ",", row.names=FALSE)                    
}  
