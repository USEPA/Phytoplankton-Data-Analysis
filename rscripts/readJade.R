### second try, same directory.

setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)
id <- grepl(pattern="jade042414", OUT$full_file_name)

### subset of jade042414 files
jade <- OUT[id  & !OUT$processed, ]

## commented out for processing work.
# bkup <- jade
# bkupOUT <- OUT

#jade <- bkup
#OUT <- bkupOUT

### change OUT df
OUT$script[OUT$full_file_name %in% jade$full_file_name] <- "readJade.R"
OUT$processed[OUT$full_file_name %in% jade$full_file_name] <- TRUE


###
### id for full list


### don't read sheets starting with 121.
iii <- grep("^121", jade$sheet )
jade$processed[iii] <- TRUE
jade$script[iii]    <- "readJade042414.R"
jade$skip[iii]      <- "Not processed, data read by merging sample results and details."

sum(jade$processed)
####  manually read data from 2 files

xxx <- NULL

files <- "jade042414/2GRR20120912 No Biovolume Phytoplankton.xlsx"

for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" )
  tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Results"]  
  
  
  tempDetails <- readWorksheet(wb, sheet="Sample Details" )
  tempBoth <- merge(tempResults, tempDetails, all.x = TRUE )
  tempBoth$sheet_id <- subset(OUT, sheet == "Sample Results" & full_file_name == files[j] )$sheet_id
  if(j == 1){
    xxx <- tempBoth
  }else { xxx <- merge(xxx, tempBoth, all = TRUE)}
}


i <- OUT$full_file_name %in% files
OUT$processed[i] <- TRUE

groupA <- xxx
####
jade$processed[jade$full_file_name == "jade042414/2GRR20120912 No Biovolume Phytoplankton.xlsx"] <-TRUE

files <- "jade042414/2BRR20120911 Phytoplankton.xls" 
### this file contains meta information on the sheet with info on "Sample Details"
for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" ) 
  tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Results"]  
  
  wsheets <-   wsheets[grep("^121",wsheets)]
  
  for( i in 1:length(wsheets) ){
    tempSheet <- readWorksheet(wb, sheet=wsheets[i], startRow=9 ) 
    tempSheet <- tempSheet[!is.na(tempSheet$Genus.species), ]
    tempSheet$sheet_id <- jade$sheet_id[ jade$full_file_name == files[j] & jade$sheet==wsheets[i]]
    tempSheet$Sample.ID <- wsheets[i]
    if(i == 1){ xxx <- tempSheet}else{ 
    xxx <- merge(xxx, tempSheet, all = TRUE)}
  }
  tempBoth <- merge(tempResults[, 1:5], xxx, all = TRUE, by = "Sample.ID" ) 
}

groupAA <- tempBoth

###
jade$processed[jade$full_file_name == "jade042414/2BRR20120911 Phytoplankton.xls"] <-TRUE
sum(jade$processed)

### drop files with 3 digit sheet names

i <- nchar(jade$sheet) == 3
jade$processed[i] <- TRUE
jade$skip <- "Not processed, data read by merging sample results and details."

sum(jade$processed)

i <- jade$nrow == 1 ## in sequence files with one row have numeric sheets with algae data, without volume
files <- unique(jade$full_file_name[i])

jade$processed[jade$full_file_name %in% files] <- TRUE

for(j in 1:4){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  ws <- wsheets[!grepl("Sam", wsheets)]
  tempResults <- readWorksheet(wb, sheet="Sample Details" ) 
  tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Details"]  
  
  tempData <- readWorksheet(wb, sheet = ws, startRow=9)
  tempData <- tempData[!is.na(tempData$Division),]
  tempData$Location <- tempResults$Location
  tempData$Sample.Date <- tempResults$Sample.Date
  tempData$Depth <- tempResults$Sample.Depth
  tempData$Time <- tempResults$Sample.Time
  tempData$sheet_id <- tempResults$sheet_id
  if(j == 1){xxx <- tempData}else{
  xxx <- merge(xxx, tempData, all = TRUE) }
  
}

### a little cleanup

i <- nchar(xxx$Location) == 24
xxx$Time[i] <- substr(xxx$Location[i], 18,21)

###
groupB <- xxx
### grab sample results, merge with sample details

i <- jade$sheet == "Sample Results"  & !jade$processed ## in sequence files with one row have numeric sheets with algae data, without volume

files <- unique(jade$full_file_name[i])

for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
 
#  tempDetails <- readWorksheet(wb, sheet="Sample Details" ) 
  tempResults <- readWorksheet(wb, sheet = "Sample Results")

  ind <- grepl("^Col", names(tempResults) )
  tempResults <- tempResults[,!ind]
  names(tempResults) <- gsub("\\.", replacement="", names(tempResults))
  names(tempResults)[names(tempResults) %in% c( "Cellsliter", "DensitycellsL")] <- "CellsL"  
  tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Results"]  
  ii <- is.na(tempResults$Location)
  tempResults <- tempResults[!ii, ]

  if(j == 1){xxx <- tempResults} else{
  xxx <- merge(xxx, tempResults, all = TRUE) }



}

groupC <- xxx

i <- is.na(groupC$TotalBiovolumeum3L)
groupC$TotalBiovolumeum3L[i] <- groupC$Totalbiovolumeµm3L[i]

jade$processed[jade$full_file_name %in% files] <- TRUE


### groupA, groupAAA, groupB, groupC
ID <- paste(groupA$Location, groupA$Sample.Date, 
            formatC(as.numeric(groupA$Sample.Time), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupA$Sample.Depth), flag = "0", digits=3, width=3), sep = "" )
algae1 <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = groupA$Taxa,
                    cell_per_l = groupA$Cells.liter,
                    BV.um3.L = groupA$Total.Biovolume..µm3.L.,  ## how can I be certain about these units?
                    class = NA,
                    hab = TRUE,   ### hard coded HAB, see issue 
                    sheet_id = groupA$sheet_id)


ID <- paste(groupAA$Location, #format(groupAA$Sample.Date, "%Y%m%d"), 
            formatC(as.numeric(groupAA$Sample.Time), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupAA$Sample.Depth), flag = "0", digits=3, width=3), sep = "" )

algae2 <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = groupAA$Genus.species,
                     cell_per_l = groupAA$Concentration..cell...L.,
                     BV.um3.L = groupAA$Total.biovolume..µm3.L.,  ## 0 or NA
                     class = NA,
                     hab = TRUE,   ### hard coded HAB, see issue 
                     sheet_id = groupAA$sheet_id)

###groupB

ID <- groupB$Location
  i <- nchar(ID) != 24
ID[i] <- paste(substr( ID[i], 1, 4), "99999", substr( ID[i], 5, 12), 
               formatC(as.numeric(groupB$Time[i]), flag = "0", digits=4, width=4), 
               formatC(as.numeric(groupB$Depth[i]), flag = "0", digits=3, width=3),
               sep = ""
               )

algae3 <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = groupB$Genus.species,
                     cell_per_l = groupB$Concentration..cell...L.,
                     BV.um3.L = groupB$Total.biovolume..µm3.L.,  ## 0 or NA
                     class = NA,
                     hab = TRUE,   ### hard coded HAB, see issue 
                     sheet_id = groupB$sheet_id)
## groupC


ID <- paste(groupC$Location, 
            groupC$SampleDate, 
            formatC(as.numeric(groupC$SampleTime), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupC$SampleDepth), flag = "0", digits=3, width=3), sep = "")
 i <- nchar(ID) != 24  # fix sample date


ID <- gsub("NONAME5", "99999", ID)

algae4 <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = groupC$Taxa,
                     cell_per_l = groupC$CellsL,
                     BV.um3.L = groupC$TotalBiovolumeum3L,  ## 0 or NA
                     class = NA,
                     hab = TRUE,   ### hard coded HAB, see issue 
                     sheet_id = groupC$sheet_id)
#### combine

algae <- rbind(algae1, algae2, algae3, algae4)


yy <- as.Date(as.character(algae$date), format = "%Y%m%d")
i <- is.na(yy)
algae$date[i] <- paste(substr(algae$date[i], 5,8),
                       substr(algae$date[i], 1,4),
                       sep = "")
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}

