################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### in jade042414/
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the jade042414 files
OUT <- factor_2_character(OUT)
id <- grepl(pattern="jade042414", OUT$full_file_name)
jade <- OUT[id  & !OUT$processed, ]

### Change OUT df
OUT$script[OUT$full_file_name %in% jade$full_file_name] <- "readJade.R"
OUT$processed[OUT$full_file_name %in% jade$full_file_name] <- TRUE


## Don't read sheets starting with 121.
iii <- grep("^121", jade$sheet )
jade$processed[iii] <- TRUE
jade$script[iii]    <- "readJade042414.R"
jade$skip[iii]      <- "Not processed, data read by merging sample results and details."
sum(jade$processed)


## Manually read data from 2 files
xxx <- NULL
## First file
files <- "jade042414/2GRR20120912 No Biovolume Phytoplankton.xlsx"
for(j in 1:length(files)){ # j = 1
  err <-    try( excel_sheets(files[j]) )
  if(class(err) == "try-error"){print( "File Missing") }else {
    wb <- files[j]
    wsheets <- excel_sheets(wb) 
    tempResults <- read_excel(wb, sheet="Sample Results" )
    tempDetails <- read_excel(wb, sheet="Sample Details" )
    tempBoth <- merge(tempResults, tempDetails, all.x = TRUE )
    tempBoth$sheet_id <- subset(OUT, sheet == "Sample Results" & full_file_name == files[j] )$sheet_id
    if(j == 1){
      xxx <- tempBoth
    }else { xxx <- merge(xxx, tempBoth, all = TRUE)}
  } 
}

## Cross these sheets off the list
i <- jade$full_file_name %in% files
jade$processed[i] <- TRUE
sum(jade$processed)
groupA <- xxx

## Second file
files <- "jade042414/2BRR20120911 Phytoplankton.xls" 
### this file contains meta information on the sheet with info on "Sample Details"
for(j in 1:length(files)){ # j = 1
  err <-    try( excel_sheets(files[j]) )
  if(class(err) == "try-error"){print( "File Missing") }else {
    wb <- files[j]
    wsheets <- excel_sheets(wb)  
    tempResults <- read_excel(wb, sheet="Sample Results" ) 
    tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Results"]  
    wsheets <-   wsheets[grep("^121",wsheets)]
    
    for( i in 1:length(wsheets) ){ # i = 1
      tempSheet <- read_excel(wb, sheet=wsheets[i], skip=8)[,1:8]
      tempSheet <- tempSheet[!is.na(tempSheet$`Genus species`), ]
      tempSheet$sheet_id <- jade$sheet_id[ jade$full_file_name == files[j] & jade$sheet==wsheets[i]]
      tempSheet$Sample.ID <- wsheets[i]
      if(i == 1){ xxx <- tempSheet}else{ 
        xxx <- merge(xxx, tempSheet, all = TRUE)}
    }
    tempBoth <- merge(tempResults[, 1:5], xxx, all = TRUE, by.x = "Sample ID",
                      by.y = "Sample.ID") 
  }
}

groupAA <- tempBoth

## Cross these sheets off the list
jade$processed[jade$full_file_name == "jade042414/2BRR20120911 Phytoplankton.xls"] <-TRUE
sum(jade$processed)

## Drop files with 3 digit sheet names
i <- nchar(jade$sheet) == 3
jade$processed[i] <- TRUE
jade$skip <- "Not processed, data read by merging sample results and details."
sum(jade$processed)


## This section is deprecated; it doesn't appear to do anything.
if(FALSE){
  ## in sequence files with one row have numeric sheets with algae data, without volume
  i <- jade$nrow == 1 
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
}

## Grab sample results, merge with sample details
i <- jade$sheet == "Sample Results"  & !jade$processed 
files <- unique(jade$full_file_name[i])
for(j in 1:length(files)){ # j = 1
  err <-    try( excel_sheets(files[j]) )
  if(class(err) == "try-error"){print( "File Missing") }else {
    wb <- files[j]
    #  tempDetails <- readWorksheet(wb, sheet="Sample Details" ) 
    tempResults <- read_excel(wb, sheet = "Sample Results")
    ind <- grepl("^Col", names(tempResults) )
    tempResults <- tempResults[,!ind]
    names(tempResults) <- gsub("[^[:alnum:] ]", replacement="", names(tempResults))
    names(tempResults) <- gsub(" ", replacement="", names(tempResults))
    names(tempResults)[names(tempResults) %in% c( "Cellsliter", "DensitycellsL")] <- "CellsL"  
    tempResults$sheet_id <- jade$sheet_id[jade$full_file_name == files[j] & jade$sheet == "Sample Results"]  
    ii <- is.na(tempResults$Location)
    tempResults <- tempResults[!ii, ]
    
    if(j == 1){xxx <- tempResults} else{
      xxx <- merge(xxx, tempResults, all = TRUE) }
  } 
}

groupC <- xxx

## Fix some biovolume errors
i <- is.na(groupC$TotalBiovolumeum3L)
groupC$TotalBiovolumeum3L[i] <- groupC$Totalbiovolumeµm3L[i]
jade$processed[jade$full_file_name %in% files] <- TRUE


### Put together the algae data for groupA, groupAAA, groupB, groupC
habSheets <- subset(jade, grepl("HAB",jade$file))$sheet_id 
# 0 HAB files above, so we'll hard-code the HAB column as FALSE

## groupA
ID <- paste(groupA$Location, groupA$`Sample Date`, 
            formatC(as.numeric(groupA$`Sample Time`), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupA$`Sample Depth`), flag = "0", digits=3, width=3), sep = "" )
algae1 <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = groupA$Taxa,
                    cell_per_l = groupA$`Cells/liter`,
                    BV.um3.L = groupA$`Total Biovolume (µm3/L)`,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = groupA$sheet_id)

## groupAA
ID <- paste(groupAA$Location, #format(groupAA$Sample.Date, "%Y%m%d"), 
            formatC(as.numeric(groupAA$`Sample Time`), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupAA$`Sample Depth`), flag = "0", digits=3, width=3), sep = "" )

algae2 <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = groupAA$`Genus species`,
                     cell_per_l = groupAA$`Concentration (cell #/L)`,
                     BV.um3.L = groupAA$`Total biovolume (µm3/L)`,  ## 0 or NA
                     class = NA,
                     hab = FALSE,   ### hard coded HAB, see issue 
                     sheet_id = groupAA$sheet_id)

# This is old code, I believe.
if(FALSE){
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
                       hab = TRUE,
                       sheet_id = groupB$sheet_id)
}


## groupC
ID <- paste(groupC$Location, 
            groupC$SampleDate, 
            formatC(as.numeric(groupC$SampleTime), flag = "0", digits=4, width=4), 
            formatC(as.numeric(groupC$SampleDepth), flag = "0", digits=3, width=3), sep = "")

# Some sample IDs are messed up...
i <- nchar(ID) != 24  
ID[i]
# Swap out 'NONAME5' with something equally meaningless but with 5 characters...
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
                     hab = FALSE,   
                     sheet_id = groupC$sheet_id)


#### Combine different sets of algae data
algae <- rbind(algae1, algae2, algae4)

## Fix some dates
yy <- as.Date(as.character(algae$date), format = "%Y%m%d")
i <- is.na(yy)
## The dates are backwards
algae$date[i] <- paste(substr(algae$date[i], 5,8),
                       substr(algae$date[i], 1,4),
                       sep = "")
setwd(homeDir)
chunck_check(algae)


if(WRITE){
  write.csv(algae, "processed_data/algae.csv", row.names=FALSE,col.names = TRUE)          
  
}

