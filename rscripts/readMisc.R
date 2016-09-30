################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in data
#### with certain sheet names in the file
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset the files
id <- OUT$sheetNames == "LAKE; STATION; I_D__; DATE; DEPTH; TAXA; CELLS_L; BV_UM3_L"
### Read EFF Phyto.
OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"


## Read the data
for( i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet=OUTsub2$sheet[i], col_names = TRUE)
    if(max( class(temp$DATE) == "POSIXct" ) ) temp$DATE <- as.character(temp$DATE)
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(i == 1){ AAA <- temp}else{
      AAA <- rbind(AAA, temp)
    }
  }  
}


## Some formatting
depth <- as.numeric(gsub(pattern="'", "", AAA$DEPTH ) )
temp <- substr(AAA$DATE, 1, 10)
date <- gsub(pattern="-", "", temp )
ID <- paste("2", AAA$LAKE, AAA$STATION, date, "9999", 
            formatC(depth, flag = "0", width=3), sep = "")

## Algae DF
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


## Some dates show up as 1902 for some reason -- Excel weirdness, maybe?
ii <- grepl("^1902", algae$date)
algae$date[ii] <- gsub(pattern="^1902",replacement="2002", algae$date[ii] )


## Drop data with ambiguous dates
i <- grepl("^2", algae$date)           
algae <- algae[i,]


## Save for later
algae1 <- algae



#### Second set of files

## Subset the files
id <- grepl("^Lake", OUT$sheetNames, ignore.case=TRUE)
OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"


## Read the data
## Check the names
nmsList <- list()
for( i in 1:nrow(OUTsub2)){ # i = 1
  wb <- OUTsub2$full_file_name[i]
  temp <- read_excel(wb, sheet=OUTsub2$sheet[i], col_names =TRUE)
  nms <- names(temp)
  nmsList[[i]] <- nms
}
for( i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet=OUTsub2$sheet[i], col_names =TRUE)
    nms <- names(temp)
    nms <- gsub(  "_", ".", nms )
    nms <- tolower(nms)
    names(temp) <- nms
    if(ncol(temp)==9){ 
      nmsDrop <- grepl("sp", nms) | is.na(nms)
      temp <- temp[, !nmsDrop]
    }
    if(max( class(temp$date) == "POSIXct" ) ) temp$date <- as.character(temp$date)
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(i == 1){ AAA <- temp}else{
      names(temp) <- names(AAA)
      AAA <- rbind(AAA, temp)
    }
  }
}


## Formatting
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
                    cell_per_l = AAA$cells.l,
                    BV.um3.L = AAA$bv.l,
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

## Some bad dates
i <- grepl("^2020", algae$date)
algae <- algae[!i,]
print(paste("Dropping ", sum(i), " Records with bad dates"))

algae$taxa <- gsub(pattern='"', replacement = "", algae$taxa)
algae$taxa <- gsub(pattern="'", replacement = "", algae$taxa)

## Save for later
algae2 <- algae



#### Third set of files
id <- grepl("Drew data/j/EFR Data Phytoplankton zlw.xlsx", OUT$full_file_name, ignore.case=TRUE)
OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readMisc.R"


## Reat the data
wb <- "Drew data/j/EFR Data Phytoplankton zlw.xlsx"
err < try( excel_sheets(wb ))
if(class(err) == "try-error"){ print("Error")}else{
  temp <- read_excel(wb, sheet="LRN Phytoplankton Results ", col_names = TRUE)
  temp <- temp[!is.na(temp$Location), ]
}


## Formatting
# Date read in as 5 digits - Excel issue. Some somputers might not do that.
if(all(nchar(temp$`Sample Date`) == 5)){
  date <- as.Date(temp$`Sample Date`, origin = "1899-12-30")
  date <- gsub("-", "", as.character(date))
}else if(all(grepl("-",temp$`Sample Date`))){
  date <- gsub("-", "", temp$`Sample Date`)
}

depth <- formatC( temp$`Sample Depth`, width = 3, flag = "0")
ID <- paste(temp$Location, date, temp$`Sample Time`, depth, sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station =  substr(ID, 5,9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = temp$Taxa,
                    cell_per_l = temp$`Density (#/mL)` * 1000,
                    BV.um3.L = temp$`Biovolume (um3/mL)`, 
                    class = NA,
                    hab = FALSE,
                    sheet_id = OUTsub2$sheet_id[1])

algae$taxa <- gsub(pattern='"', replacement = "", algae$taxa)
algae$taxa <- gsub(pattern="'", replacement = "", algae$taxa)


## Save
algae3 <- algae


## Write data
algae <- rbind(algae1, algae2, algae3)
chunck_check(algae)

setwd(homeDir)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append=TRUE, col.names=FALSE)      
}  


