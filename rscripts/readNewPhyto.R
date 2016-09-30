################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in 
#### the 1988 / 2012 / 2014 files provided by Nathan Smucker in April 2016
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## 1988 data
id1988 <- which(grepl("1988DataFilesNJSdownload", OUT$full_file_name) & !OUT$processed)
OUT1988 <- OUT[id1988,]
dat <- NULL
for( i in 1:nrow(OUT1988)){
  # i = 1
  err <-    try( excel_sheets(OUT1988$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}  
  temp <- read_excel(OUT1988$full_file_name[i], sheet=OUT1988$sheet[i])
  if(OUT1988$full_file_name[i] == "1988DataFilesNJSdownload/rrr21002880622.xls"){
    for(j in 1:4){
      temp[,j] <- temp[1,j]
    }
  }
  ## Some data sheets are reading in a bunch of all NA rows.
  numNA <- apply(temp,1, FUN = function(x){sum(is.na(x))})
  temp <- temp[!(numNA == ncol(temp)),]
  temp$iCheck <- i
  print(nrow(temp))
  temp$sheet_id <- OUT1988$sheet_id[i]
  dat <- rbind(dat,temp)
}
dat <- subset(dat, Group != "Totals")
OUT$script[id1988] <- "readNewPhyto.R"
OUT$processed[id1988] <- TRUE

## Format 1988 data
dat <- subset(dat, !is.na(Station) & !grepl("20R7",dat$Station))
## A couple of NA stations, and some Ohio River stations that don't need to be read.
dat$Station <- ifelse(nchar(dat$Station) == 9, dat$Station, paste("2",dat$Station, sep=""))
## One depth field has an error. The sheet indicates the depth is 005.
id <- grep("ml", dat$`Depth-ft`)
dat$`Depth-ft`[id] <- "005"

ID <- paste(dat$Station, 
            format.Date(as.Date(as.character(dat$`Date (yymmdd)`), format = "%y%m%d"),
                        format = "%Y%m%d"),
            rep("0000",nrow(dat)),
            formatC(as.numeric(dat$`Depth-ft`), width = 3, flag = "0"), sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5,9) ,
                    depth_ft = substr(ID,22,24 ),
                    date = substr(ID, start=10, stop=17),
                    taxa = dat$Species,
                    cell_per_l = as.numeric(dat$`Cells/ml` * 1000),
                    BV.um3.L = as.numeric(NA),
                    class = dat$Group,
                    hab = FALSE,
                    sheet_id = dat$sheet_id,
                    qual_replicate = NA)

# Save object for rbind'ing later
algae1988 <- algae


## 2012 data
id2012 <- which(grepl("2012PhytoDataFiles", OUT$full_file_name) & !OUT$processed)
OUT2012 <- OUT[id2012,]
## For each file that starts with a '2', there is a Sample Details worksheet with metadata, like station ID.
## This sheet also contains the Sample IDs, which are listed as different sheets.
## Harvest metadata from 'Sample Details', then loop through each sheet represented by
## the Sample ID column.

algae <- NULL
uniqueFiles <- unique(OUT2012$full_file_name)
for( i in 1:length(uniqueFiles)){
  # i = 1
  wb <- uniqueFiles[i]
  err <-    try( excel_sheets(uniqueFiles[i]) )
  if(class(err) == "try-error"){ print("Error")}else{
    # If the file name starts with a '2', grab metadata first and loop through sheets
    fn <- strsplit(wb, "/")[[1]][2]
    if(substr(fn,start=1,stop=1) == "2"){
      metaTmp <- read_excel(wb, sheet = "Sample Details")
      for(j in 1:nrow(metaTmp)){
        # j = 1
        sheetTmp <- read_excel(wb, sheet = trimws(as.character(metaTmp$`Sample ID`[j]),which="both"),skip = 8, col_names = TRUE)
        sheetTmp <- subset(sheetTmp, !is.na(sheetTmp$'Genus species'))
        sheetTmp <- subset(sheetTmp, !grepl("ml", sheetTmp$Division))
        if(nrow(sheetTmp) > 0){
          if(grepl("2GRR20120912",wb)){
            idTmp <- paste(metaTmp$Location[j],
                           metaTmp$`Sample Date`[j],
                           formatC(as.numeric(metaTmp[j,"Sample Time"]), width = 4, flag = "0"),
                           formatC(as.numeric(metaTmp[j,"Sample Depth"]), width = 3, flag = "0"),
                           sep="")
          }else if(grepl("2SRR20120919", wb)){
            idTmp <- metaTmp$Location[j] 
          }else {
            idTmp <- paste(metaTmp$Location[j],
                           formatC(as.numeric(metaTmp[j,"Sample Time"]), width = 4, flag = "0"),
                           formatC(as.numeric(metaTmp[j,"Sample Depth"]), width = 3, flag = "0"),
                           sep = "")
            
          }
          algaeTmp <- data.frame(ID = rep(idTmp,nrow(sheetTmp)),
                                 lake = rep(substr(idTmp, 2,4),nrow(sheetTmp)),
                                 station = rep(substr(idTmp, 5,9),nrow(sheetTmp)),
                                 depth_ft = rep(substr(idTmp,22,24 ),nrow(sheetTmp)),
                                 date = rep(substr(idTmp, start=10, stop=17),nrow(sheetTmp)),
                                 taxa = sheetTmp$`Genus species`,
                                 cell_per_l = as.numeric(sheetTmp$`Concentration (cell #/L)`),
                                 BV.um3.L = as.numeric(sheetTmp$`Total biovolume (µm3/L)`),
                                 class = rep(NA,nrow(sheetTmp)),
                                 hab = rep(FALSE,nrow(sheetTmp)),
                                 sheet_id = rep(OUT2012$sheet_id[OUT2012$full_file_name == wb &
                                                               trimws(OUT2012$sheet, which = "both") == metaTmp$`Sample ID`[j]],nrow(sheetTmp)))
          algae <- rbind(algae,algaeTmp)
        }
        
      }
      
    }else{ 
      # If the file name doesn't start with '2', read the 'Sample Results' sheet only
      # i = 7
      datTmp <- read_excel(wb, sheet = "Sample Results")
      datTmp <- subset(datTmp, !is.na(Location))
      idTmp <- paste(datTmp$Location,
                     datTmp$`Sample Date`,
                     formatC(as.numeric(datTmp$`Sample Time`), width = 4, flag = "0"),
                     formatC(as.numeric(datTmp$`Sample Depth`), width = 3, flag = "0"),
                     sep="")
      algaeTmp <- data.frame(ID = idTmp,
                             lake = substr(idTmp, 2,4),
                             station = substr(idTmp, 5,9),
                             depth_ft = substr(idTmp,22,24 ),
                             date = substr(idTmp, start=10, stop=17),
                             taxa = datTmp$Taxa,
                             cell_per_l = as.numeric(datTmp$`Cells/liter`),
                             BV.um3.L = as.numeric(datTmp$`Total biovolume (µm3/L)`),
                             class = NA,
                             hab = FALSE,
                             sheet_id = OUT2012$sheet_id[OUT2012$full_file_name == wb &
                                                           OUT2012$sheet == "Sample Results"])
      algae <- rbind(algae,algaeTmp)
    }
  }
  print(nrow(algae))
  print(i)
}

OUT$script[id2012] <- "readNewPhyto.R"
OUT$processed[id2012] <- TRUE

algae$qual_replicate <- NA
algae2012 <- algae


## Read algae 2014 data

## 2014 data
id2014 <- which(grepl("2014PhytoDataFilesNJSdownload", OUT$full_file_name) & !OUT$processed)
OUT2014 <- OUT[id2014,]

## Look for files that end in 'QP.xlsx', 'P.xlsx', or 'H.xlsx'.
## These are Quality files, algae data, and HAB data, respectively.
algae <- NULL
subFiles <- unique(subset(OUT2014, grepl("QP.xlsx", OUT2014$full_file_name) |
                   grepl("P.xlsx", OUT2014$full_file_name) |
                   grepl("H.xlsx",OUT2014$full_file_name))$full_file_name)

## Read in data from Sample Results tab in each file.
algae <- NULL
for( i in 1:length(subFiles)){
  # i = 6
  wb <- subFiles[i]
  err <-    try( excel_sheets(subFiles[i]) )
  if(class(err) == "try-error"){ print("Error")}else{
    sheetTmp <- read_excel(wb, sheet = 'Sample Results', col_names = TRUE)
    # Get rid of NA rows.
    sheetTmp <- sheetTmp[!is.na(sheetTmp$Location),]
    idTmp <- paste(sheetTmp$Location,
                   ifelse(class(sheetTmp$`Sample Date`) == "Date",
                          format(sheetTmp$`Sample Date`, "%Y%m%d"),
                          sheetTmp$`Sample Date`),
                   formatC(as.numeric(sheetTmp$`Sample Time`), width = 4, flag = "0"),
                   formatC(as.numeric(sheetTmp$`Sample Depth`), width = 3, flag = "0"),
                   sep = "")
    
    # Density column changes name
    densCol <- names(sheetTmp)[grepl("ells",names(sheetTmp)) | grepl("ensity",tolower(names(sheetTmp)))]
    # Total biovolume column changes names. Search for specific names.
    bvNms <- c("Total Biovolume (um3/L)", "Total biovolume (µm3/L)","Biovolume")
    bvCol <- names(sheetTmp)[names(sheetTmp) %in% bvNms]
    if(length(bvCol) == 0){
      tmpBV <- rep(NA,nrow(sheetTmp)) 
    }else {
      tmpBV <- sheetTmp[,bvCol]
    }
    
    algaeTmp <- data.frame(ID = idTmp,
                           lake = substr(idTmp, 2,4),
                           station = substr(idTmp, 5,9),
                           depth_ft = substr(idTmp,22,24 ),
                           date = substr(idTmp, start=10, stop=17),
                           taxa = sheetTmp$Taxa,
                           cell_per_l = as.numeric(sheetTmp[,densCol]),
                           BV.um3.L = tmpBV,
                           class = rep(NA,nrow(sheetTmp)),
                           hab = ifelse(grepl("H.xlsx",wb), TRUE, FALSE),
                           sheet_id = OUT2014$sheet_id[OUT2014$full_file_name == wb &
                                                         OUT2014$sheet == "Sample Results"],
                           qual_replicate = ifelse(grepl("QP.xlsx",wb), "Q", NA))
    algae <- rbind(algae,algaeTmp)
  }
}
algae2014 <- algae
OUT$script[id2014] <- "readNewPhyto.R"
OUT$processed[id2014] <- TRUE

names(algae1988)
names(algae2012)
names(algae2014)

out <- rbind(algae1988,algae2012,algae2014)
#chunck_check(out)


## Read in existing algae file, since the qual_replicate column is new
setwd(homeDir)
algae <- read.csv("processed_data/algae.csv")
algae$qual_replicate <- as.character(NA)
bothAlgae <- rbind(algae, out)

## Write
write.csv(bothAlgae, "processed_data/algae.csv", row.names = FALSE, col.names = TRUE)
