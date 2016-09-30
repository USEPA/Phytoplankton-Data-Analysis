################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### in DASLER/
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the DASLER files - not the Thomason files, though
id <- grepl("DASLER", OUT$sheet)
OUTsub2 <- OUT[id & OUT$processed == FALSE, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDASLER.R"


## Loop through the file names / sheets
for( i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet=OUTsub2$sheet[i])
    if(ncol(temp) >= 16){
      temp <- subset(temp, !is.na(Location))[,-c(16:ncol(temp))]
    }
    #temp <- temp[, names(temp) != "Analyte 1"]
    temp$sheet_id <- OUTsub2$sheet_id[i]
    temp$`Sample Date` <- as.Date(temp$`Sample Date`)
    if(i == 1){
      AAA <- temp
    }else {
    AAA <- rbind(temp, AAA)
    }
  }
}


## Put together the algae data
ID <- paste(AAA$Location, format(AAA$`Sample Date`, "%Y%m%d"), 
            formatC(AAA$`Sample Time`, width = 4, flag = "0"),
            formatC(AAA$`Sample Depth`, width = 3, flag = "0"), sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5,9) ,
                    depth_ft = substr(ID,22,24 ),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Taxa,
                    cell_per_l = as.numeric(AAA$`Density (#/l)`),
                    BV.um3.L = as.numeric(AAA$`Biovolume (um3/L)`),  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)

algae$taxa <- gsub(pattern='"', replacement = "", algae$taxa)
algae$taxa <- gsub(pattern="'", replacement = "", algae$taxa)


## Check
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
}