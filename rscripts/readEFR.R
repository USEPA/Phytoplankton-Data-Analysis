################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with EFR in the file name
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset the files
id <- grepl("^EFR", OUT$file, ignore.case=FALSE) & OUT$ncol == 9
id2 <- grepl("1988DataFiles", OUT$full_file_name)
OUTsub2 <- OUT[id  & !(OUT$processed | id2), ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readEFR.R"


## Read the data
for( i in 1:nrow(OUTsub2)){ # i = 1
  err <- try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet=OUTsub2$sheet[i], col_names = TRUE)
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(i == 1){ AAA <- temp}else{
      AAA <- rbind(AAA, temp)
    }
  } # end else
} # end for


## A few changes
date <- paste("19", AAA$`Date (yymmdd)`, sep = "")
depth <- formatC(AAA$`Depth-ft`, flag = "0", width = 3)
ID <- paste(AAA$Station, date, "9999", depth, sep = "")

AAA <- AAA[nchar(ID) == 24, ]
ID <- ID[nchar(ID) == 24]


algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Species,
                    cell_per_l = AAA$`Cells/ml` * 1000, ### convert to liters.
                    BV.um3.L = NA,  ## how can I be certain about these units?
                    class = AAA$Group,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)
chunck_check(algae)
setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
  
}

