################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### in jade0623/
################

## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the jade0623 files
i <- grep("jade0623", OUT$full_file_name)
OUTsub <- OUT[i, ]

## Loop through files
files <- OUTsub$full_file_name
for( i in 1:length(files)){ # i = 2
  wb <- files[i]
  temp <- read_excel(wb, sheet=1)
  temp$sheet_id <- OUTsub$sheet_id[i]
   if(i == 1){xxx <- temp}else{
     names(temp) <- names(xxx) # Force names to be the same
     #xxx <- merge(xxx, temp, all = TRUE)
     xxx <- rbind(xxx, temp)
  }
}

## Write algae data
habSheets <- subset(OUTsub, grepl("HAB",OUTsub$file))$sheet_id # None
ID <- xxx$`Sample ID`
algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = xxx$Species,
                    cell_per_l = xxx$`Number of Cells/L`,
                    BV.um3.L = xxx$`Biovolume as um3/L`,  ## 0 or NA
                    class = NA,
                    hab = FALSE,   ### hard coded HAB, see issue 
                    sheet_id = xxx$sheet_id)


OUT$processed[OUT$full_file_name %in% unique(OUTsub$full_file_name)] <- TRUE
OUT$script[OUT$full_file_name %in% unique(OUTsub$full_file_name)] <- "readJade0623.R"

setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)            
}
