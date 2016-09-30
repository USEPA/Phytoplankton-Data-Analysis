################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in CTL files
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset of jade0424a files
id <- grepl("CTL", OUT$full_file_name) & !OUT$processed
OUT$processed[id] <- TRUE
OUT$script[id] <- "readCTL.R"
OUTsub <- OUT[id, ]
OUTsub <- subset(OUTsub, sheet == "Sample Results")
files <- unique(OUTsub$full_file_name)

## Read data
## Check names
nmsList <- list()
for( i in 1:length(files)){ # i = 5
  wb <- files[i]
  temp <- read_excel(wb, sheet="Sample Results")
  nmsList[[i]] <- names(temp)
}
## Some sheets with "" in cells - causes problems. Cutoff data at 19 columns
for( i in 1:length(files)){ # i = 5
  wb <- files[i]
  temp <- read_excel(wb, sheet="Sample Results")[,1:19]
  temp <- temp[, names(temp)!= "Date Analyzed" ]
  temp$sheet_id <- OUTsub$sheet_id[i]
  if(i == 1){ xxx <- temp }else{
    xxx<- rbind(xxx, temp)
  }
}

## Formatting
xxx$Location <- gsub("NONAME5", "99999", xxx$Location)
ID <- paste(xxx$Location, 
            xxx$`Sample Date`, 
            formatC(as.numeric(xxx$`Sample Time`), flag = "0", digits=4, width=4), 
            formatC(as.numeric(xxx$`Sample Depth`), flag = "0", digits=3, width=3), sep = "")
i <- nchar(ID) != 24  # All of these have NA for every entry
xxx <- xxx[!i,]
ID <- ID[!i]


## algae df
algae <- data.frame(ID = ID,
                     lake = substr(ID, 2,4),
                     station = substr(ID, 5, 9),
                     depth_ft = substr(ID, 22, 24),
                     date = substr(ID, start=10, stop=17),
                     taxa = xxx$Taxa,
                     cell_per_l = xxx$`Cells/liter`,
                     BV.um3.L = xxx$`Total biovolume (µm3/L)`, 
                     class = NA,
                     hab = FALSE, 
                     sheet_id = xxx$sheet_id)

## Some dates are backwards.
yy <- as.Date(as.character(algae$date), format = "%Y%m%d")
i <- is.na(yy)
algae$date[i] <- paste(substr(algae$date[i], 5,8),
                       substr(algae$date[i], 1,4),
                       sep = "")

## Write data
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)            
}
