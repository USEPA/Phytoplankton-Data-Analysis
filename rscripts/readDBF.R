################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in dbf files
#### from Drew data / a
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Read dbf files in Drew/a
## Note - dbf files not currently on master list.
library(foreign)
file <- "Drew data/a/93Algae.dbf"
dat <- read.dbf(file, as.is=TRUE)
datTemp <- strptime(dat$DATE, format = "%m/%d/%y")
datTemp <- format(datTemp, "%Y%m%d")
datDepth <- formatC(as.numeric(dat$DEPTH), flag = "0", width 
                    = 3, digits=3)
ID <- paste(dat$STATION, datTemp, "9999", datDepth, sep = "")

algae1 <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = dat$DEPTH,
                    date = datTemp,
                    taxa = dat$TAXON,
                    cell_per_l = dat$DENSITY,
                    BV.um3.L = dat$TOTBV,  
                    class = NA,
                    hab = FALSE,
                    sheet_id = NA)


## Second DBF
file <- "Drew data/a/ALGAE-95.dbf"
dat <- read.dbf(file, as.is=TRUE)
datTemp <- format(dat$DATE, "%Y%m%d")
datDepth <- gsub(pattern="'", "",dat$DEPTH)
datDepth <- gsub(pattern="0-", "",datDepth)
datDepth <- formatC(as.numeric(datDepth), flag = "0", width 
                    = 3, digits=3)

ID <- paste("2", dat$LAKE, dat$STATION, datTemp, "9999", datDepth, sep = "")

algae2 <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = datDepth,
                    date = datTemp,
                    taxa = dat$TAXA,
                    cell_per_l = dat$CELLS_L,
                    BV.um3.L = dat$BV_L,  
                    class = NA,
                    hab = FALSE,
                    sheet_id = NA)

algae <- rbind(algae1, algae2)

algae$taxa <- gsub(pattern="'","", algae$taxa)
algae$taxa <- gsub(pattern='"',"", algae$taxa)

algae$lake <- gsub("grr", "GRR", algae$lake)
algae$ID <- gsub("grr", "GRR", algae$ID)

chunck_check(algae)

setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          
}

