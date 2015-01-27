## read dbf files in Drew/a
library(foreign)
 
  
  ###check 0307

setwd("originalData/algae/EFR Phytoplankton Data/")


file <- "Drew data/a/93Algae.dbf"

## note - dbf files not currently on master list.
# OUTsub2 <- OUT[id  & !OUT$processed, ]
# OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
# OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDBF.R"



dat <- read.dbf(file, as.is=TRUE)

datTemp <- strptime(dat$DATE, format = "%m/%d/%y")
datTemp <- format(datTemp, "%Y%m%d")

datDepth <- formatC(as.numeric(dat$DEPTH), flag = "0", width 
                    = 3, digits=3)

ID <- paste(dat$STATION, datTemp, "9999", datDepth, sep = "")

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = dat$DEPTH,
                    date = datTemp,
                    taxa = dat$TAXON,
                    cell_per_l = dat$DENSITY,
                    BV.um3.L = dat$TOTBV,  
                    class = NA,
                    hab = FALSE,
                    sheet_id = 999)

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
                    sheet_id = -999)

all <- rbind(algae, algae2)

all$taxa <- gsub(pattern="'","", all$taxa)
all$taxa <- gsub(pattern='"',"", all$taxa)

all$lake <- gsub("grr", "GRR", all$lake)
all$ID <- gsub("grr", "GRR", all$ID)

chunck_check(all)

setwd(homeDir)

if(WRITE){
  write.table(all, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}

