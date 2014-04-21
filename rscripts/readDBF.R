## read dbf files in Drew/a
library(foreign)
dat <- 
  
  ###check 0307
  setwd("originalData/algae/EFR Phytoplankton Data/")

file <- "originalData/algae/EFR Phytoplankton Data/Drew data/a/93Algae.dbf"

dat <- read.dbf(file, as.is=TRUE)

datTemp <- strptime(dat$DATE, format = "%m/%d/%y")
datDepth <- formatC(dat$DEPTH, flag = "0", width 
                    = 3, digits=3)

algae <- data.frame(ID = AAA$sample_id,
                    lake = substr(AAA$sample_id, 2,4),
                    station = substr(AAA$sample_id, 5, 9),
                    depth_ft = substr(AAA$sample_id, 22, 24),
                    date = NA,
                    taxa = AAA$Gen,
                    cell_per_l = AAA$Conc,
                    BV.um3.L = AAA$Tot,  
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)
