################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### from 92Rawdat.xlsx
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
id <- grepl("92Rawdat.xlsx", OUT$file)
id2 <- grepl("summary", OUT$sheet)
OUTsub2 <- OUT[id  & !id2, ]
OUT$skip[id & id2] <- "Summary Table"
OUT$processed[id] <- TRUE
OUT$script[id] <- "readRawData.R"
shortList <- OUTsub2


## Read the data 
temp <- NULL
for( i in 1:18 ){ #  i = 1
  wb <- unique(shortList$full_file_name)
  temp1 <- read_excel(wb, sheet = shortList$sheet[i], skip=5, col_names = FALSE)
  temp1$sheet_id <- shortList$sheet_id[i]
  temp <- rbind(temp, temp1)
}


## Manually omit last sheet
temp1 <- read_excel(wb, sheet = shortList$sheet[19], skip=5, col_names = FALSE)
OUT$skip[OUT$full_file_name == shortList$full_file_name[19] &
           OUT$sheet == shortList$sheet[19]] <- "Inconsistent first column"

## Read header for names
hdr <- read_excel(wb, sheet = shortList$sheet[1], skip = 1)
names(temp) <- c(unlist( names(hdr) ), "sheet_id")
AAA <- temp
AAA$sample_id <- paste( temp$Station, temp$Collection, "9999", formatC(temp$Depth, flag = "0", width = 3  ), sep = "")


## drop poor format and inconsistent first column.
id <- nchar(AAA$sample_id) != 24
AAA <- AAA[!id, ]

algae <- data.frame(ID = AAA$sample_id,
                    lake = substr(AAA$sample_id, 2,4),
                    station = substr(AAA$sample_id, 5, 9),
                    depth_ft = substr(AAA$sample_id, 22, 24),
                    date = substr(AAA$sample_id, start=10, stop=17),
                    taxa = AAA$Taxon,
                    cell_per_l = AAA$Density,
                    BV.um3.L = AAA[, 14],
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id )

chunck_check(algae)
setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
  
}

