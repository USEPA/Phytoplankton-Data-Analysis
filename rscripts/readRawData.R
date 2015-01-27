## check 0307

setwd("originalData/algae/EFR Phytoplankton Data/")


id <- grepl("92Rawdat.xlsx", OUT$file)
id2 <- grepl("summary", OUT$sheet)

### files to explicitly skip first go around.

OUTsub2 <- OUT[id  & !id2, ]
OUT$skip[id & id2] <- "Summary Table"

OUT$processed[id] <- TRUE
OUT$script[id] <- "readRawData.R"

shortList <- OUTsub2
#######

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") print( "File Missing") 
temp <- NULL


for( i in 1:18 ){
  temp1 <- readWorksheet(wb, sheet = shortList$sheet[i], startRow=6, header = FALSE)
  temp1$sheet_id <- shortList$sheet_id[i]
  temp <- rbind(temp, temp1)
  
}

### mannually fix last sheet

temp1 <- readWorksheet(wb, sheet = shortList$sheet[19], startRow=6, header = FALSE)

OUT$skip[OUT$full_file_name == shortList$full_file_name[19]] <- "Inconsistent first column"

hdr <- readWorksheet(wb, sheet = shortList$sheet[i], startRow=3, endRow=3, header = FALSE)
names(temp) <- c(unlist(hdr ), "sheet_id")
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
                    BV.um3.L = AAA[, 14],  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id )

chunck_check(algae)
setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}

