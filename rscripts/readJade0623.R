### process files from jade062
setwd("originalData/algae/EFR Phytoplankton Data/")

i <- grep("jade0623", OUT$full_file_name)
OUTsub <- OUT[i, ]

files <- unique(OUTsub$full_file_name)

for( i in 1:length(files)){
  
  wb <- loadWorkbook(files[i])
  temp <- readWorksheet(wb, sheet=1)
  temp$sheet_id <- OUTsub$sheet_id[i]
 
   if(i == 1){xxx <- temp}else{
   xxx <- merge(xxx,temp, all = TRUE)
  }
 
}

i <- !is.na(xxx$Biovolume.as.um3.L.)
xxx$Biovolume.as.um3.L[i] <- xxx$Biovolume.as.um3.L.[i]

ID <- xxx$Sample.ID
algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5, 9),
                    depth_ft = substr(ID, 22, 24),
                    date = substr(ID, start=10, stop=17),
                    taxa = xxx$Species,
                    cell_per_l = xxx$Number.of.Cells.L,
                    BV.um3.L = xxx$Biovolume.as.um3.L,  ## 0 or NA
                    class = NA,
                    hab = TRUE,   ### hard coded HAB, see issue 
                    sheet_id = xxx$sheet_id)
#### combine


OUT$processed[OUT$full_file_name %in% unique(OUTsub$full_file_name)] <- TRUE
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)            
}
