###check 0307
setwd("originalData/algae/EFR Phytoplankton Data/")

#### batch 8


id <- grepl("^Algal", OUT$sheetNames)

### files to explicitly skip first go around.

OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readAlgal.R"



shortList <- OUTsub2
#######

i <- 1
err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
if(class(err) == "try-error") print( "File Missing") 

sheets <- getSheets(wb)
batch6 <- NULL

for(i in 1:20){
  
  temp <- readWorksheet(wb, sheet=sheets[i], startRow=9)
  temp <- temp[!is.na(temp$Division), ]
  
  temp$sample_id <- unlist(readWorksheet(wb, sheet=sheets[i], 
                                         startCol=2,endCol=2,startRow=5, endRow=5, header = FALSE) )
  temp$sheet_id <- shortList$sheet_id[i]
  ### When column names don't match after i = 10, force them to match
 
    names(temp)[names(temp)=="Col6"] <- "Individual.biovolume..µm3."
  
#   if(i > 1){
#     if( sum(names(temp) %in% names(batch6))!=9 ) { names(temp) <- names(batch6) }
#   }
  batch6 <- merge( batch6, temp, all =TRUE)
}

AAA <- batch6

algae <- data.frame(ID = AAA$sample_id,
                     lake = substr(AAA$sample_id, 2,4),
                     station = substr(AAA$sample_id, 5, 9),
                     depth_ft = substr(AAA$sample_id, 22, 24),
                     date = substr(AAA$sample_id, start=10, stop=17),
                     taxa = AAA$Genus.species,
                     cell_per_l = AAA$Concentration..cell...L.,
                     BV.um3.L = AAA$Total.biovolume..µm3.L.,  
                     class = NA,
                     hab = FALSE,
                     sheet_id = AAA$sheet_id)

chunck_check(algae)

setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}
