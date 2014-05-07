###check 0307
setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl("^Algal", OUT$sheetNames)
id1 <- grepl("jade0424a", OUT$full_file_name) ## analyze new files separately

### files to explicitly skip first go around.

OUTsub2 <- OUT[(id & !id1)  & !OUT$processed, ]
#OUTsub2 <- OUT[(id & !id1)  , ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readAlgal.R"

#######

files <- unique(OUTsub2$full_file_name)

for (j in 1:length(files)) { 

  xlcFreeMemory()  
err <-    try( wb     <- loadWorkbook(files[j]) )
if(class(err) == "try-error") print( "File Missing") 

sheets <- getSheets(wb)
batch6 <- NULL
  shortList <- subset(OUTsub2, full_file_name == files[j] )
  

for(i in 1:length(sheets)){
  
  temp <- readWorksheet(wb, sheet=sheets[i], startRow=9)
  temp <- temp[!is.na(temp$Division), ]
  id <- "Col5" %in% names(temp)
  
  temp <- temp[, names(temp)!="Col5" ]
  
  if(ncol(temp) != 8){warning("Check Dimensions")}
  names(temp) <- c("Div", "Gen", "Syn", "Conc", 
                   "Rel", "Mean", "Tot", "Rel")
  
  temp$sample_id <- unlist(readWorksheet(wb, sheet=sheets[i], 
                                         startCol=2,endCol=2,startRow=5, endRow=5, header = FALSE) )
  if(id){nColid <- 9}else{nColid <- 8 }
  temp$date2 <- unlist(readWorksheet(wb, sheet=sheets[i], 
                                         startCol=nColid,endCol=nColid,startRow=3, endRow=3, header = FALSE, colTypes="character") )
  
  temp$sheet_id <- shortList$sheet_id[i]
  ### When column names don't match after i = 10, force them to match
 
    names(temp)[names(temp)=="Col6"] <- "Individual.biovolume..µm3."
  
   if(i == 1){
     batch6 <- temp 
   }else{
          batch6 <- rbind( batch6, temp) }
}
if (j == 1){ all <- batch6}else{
  all <-rbind(all, batch6)
}
  }  ## close j

AAA <- all

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

temp <- strptime(AAA$date2, format = "%Y-%m-%d %H:%M:%S")
algae$date <- format(temp, "%Y%m%d")

### fix date

i <- is.na(temp)
algae$date[i] <- "20090627"



chunck_check(algae)

setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = "\t", append= TRUE, col.names = FALSE)          
  
}
