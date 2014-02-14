setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)

id <- grepl(pattern="/k/", OUT$full_file_name)
OUTsub <- OUT[id, ]

OUTsub2 <- subset(OUTsub, file == "92RAWDAT.xls")

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

sheets <- getSheets(wb)
WQ1 <- NULL
for( i in 1:length(sheets)){
  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], startRow=6,header=FALSE)
  temp[,1] <- substr(temp[,1], 1, 9)
  temp$sheet_id <- OUTsub2$sheet_id[i]
  
  WQ1 <- rbind(WQ1, temp)
}

WQ1 <- subset(WQ1, !is.na(Col1) )

### 
headr <- readWorksheet(wb, sheet=sheets[i], startRow=3, endRow=3,header=FALSE)
names(WQ1)<- c(headr, "sheet_id")

WQ1$ID <- paste(WQ1$Station, 
                WQ1$Collection,  
                "9999",
                formatC(WQ1$Depth, width=3, flag = "0"), sep = "" )
  
algae <- data.frame(ID = WQ1$ID,
                   lake = substr(WQ1$ID, 2,4),
                   station = substr(WQ1$ID, 5,9)  ,
                   depth_ft = WQ1$Depth,
                   date = WQ1$Collection,
                   taxa = WQ1$Taxon,
                   cell_per_l = WQ1$Density,
                   BV.um3.L = WQ1$Biovolume,  ## how can I be certain about these units?
                   class = NA,
                   hab = FALSE,
                   sheet_id = WQ1$sheet_id

####

EFR - Phytoplankton Results (2).xls

OUTsub2 <- OUTsub[grepl("EFR - Phytoplankton Results", OUTsub$file) ,  ]

  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing")   
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i])
  temp$sheet_id <- OUTsub2$sheet_id[i]
  BBB <- temp

##

algae <- data.frame(ID = BBB$Sample.ID,
                    lake = substr(BBB$Sample.ID, 2,4),
                    station = substr(BBB$Sample.ID, 5,9),
                    depth_ft = 999,
                    date = substr(BBB$Sample.ID, start=10, stop=17),
                    taxa = BBB$Species,
                    cell_per_l = BBB$Number.of.Cells.L,
                    BV.um3.L = BBB$Biovolume.as.um3.L,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = BBB$sheet_id)

write.table(algae, "../../../processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          

####

OUTsub2 <- OUTsub[grepl("RUN", OUTsub$file,ignore.case=TRUE ) ,  ]

#### note some files are duplicates, I believe because of incorrectly copied dates.
iDrop <- c("EFR-2001-Run1.xls" , "EFR-2001-Run3.xls" )
OUTsub2 <- subset( OUTsub2, ! file %in% iDrop )

AAA <- NULL

for(i in 1:nrow(OUTsub2)){
  err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i])
  
  print(dim(temp))
  if(nrow(temp)==0)next
  temp$sheet_id <- OUTsub2$sheet_id[i]
  AAA <- rbind( AAA, temp)
}

### get headers 
wq_hdr <- read.table( "../../../processed_data/water_quality.csv", sep = ",", header = TRUE )       
library(reshape2)

xx <- melt( AAA, id.vars = c("Station", "Date", "Time", "Depth" , "sheet_id" ), variable.name="analyte", value.name="result")
xx <- cbind(xx, result_convert(xx$result) )

wq_dat <- data.frame(location = xx$Station,
                     sample_date  = xx$Date ,
                     sample_time  = xx$Time,
                     sample_depth = xx$Depth ,
                     lrl_tag_num  = NA,     
                     analyte  = xx$analyte ,
                     analyte_code = NA,
                     result = xx$result,
                     units  = NA,
                     qualifiers = NA , 
                     detect_limit  = NA , 
                     report_limit = NA, 
                     prep_method  =NA,     
                     test_method  = NA,    
                     df = NA,           
                     lab_id = NA,           
                     lab_sample_number = NA,
                     analysis_date = NA,     
                     imported  = NA,         
                     sheet_id  = xx$sheet_id,         
                     original = xx$original,        
                     qual1  = xx$qual1,           
                     result_num = xx$result_num,       
                     ID = paste(xx$Station, xx$Date, xx$Time, 
                                formatC(as.numeric(xx$Depth), flag = "0", width = 3)
                                )              
  )
wq_dat <- factor_2_character(wq_dat )

if(WRITE){
write.table(algae, "../../../processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          

write.table(wq_dat, "../../../processed_data/water_quality.csv", sep = ",", row.names=FALSE, col.names=FALSE, append = TRUE)                    
}
