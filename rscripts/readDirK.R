################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### in k/
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## ## Subset the k/ files
OUT <- factor_2_character(OUT)
id <- grepl(pattern="/k/", OUT$full_file_name)
OUTsub <- OUT[id, ]

## 92RAWDAT.xls file first
OUTsub2 <- subset(OUTsub, file == "92RAWDAT.xls")
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirK.R"

wb <- OUTsub2$full_file_name[1]
sheets <- excel_sheets(wb)
alg <- NULL
for( i in 1:length(sheets)){ # i = 1
  temp <- read_excel(wb, sheet=OUTsub2$sheet[i], skip=5,col_names=FALSE)
  temp[,1] <- substr(temp[,1], 1, 9)
  temp$sheet_id <- OUTsub2$sheet_id[i]
  alg <- rbind(alg, temp)
}

alg <- subset(alg, !is.na(alg[,1]) )

### 
headr <- read_excel(wb, sheet=sheets[i], skip = 2, col_names = FALSE)[1,]
names(alg)<- c(headr, "sheet_id")

alg$ID <- paste(alg$Station, 
                   alg$Collection,  
                "9999",
                formatC(alg$Depth, width=3, flag = "0"), sep = "" )
  
algae <- data.frame(ID = alg$ID,
                   lake = substr(alg$ID, 2,4),
                   station = substr(alg$ID, 5,9)  ,
                   depth_ft = alg$Depth,
                   date = alg$Collection,
                   taxa = alg$Taxon,
                   cell_per_l = alg$Density,
                   BV.um3.L = alg[, 14],
                   class = NA,
                   hab = FALSE,
                   sheet_id = alg$sheet_id)


## EFR - Phytoplankton file
OUTsub2 <- OUTsub[grepl("EFR - Phytoplankton Results", OUTsub$file) ,  ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirK.R"

wb <-  OUTsub2$full_file_name  
temp <- read_excel(wb, sheet=OUTsub2$sheet)
temp$sheet_id <- OUTsub2$sheet_id[i]
BBB <- temp

## Algae object
algae1 <- data.frame(ID = BBB$`Sample ID`,
                    lake = substr(BBB$`Sample ID`, 2,4),
                    station = substr(BBB$`Sample ID`, 5,9),
                    depth_ft = substr(BBB$`Sample ID`, 22,24),
                    date = substr(BBB$`Sample ID`, start=10, stop=17),
                    taxa = BBB$Species,
                    cell_per_l = BBB$`Number of Cells/L`,
                    BV.um3.L = BBB$`Biovolume as um3/L`,  ## how can I be certain about these units?
                    class = NA,
                    hab = FALSE,
                    sheet_id = BBB$sheet_id)



## Files with 'RUN' in the name
OUTsub2 <- OUTsub[grepl("RUN", OUTsub$file,ignore.case=TRUE ) ,  ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirK.R"
## These are water quality data; ignore
if(FALSE){
  ## Note some files are duplicates, I believe because of incorrectly copied dates.
  iDrop <- c("EFR-2001-Run1.xls" , "EFR-2001-Run3.xls" )
  OUTsub2 <- subset( OUTsub2, ! file %in% iDrop )
  
  AAA <- NULL
  
  for(i in 1:nrow(OUTsub2)){ # i = 1
    err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
    if(class(err) == "try-error"){print( "File Missing") }else {
      wb <- OUTsub2$full_file_name[i]
      temp <- read_excel(wb, sheet=OUTsub2$sheet[i])
      # print(dim(temp))
      if(nrow(temp)==0)next
      temp$sheet_id <- OUTsub2$sheet_id[i]
      AAA <- rbind( AAA, temp)
    } 
  }
  
  ### get headers 
  
  ### wq_hdr <- read.table( "../../../processed_data/water_quality.csv", sep = ",", header = TRUE )       
  
  
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
                       qual2 = xx$qual2,
                       result_num2 = xx$result_num2,
                       result_num = xx$result_num,       
                       ID = paste(xx$Station, xx$Date, xx$Time, 
                                  formatC(as.numeric(xx$Depth), flag = "0", width = 3), 
                                  sep = ""
                       )              
  )
  wq_dat <- wq_datK <- factor_2_character(wq_dat )
}


setwd(homeDir)
chunck_check(algae)
chunck_check(algae1)

if(WRITE){
write.table(rbind(algae,algae1), "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)          

# print(dim(wq_dat))
# #print(head(wq_dat))
# write.table(wq_dat, "processed_data/water_quality.csv", sep = "\t", row.names=FALSE, col.names=FALSE, append = TRUE)                    
}
