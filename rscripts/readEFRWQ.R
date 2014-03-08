### check 0307

setwd("originalData/algae/EFR Phytoplankton Data/")

#### batch 8

txt <- "Sample.Description; Lab; PREP; Method; UNITS; Result; LOD; Reporting.Limit; Qualifiers; Analyte; Sample"
id <- grepl(txt, OUT$sheetNames)

### files to explicitly skip first go around.
id2 <- grepl("graph", OUT$full_file_name,ignore.case=TRUE)
id3 <- grepl("organized", OUT$full_file_name,ignore.case=TRUE)
id4 <- grepl("Data Q&A", OUT$full_file_name,ignore.case=TRUE) #### do not include because all files are parsed, split up and contain figures.

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readEFRWQ.R"

shortListA <- OUT[id & !(id2 | id3| id4), ]

OUT$batch[id & !(id2 | id3)] <- "batch8"

### break into groups based on ncol
shortList <- subset(shortListA, ncol == 11)
dimCheck <- 0

for(i in 1:nrow(shortList)){
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next  
  
  temp <- readWorksheet(wb, shortList$sheet[i])
  if(i == 1){
    batch8 <- temp
  }
  temp$sheet_id <- shortList$sheet_id[i]
  xlcFreeMemory()
  print(dim(temp))
  dimCheck <- dimCheck + nrow(temp)
  batch8 <- merge(batch8, temp, all = TRUE)
  
}

batch8_ncol11 <- batch8

shortList <- subset(shortListA, ncol == 14)
if(FALSE){
dimCheck <- 0

for(i in 1:nrow(shortList)){
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next  
  
  temp <- readWorksheet(wb, shortList$sheet[i])
  if(i == 1){
    batch8 <- temp
  }
  temp$sheet_id <- shortList$sheet_id[i]
  xlcFreeMemory()
  print(dim(temp))
  print(tail(temp))
  dimCheck <- dimCheck + nrow(temp)
  batch8 <- merge(batch8, temp, all = TRUE)
  
}
}


batch8_ncol14 <- batch8

shortList <- subset(shortListA, ncol == 15)

dimCheck <- 0

for(i in 1:nrow(shortList)){
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next  
  
  temp <- readWorksheet(wb, shortList$sheet[i])
  if(i == 1){
    batch8 <- temp
  }
  temp$sheet_id <- shortList$sheet_id[i]
  xlcFreeMemory()
  print(dim(temp))
  dimCheck <- dimCheck + nrow(temp)
  batch8 <- merge(batch8, temp, all = TRUE)
  
}

sum(batch8$Analyte != batch8$Analyte.1 )
sum(batch8$Result != batch8$Result.1 )
sum(batch8$Sample.Description != batch8$Sample.Description.1 )

batch8_ncol15 <- batch8[, !names(batch8)%in% c("Col12", "Analyte.1", "Sample.Description.1", "Result.1"  )]

#### merge

all <- merge(batch8_ncol11, batch8_ncol15, all = TRUE)
setwd(homeDir)

id <- nchar(all$Sample.Description) == 24
fix_samp_desciption <- all[!id, ]
rest <- all[id, ]

rest <- cbind(rest, result_convert(rest$Result) )
xx <- split_sampleID(rest$Sample.Description)
xx <- cbind(rest, xx)

wq_dat <- data.frame(location = xx$station,
                     sample_date  = xx$date ,
                     sample_time  = xx$time,
                     sample_depth = xx$depth_ft ,
                     lrl_tag_num  = NA,     
                     analyte  = xx$Analyte,
                     analyte_code = NA,
                     result = xx$Result,
                     units  = xx$UNITS,
                     qualifiers =xx$Qualifiers , 
                     detect_limit  = xx$LOD , 
                     report_limit = xx$Reporting.Limit, 
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
                     ID = xx$ID )

wq_dat<- wq_dat_EFRWQ <- wq_datK <- factor_2_character(wq_dat )


setwd(homeDir)


if(WRITE){
  write.table(wq_dat, "processed_data/water_quality.csv", sep = ",", row.names=FALSE, col.names=FALSE, append = TRUE)                    
            
}
