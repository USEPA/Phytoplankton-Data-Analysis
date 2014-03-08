### check 0703

setwd("originalData/algae/EFR Phytoplankton Data/")

#### batch 8
id <- grepl("EDD", OUT$sheet)

### files to explicitly skip first go around.

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readEDD.R"

OUT$skip[OUT$full_file_name %in% OUTsub2$full_file_name]   <- "Processsed by not incorporated.  Cannot resolve sample ID."


shortList <- subset(OUTsub2, ncol == 11)
#######

dimCheck <- 0
batch8 <- 12

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

temp11 <- batch8
shortList <- subset(OUTsub2, ncol == 21)
#######

dimCheck <- 0
batch8 <- 12

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

temp21 <- batch8



# wq_dat11 <- data.frame(location = xx$station,
#                      sample_date  = xx$date ,
#                      sample_time  = xx$time,
#                      sample_depth = xx$depth_ft ,
#                      lrl_tag_num  = NA,     
#                      analyte  = xx$Analyte,
#                      analyte_code = NA,
#                      result = xx$Result,
#                      units  = xx$UNITS,
#                      qualifiers =xx$Qualifiers , 
#                      detect_limit  = xx$LOD , 
#                      report_limit = xx$Reporting.Limit, 
#                      prep_method  =NA,     
#                      test_method  = NA,    
#                      df = NA,           
#                      lab_id = NA,           
#                      lab_sample_number = NA,
#                      analysis_date = NA,     
#                      imported  = NA,         
#                      sheet_id  = xx$sheet_id,         
#                      original = xx$original,        
#                      qual1  = xx$qual1,
#                      qual2 = xx$qual2,
#                      result_num2 = xx$result_num2,
#                      result_num = xx$result_num,       
#                      ID = xx$ID )

setwd(homeDir)

