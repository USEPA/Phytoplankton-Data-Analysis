################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with 'EDD' in the sheet name
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
id <- grepl("EDD", OUT$sheet)
OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readEDD.R"
OUT$skip[OUT$full_file_name %in% OUTsub2$full_file_name]   <- "Processsed but not incorporated.  Cannot resolve sample ID."
shortList <- subset(OUTsub2, ncol == 11)


## All water quality data. Not read in. 
## FYI for future: note that the Sample IDs are
## weird and Matt couldn't ever resolve how to standardize them.
if(FALSE){
  dimCheck <- 0
  for(i in 1:nrow(shortList)){ # i = 1
    err <-    try( excel_sheets(shortList$full_file_name[i]) )
    if(class(err) == "try-error") next  
    wb <- shortList$full_file_name[i]
    temp <- read_excel(wb, sheet = shortList$sheet[i])
    if(i == 1){
      batch8 <- temp
    }
    temp$sheet_id <- shortList$sheet_id[i]
    xlcFreeMemory()
    print(dim(temp))
    dimCheck <- dimCheck + nrow(temp)
    
    if(i == 1){ batch8 <- temp}else{
      batch8 <- merge(batch8, temp, all = TRUE)
    } 
  }
  
  temp11 <- batch8
  shortList <- subset(OUTsub2, ncol == 21)
  #######
  
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
    if(i == 1){ batch8 <- temp}else{
      batch8 <- merge(batch8, temp, all = TRUE)
    } 
    
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
}

setwd(homeDir)

