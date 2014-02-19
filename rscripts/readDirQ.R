library(XLConnect)
## to keep the out processed filed 
# OUT   <- read.table("output/reducedFileSurvey.csv", sep = ",", header = TRUE, as.is = TRUE)

# INFO  <- read.table("output/reducedFileList.csv", sep = ",", header = TRUE, as.is = TRUE)

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl(pattern="/q/", OUT$full_file_name)

OUTsub <- OUT[id & OUT$processed == FALSE, ]

OUTsub2 <- subset(OUTsub, file == "EFR2011 CT Data organized.xlsx")  ## one file
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE

OUTsub2 <- subset(OUTsub2, ! sheet %in% c( "ALL", "Sheet1", "Sheet2") )

err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )
if(class(err) == "try-error"){ print("Error")}

AAA  <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE)
dimCheck <- nrow(AAA)
### lesson learned today.  You can't merge a NULL to a data.frame and get anything returned.

for( i in 2:nrow(OUTsub2)){
    
  temp <- readWorksheet(wb, sheet=OUTsub2$sheet[i], header=TRUE)
  temp <- temp[, names(temp) != "Analyte.1"]
  
  dimCheck <- dimCheck + nrow(temp)
  
  temp$sheet_id <- OUTsub2$sheet_id[i]
  AAA <- merge(temp,AAA,  all.x = TRUE, all.y = TRUE )
  
}

if(dimCheck != nrow(AAA)) warning("Check your merge.")

### compare_columns, check for na


compColumns(AAA$Result, AAA$Result.1)
compColumns(AAA$Analyte, AAA$Analyte.1)


AAA <- AAA[, !names(AAA) %in% c("Result.1", "Analyte.1")]
AAA <- cbind(AAA,result_convert(AAA$Result))

### correct long sample id

id <- nchar(AAA$Sample.Description) != 24
temp <- gsub("-BETHEL", replacement="21001", AAA$Sample.Description[id]) ## replace with station info
temp <- paste(temp, "9999", sep = "")  ## missing time
temp <- paste(temp, "999", sep = "") ## missing depth
AAA$Sample.Description[id] <- temp



wq_dat <- data.frame(location = substr(AAA$Sample.Description, 5,9),
                     sample_date  = substr(AAA$Sample.Description, 10,17) ,
                     sample_time  = substr(AAA$Sample.Description, 18,21) ,
                     sample_depth = substr(AAA$Sample.Description, 22,24) ,
                     lrl_tag_num  = NA,     
                     analyte  = AAA$Analyte ,
                     analyte_code = NA,
                     result = AAA$original,
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
                     sheet_id  = AAA$sheet_id,         
                     original = AAA$original,        
                     qual1  = AAA$qual1,   
                     qual2  = AAA$qual2,   
                     result_num2 = AAA$result_num2,
                     result_num = AAA$result_num,       
                     ID = AAA$Sample.Description
                     )              


wq_dat <- factor_2_character(wq_dat )
wq_datQ <- wq_dat
setwd(homeDir)
if(WRITE){
  print(dim(wq_dat))
  print(head(wq_dat))  
write.table(wq_dat, "processed_data/water_quality.csv", sep = ",", row.names=FALSE, col.names=FALSE, append = TRUE)         
}