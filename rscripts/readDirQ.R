library(XLConnect)

OUT   <- read.table("output/reducedFileSurvey.csv", sep = ",", header = TRUE, as.is = TRUE)
INFO  <- read.table("output/reducedFileList.csv", sep = ",", header = TRUE, as.is = TRUE)

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- grepl(pattern="/q/", OUT$full_file_name)
OUTsub <- OUT[id, ]

OUTsub2 <- subset(OUTsub, file == "EFR2011 CT Data organized.xlsx")  ## one file
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
temp <- gsub("-BETHEL", replacement="XXXXX", AAA$Sample.Description[id]) ## replace with station info
temp <- paste(temp, "9999", sep = "")  ## missing time
temp <- paste(temp, "999", sep = "") ## missing depth
AAA$Sample.Description[id] <- temp

wq_dat <- data.frame(location = substr(AAA$Sample.Description, , ),
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

write.table(wq_dat, "../../../processed_data/water_quality.csv", sep = ",", row.names=FALSE, col.names=FALSE, append = TRUE)         
