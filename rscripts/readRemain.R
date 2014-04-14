### process remaining files; mostly WQ

# full_file_name  file	size	sheet
# Drew data/k/2010 data to date.xlsx	2010 data to date.xlsx	662597	78219
# Drew data/j/EFR DASLER Data Dump Chemical.xlsx	EFR DASLER Data Dump Chemical.xlsx	541604	Sheet1
# Drew data/k/EFR Field Data2010 2011.xlsx	EFR Field Data2010 2011.xlsx	59820	Sheet1
# Drew data/k/EFR_CCK May 2010.xlsx	EFR_CCK May 2010.xlsx	16653	Sheet1
# Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	376831	EFR_QC_SPLITS_NOT_IMPORTED
# Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	376831	20102011FIELD_DATA_NOT_IMPORTED
# Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	376831	2011_FIELD_DATA_NOT_IMPORTED
# Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx	376831	NonConformName_NOT_IMPORTED
# Drew data/k/efrsep2010field.xlsx	efrsep2010field.xlsx	22532	Sheet1
library(XLConnect)
library(plyr)
source("rscripts/helperFunctions.R")

setwd("originalData/algae/EFR Phytoplankton Data/")

id <- OUT$full_file_name == "Drew data/k/2010 data to date.xlsx"

### Read EFF Phyto.
### files to explicitly skip first go around.
OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readRemaining.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )

if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE, startRow=1, endRow = 8101)
temp$sheet_id <- OUTsub2$sheet_id

id <- nchar(temp$Sample.Description) == 24
xx <- temp[id, ]

wq_dat1 <- data.frame(location = substr(xx$Sample.Description, 2,4),
                      station = substr(xx$Sample.Description, 5,9),
                     sample_date  = substr(xx$Sample.Description, 10,17) ,
                     sample_time  =substr(xx$Sample.Description, 18,21) ,
                     sample_depth = substr(xx$Sample.Description, 22,24) ,
                     lrl_tag_num  = NA,     
                     analyte  = xx$Analyte ,
                     analyte_code = NA,
                     result = xx$Result,
                     units  = xx$UNITS,
                     qualifiers = xx$Qualifiers , 
                     detect_limit  = xx$LOD , 
                     report_limit = xx$Reporting.Limit, 
                     prep_method  =xx$PREP,     
                     test_method  = xx$Method,    
                     df = NA,           
                     lab_id = xx$Lab,           
                     lab_sample_number = NA,
                     analysis_date = NA,     
                     imported  = NA,         
                     sheet_id  = OUTsub2$sheet_id,         
                     ID = xx$Sample.Description           
)


id <- nchar(temp$Sample.Description) == 23
xx <- temp[id, ]

wq_dat2 <- data.frame(location = substr(xx$Sample.Description, 2,4),
                      station = substr(xx$Sample.Description, 5,8),
                      sample_date  = substr(xx$Sample.Description, 9,16) ,
                      sample_time  =substr(xx$Sample.Description, 17,20) ,
                      sample_depth = substr(xx$Sample.Description, 21,23) ,
                      lrl_tag_num  = NA,     
                      analyte  = xx$Analyte ,
                      analyte_code = NA,
                      result = xx$Result,
                      units  = xx$UNITS,
                      qualifiers = xx$Qualifiers , 
                      detect_limit  = xx$LOD , 
                      report_limit = xx$Reporting.Limit, 
                      prep_method  =xx$PREP,     
                      test_method  = xx$Method,    
                      df = NA,           
                      lab_id = xx$Lab,           
                      lab_sample_number = NA,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id,         
                      ID = xx$Sample.Description           
)

id <- nchar(temp$Sample.Description) == 25
xx <- temp[id, ]

wq_dat3 <- data.frame(location = substr(xx$Sample.Description, 2,4),
                      station = substr(xx$Sample.Description, 5,10),
                      sample_date  = substr(xx$Sample.Description, 11,18) ,
                      sample_time  =substr(xx$Sample.Description, 19,22) ,
                      sample_depth = substr(xx$Sample.Description, 23,25) ,
                      lrl_tag_num  = NA,     
                      analyte  = xx$Analyte ,
                      analyte_code = NA,
                      result = xx$Result,
                      units  = xx$UNITS,
                      qualifiers = xx$Qualifiers , 
                      detect_limit  = xx$LOD , 
                      report_limit = xx$Reporting.Limit, 
                      prep_method  =xx$PREP,     
                      test_method  = xx$Method,    
                      df = NA,           
                      lab_id = xx$Lab,           
                      lab_sample_number = NA,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id,         
                      ID = xx$Sample.Description           
)

## read bottom rows; not clear on the correct headers.
## resolved by Jake
temp <- readWorksheet(wb, sheet=OUTsub2$sheet[1], header=TRUE, startRow = 8102)
temp$sheet_id <- OUTsub2$sheet_id


wq_dat3 <- data.frame(location = substr(temp$ClientsampID, 2,4),
                      station = substr(temp$ClientsampID, 5,10),
                      sample_date  = substr(temp$ClientsampID, 11,18) ,
                      sample_time  =substr(temp$ClientsampID, 19,22) ,
                      sample_depth = substr(temp$ClientsampID, 23,25) ,
                      lrl_tag_num  = NA,     
                      analyte  = temp$R_Rslt ,
                      analyte_code = NA,
                      result = temp$Lab,
                      units  = temp$Col6,
                      qualifiers = temp$R_Units, 
                      detect_limit  = temp$PrepCodeNo, 
                      report_limit = NA, 
                      prep_method  =temp$Col5,     
                      test_method  = temp$Col5,    
                      df = NA,           
                      lab_id = NA,           
                      lab_sample_number = NA,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id,         
                      ID = temp$ClientsampID           
)

all <- rbind(wq_dat1, wq_dat2, wq_dat3)


####
id <- OUT$full_file_name == "Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx"

### Read EFF Phyto.
### files to explicitly skip first go around.

OUTsub2 <- OUT[id, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readRemaining.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[2]) )

if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet=OUTsub2$sheet[2], header=TRUE)


