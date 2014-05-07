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

###
id<- OUT$sheet == "NonConformName_NOT_IMPORTED"
OUT$processed[id ] <- TRUE
OUT$script[id   ] <- "readRemaining.R"
OUT$skip[id] <- "Not imported see issue 22, item 3"

##
id<- OUT$sheet == "EFR_QC_SPLITS_NOT_IMPORTED"
OUT$processed[id ] <- TRUE
OUT$script[id   ] <- "readRemaining.R"
OUT$skip[id] <- "Not imported see issue 22, item 3"


##
id<- OUT$sheet == "2011_FIELD_DATA_NOT_IMPORTED"
OUT$processed[id ] <- TRUE
OUT$script[id   ] <- "readRemaining.R"
OUT$skip[id] <- "Contains 1 record"



###

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


wq_dat4 <- data.frame(location = substr(temp$ClientsampID, 2,4),
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

all <- rbind(wq_dat1, wq_dat2, wq_dat3, wq_dat4)


####
id <- OUT$full_file_name == "Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx"
id1 <- OUT$sheet == "20102011FIELD_DATA_NOT_IMPORTED"
### Read EFF Phyto.
### files to explicitly skip first go around.

OUTsub2 <- OUT[(id & id1), ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readRemaining.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name[1]) )

if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet= "20102011FIELD_DATA_NOT_IMPORTED", header=TRUE, endCol=19, endRow = 823)


wq_dat1 <- data.frame(location = temp$LAKE,
                      station = substr(temp$Location, 5,10),
                      sample_date  = temp$S_DATE ,
                      sample_time  =temp$S_TIME,
                      sample_depth = temp$S_Depth,
                      lrl_tag_num  = NA,     
                      analyte  = temp$PARAMETER ,
                      analyte_code = temp$P_Code,
                      result = temp$RESULTS,
                      units  = temp$UNITS,
                      qualifiers = temp$Qualifiers, 
                      detect_limit  = NA, 
                      report_limit = temp$Reporting.Limit, 
                      prep_method  =  temp$Prep_Method,     
                      test_method  = temp$Test_Method,    
                      df = NA,           
                      lab_id = temp$Lab,           
                      lab_sample_number = NA,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id            
)
wq_dat1$ID <- paste( "2", wq_dat1$location, wq_dat1$station, wq_dat1$sample_date, wq_dat1$sample_time, temp$S_Depth, sep = "" )
      

all <- rbind(wq_dat1, all)
#####

####
id <- OUT$full_file_name == "Drew data/j/EFR DASLER Data Dump Chemical.xlsx"
id1 <- OUT$sheet == "Sheet1"

OUTsub2 <- OUT[id & id1, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readRemaining.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook("Drew data/j/EFR DASLER Data Dump Chemical.xlsx") )

if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet= 1, header=TRUE, colTypes=c("character", "integer", rep("character", 10)))
temp <- readWorksheet(wb, sheet= 1, header=TRUE )

##

wq_dat1 <- data.frame(location = substr(temp$Location...., 2,4),
                      station = substr(temp$Location, 5,10),
                      sample_date  = substr(temp$Sample.Number...., 1,8) ,
                      sample_time  = substr(temp$Sample.Number...., 9,12),
                      sample_depth = substr(temp$Sample.Number...., 13,15),
                      lrl_tag_num  = NA,     
                      analyte  = temp$Parameter.........................................,
                      analyte_code = temp$Parameter.Num,
                      result = temp$Value..........,
                      units  = temp$Units..........,
                      qualifiers = temp$Quals, 
                      detect_limit  = NA, 
                      report_limit = NA, 
                      prep_method  =  temp$Prep.Mthd.,     
                      test_method  = temp$Test.Mthd.,    
                      df = NA,           
                      lab_id = temp$Lab......................,           
                      lab_sample_number = temp$Lab.Sample.Number,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id            
)
wq_dat1$ID <- paste( "2", wq_dat1$location, wq_dat1$station, wq_dat1$sample_date, wq_dat1$sample_time, temp$S_Depth, sep = "" )

###

all <- rbind(all, wq_dat1)

##################

####
id <- OUT$full_file_name == "Drew data/k/EFR Field Data2010 2011.xlsx"
id1 <- OUT$sheet == "Sheet1"

OUTsub2 <- OUT[id & id1, ]
OUT$processed[id  & !OUT$processed ] <- TRUE
OUT$script[id  & !OUT$processed ] <- "readRemaining.R"

xlcFreeMemory()
err <-    try( wb     <- loadWorkbook(OUTsub2$full_file_name) )

if(class(err) == "try-error"){ print("Error")}  
temp <- readWorksheet(wb, sheet= 1, header=TRUE)

temp1 <- melt(temp, value.name="result",id.vars=c("Station", "Date.YR.MO.DA", "Time", "Depth"),
              measure.vars=c("Temp", "D.O.", "Sp.Cond",  "pH", "Turbidity", "Secchi" ) )
##
wq_dat1 <- data.frame(location = substr(temp1$Station, 2,4),
                      station = substr(temp1$Station, 5,10),
                      sample_date  = temp1$Date.YR.MO.DA ,
                      sample_time  = temp1$Time,
                      sample_depth = temp1$Depth,
                      lrl_tag_num  = NA,     
                      analyte  = temp1$variable,
                      analyte_code = NA,
                      result = temp1$result,
                      units  = NA,
                      qualifiers = NA, 
                      detect_limit  = NA, 
                      report_limit = NA, 
                      prep_method  =  NA,     
                      test_method  = "Field",    
                      df = NA,           
                      lab_id = NA,           
                      lab_sample_number = NA,
                      analysis_date = NA,     
                      imported  = NA,         
                      sheet_id  = OUTsub2$sheet_id            
)
wq_dat1$ID <- paste( "2", wq_dat1$location, wq_dat1$station, wq_dat1$sample_date, 
                       formatC(wq_dat1$sample_time, flag = "0", width =4, digits=4),
                       formatC(wq_dat1$sample_depth, flag = "0", width =3, digits=3), sep = "" )


ii <- nchar(wq_dat1$ID)==25  ## corrupt date
wq_dat1 <- wq_dat1[!ii, ]
###

all <- rbind(all, wq_dat1)

setwd(homeDir)













