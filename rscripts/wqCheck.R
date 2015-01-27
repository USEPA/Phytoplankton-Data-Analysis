#### Script to clean water quality file.
## Mostly, this involves removing non-uniques and make sure the site names 
## are formatted in a consistent manner. Fixing dates in the data is also
## an issue.

library(plyr)
library(stringr)
library(XLConnect)

chem <- read.delim(file="processed_data/combined_wq_20150126.txt", sep = "\t", header = TRUE, fill = TRUE, as.is = TRUE, comment.char="")

## Fix date issues
table(chem$sample_date)
all(nchar(chem$sample_date)==8)
## Turns out that unlist() will kill the Date class of the objects in a list.
## Use do.call() instead.
sampleDate <- do.call("c",lapply(chem$sample_date, FUN = function(x){
  flg <- FALSE
  if(nchar(x)==8 & substr(x,1,2) %in% c(19,20)){
    flg <- TRUE
    yr <- substr(x,1,4)
    mth <- substr(x,5,6)
    dy <- substr(x,7,8)
    out <- as.Date(paste(yr,mth,dy,sep="-"),format="%Y-%m-%d")
  }
  if(substr(x,1,1)=="9" & nchar(x)==7){
    flg <- TRUE
    yr <- substr(x,4,7)
    mth <- "09"
    dy <- substr(x,2,3)
    out <- as.Date(paste(yr,mth,dy,sep="-"),format="%Y-%m-%d")
  }
  if(substr(x,1,2)=="10" & nchar(x)==8){
    flg <- TRUE
    yr <- substr(x,5,8)
    mth <- "10"
    dy <- substr(x,3,4)
    out <- as.Date(paste(yr,mth,dy,sep="-"),format="%Y-%m-%d")
  }
  if(!flg){
    out <- as.Date("2020-01-01",format="%Y-%m-%d")
  } 
  return(out)
}))

chem$sample_date_fixed <- sampleDate



########
#### Jake wrote a lot of code to format / QC the water quality data.
#### Implement that code here.


# Date
table(chem$sample_date_fixed, useNA="always")  # No 999 entries, but 1069 instances of 2020-01-01
chem$rdate <- as.Date(as.character(chem$sample_date_fixed), format = '%Y-%m-%d')

# Pull out observations that are not included in STORET data.
# Prior to 2000-04-03 and after 2012-08-30
time.index <- with(chem, (rdate < as.Date("2000-04-03") | rdate > as.Date("2012-08-30")) &
                     rdate != as.Date("2020-01-01"))
chem.sub <- chem[time.index, ]
str(chem.sub)  # 1212 observations    

# Exlcude stations in inflows and outflows.  
# Stations begining with "1" represent in or outflows
# Inspect station  
table(chem.sub$station, useNA="always")  # 36 w/out station
chem.sub[is.na(chem.sub$station), "ID"]  # station is identified in ID.
chem.sub[is.na(chem.sub$station), "station"] = substr(chem.sub[is.na(chem.sub$station), "ID"], start=5, stop=8)  # Assign values based on ID
chem.sub <- chem.sub[substr(chem.sub$station, 1, 1) != 1, ]
str(chem.sub)  # 659 observations

# Review dates in chem.sub
table(chem.sub$sample_date_fixed, useNA="always")  # All good.

# Time
table(chem.sub$sample_time)  # These all look good.  
# Add leading zero where needed
chem.sub$sample_time <- ifelse(nchar(chem.sub$sample_time) < 4,
                               paste(0, chem.sub$sample_time, sep=""),
                               chem.sub$sample_time)

# Inspect ID
unique(chem.sub$ID)  # Look good
table(nchar(chem.sub$ID), useNA="always")  # 36 IDs with 23 characters, 623 with 24 characters. 24 is expected
chem.sub[nchar(chem.sub$ID) == 23, "ID"]  # these are OK, all have 4 character station names. 

# Inspect lake
table(chem.sub$lake, useNA="always")  # 36 instances of NA
chem.sub[is.na(chem.sub$lake), "ID"]  # Lake is identified in ID.
chem.sub[is.na(chem.sub$lake), "lake"] = substr(chem.sub[is.na(chem.sub$lake), "ID"], start=2, stop=4)  # Assign values based on ID

# Inspect depth
table(chem.sub$sample_depth, useNA="always")  # Looks good.
names(chem.sub)[names(chem.sub) %in% "sample_depth"] = "depth.ft"  # Change name to be consistent with algae file

# Create District vector to be consistent with algae
chem.sub$district <- substr(chem.sub$ID, start=1, stop=1)
table(chem.sub$district, useNA="always")  # all good

# Take a quick look at reported values
chem.sub[, c("rdate", "ID", "analyte", "result_num", "result_num2", "sheet_id")]  # Looks OK
sum(is.na(chem.sub$result_num))  # No instances of result_num == NA

# CENSORED WATER CHEM DATA-------------------------------------------------------------
# Dual censored values: none
chem.sub[!is.na(chem.sub$qual1) & !is.na(chem.sub$qual2), 
         c('analyte', 'original', 'qual1', 'qual2', 'result_num2', 'result_num')
         ]  # No dual censored observations

# Other censoring?
table(chem.sub$qualifiers)  # A few.  "J", "U".  Don't know what these are
table(chem.sub$qual1)  # 68 "<"
with(chem.sub[chem.sub$qual1 == "<", ], unique(paste(qual1, result_num)))  # all "<" report a MDL.
# Assign final number as = to result_num, but carry "<" flag
table(chem.sub$qual2)  # none

# ND: when the data are reported as ND with a U qualifier, Matt set the value equal to -777.
chem.sub[chem.sub$qual1 == 'ND' & !is.na(chem.sub$qual1),  # None 
         c('ID', 'analyte', 'qualifiers', 'qual1', 'detect_limit', 'original', 'result_num')]

# Final value assignment
chem.sub$result_final <- chem.sub$result_num  #  Can ignore result_num2, which is upper censor

# TAKE A LOOK AT WATER CHEM ANALYTES---------------------------------------------------
length(unique(chem.sub$analyte))  # 16 analytes

# Want unique combinations of analyte, prep_method, test_method, and units
length(unique(with(chem.sub, paste(analyte, prep_method, test_method, units, sep = " "))))  # 16!

# Pull out all unique combination of "Analyte", "prep_method", "test_method", and "units"
# Write to disk for further inspection
name.method <- data.frame(stringsAsFactors=FALSE,
                          combined = 
                            unique(paste(chem.sub$analyte, 
                                         chem.sub$prep_method,
                                         chem.sub$test_method,
                                         chem.sub$units,
                                         sep = "#")))  # Odd separator used to split on below
name.method$analyte <- sapply(strsplit(name.method$combined, split="#"), "[[", 1)
name.method$prep_method <- sapply(strsplit(name.method$combined, split="#"), "[[", 2)
name.method$test_method <- sapply(strsplit(name.method$combined, split="#"), "[[", 3)
name.method$units <- sapply(strsplit(name.method$combined, split="#"), "[[", 4)

write.table(name.method, file="processed_data/cleanedChemNameMethod.txt",
            row.names=FALSE)

# I inspected the file above in Excell and created a new field for updated analyte names, where applicable.
# New analyte names were chosen to be consistent with those used in the storet file.  The general format is
# "analyte, sample.fraction" where sample.fraction is total, dissolved, or NA.  New file was renamed 
# "processed_data/cleanedChemNameMethodCorrected.txt".  Below, I read in this file and use
# the information to update the analyte names in chem.sub.  Note that cleanedChemNameMethodCorrected.txt
# contains many more analytes than cleanedChemNameMethod.txt.  This is a holdover from a previous version
# where cleanedChemNameMethod.txt was written before the inflow and outflow stations were stripped.

chem.sub.analyte.names <- read.table("processed_data/cleanedChemNameMethodCorrected.txt", 
                                     header=TRUE, sep="\t",as.is=TRUE, na.string = "", comment.char="")

# match returns a vector of the positions of matches of its first argument (uncorrected analyte name in chem.sub)
# in its second argument (uncorrected analyte name in .txt file)
names.index <- match(chem.sub$analyte, chem.sub.analyte.names$analyte)  
# Create new vector for corrected analyte name in chem.sub.  
chem.sub$analyte.2 <- ifelse(is.na(chem.sub.analyte.names$analyte.2[names.index]),  # If it doesn't need to be corrected
                             chem.sub$analyte,  # then use original name
                             chem.sub.analyte.names$analyte.2[names.index])  # else use new name from .txt file
# inspect output
unique(chem.sub[, c("analyte", "analyte.2", "units")])  # looks good!

# DEAL WITH UNITS----------------------------------------
# First, strip trailing spaces from units
chem.sub$units <- gsub("^\\s+|\\s+$", "", chem.sub$units)  # strip leading and trailing spaces

# Change names as necessary
chem.sub$units2 <- with(chem.sub,
                        ifelse(analyte.2 == "Alkalinity, Total",
                               "mg/l CaCO3",  # was reported as mg/L
                               ifelse(units == "mg/L" & analyte.2 != "Alkalinity, Total",  # Fixed alkalinity above
                                      "mg/l",  # lowercase l
                                      ifelse(units == "ug/L",
                                             "mg/l",
                                             ifelse(units == "units",  # reported units for pH
                                                    "none",  # units reported in storet
                                                    ifelse(units == "ppb",
                                                           "mg/l",
                                                           units))))))

# Convert values based on units
chem.sub$result_final <- with(chem.sub, 
                              ifelse((units == "ug/L" | units == "ppb") & units2 == "mg/l",
                                     result_final/1000,  # convert to mg/l
                                     result_final))  

# FINALIZE, SCREEN FOR NA AND DUPLICATES, AND WRITE DATAFRAME TO .csv FILE----------------------
# Extract columns of interest
chem.sub.selected.columns <- chem.sub[, c("ID", "district", "lake", "station", "depth.ft", "rdate", 
                                          "sample_time", "analyte.2", "units2", "qual1", "result_final")]
# Rename to be consistent with STORET and algae
chem.sub.selected.columns <- rename(chem.sub.selected.columns, 
                                    c(ID = "id", rdate = "date", sample_time = "time",
                                      analyte.2 = "analyte", units2 = "units",
                                      qual1 = "qual", result_final = "result"))
str(chem.sub.selected.columns)

# Screen for NA
# Any rows where all 11 columns are NA?
na.index <- rowSums(is.na(chem.sub.selected.columns)) == 11  # None

# Screen for dups
sum(duplicated(chem.sub.selected.columns))  # None

# Write .csv
write.table(chem.sub.selected.columns, 
            paste("processed_data/cleaned_wq_",format(Sys.time(), "%Y%m%d"), sep = ""), 
            sep = ",", row.names=FALSE)



#### End of Jake's QC code for water quality data.
########

