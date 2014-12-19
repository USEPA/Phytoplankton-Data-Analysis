# STORET DATA
# BRING IN DATA AND DO A BIT OF PROCESSING------------------------------------
storet <- read.delim("originalData/algae/EFR Phytoplankton Data/storet/Data_JJB_20140512_091843_RegResults.txt",
                     sep="\t", na.strings="", as.is=TRUE)
str(storet)





####

#unique(algae$lake)[ unique(algae$lake) %in% unique(storet$lake)]
#unique(algae$lake)[ unique(algae$lake) %in% as.character(unique(wqOld$lake) ) ]


storet$yr <- format(storet$Date, "%Y")
table(storet$yr, storet$lake)

write.table(storet, "processed_data/storet.060514.csv", sep = ",",row.names=FALSE)

# WORK ON UNIQUE IDENTIFIERS AND SCREEN FOR NON LD LAKE SITES---------------------
# Basic info
  unique(storet$Activity.Medium)  # All water samples
  unique(storet$Sample.Fraction)  # Total, Dissolved, NA
  
# District 2 information
  district2 <- read.xls("originalData/algae/FY13WQSampleCollectionSiteLocations.xlsx", 
                        as.is=TRUE)  # Read in established sites
  district2$id <- district2$Buckhorn  # Format
  str(district2)
  district2 <- district2[, "id"]  # Format
  district2 <- district2[!(district2 %in% "ID")]  # Eliminate "ID" form lake.site name
  district2 <- district2[!grepl(pattern=":", x=district2)]  # Eliminate lake names
  district2 <- data.frame(lake.station = substr(district2,start=2, stop=length(nchar(district2))),
                          lake = substr(district2, 2, 4),
                          district = as.integer(substr(district2, 1,1)))
  district2[,c("lake.station", "lake")] <- apply(X=district2[,c("lake.station", "lake")], 
                                                 MARGIN=2, FUN="as.character")
  
# Lakes
  storet <- storet[storet$State != "ILLINOIS",]  # Exclude reservoirs in Illinois
  storet$lake <- substr(storet$Station.ID,2,4)  # Create lake vector
  unique(storet$lake)  # OR stand for Ohio River and can be eliminated
  storet <- storet[!grepl("OR", storet$lake), ]  # Eliminate Ohio River data
  unique(district2$lake) %in% unique(storet$lake)  # All LD lakes represented
  notInLD <- unique(storet$lake) %in% unique(district2$lake)  # 3 lakes not in LD
  unique(storet$lake)[!notInLD]  #TAC = Ohio River, SPC =1 record no lat long, BBC = Ohio River
  storet <- storet[!(storet$lake %in% unique(storet$lake)[!notInLD]), ]  # Remove 3 lakes above
  length(unique(storet$lake))  # 20 lakes, all in district 2, in final data set

# Stations
  unique(storet$Station.ID)
  # Remove stations that represent inflow or outflow
  # Stations begining with "1" represent in or outflows
    storet <- storet[substr(storet$Station.ID, 5, 5) != 1, ]
  # TRUE if lake.station in storet is also a routine site in District 2
    station.index <- substr(storet$Station.ID, 2, nchar(storet$Station.ID)) %in% 
      district2$lake.station 
  # 157 lake.station combinations not included in District 2 routine sampling
    unique(storet[!station.index, "Station.ID"])[order(unique(storet[!station.index, "Station.ID"]))]  # Assume these are real stations

# Date and Time
  storet$rdate <- as.Date(substr(storet$Activity.Start, 1, 10), format = "%Y-%m-%d")
  table(storet$rdate, useNA="ifany")  # looks good
  storet$time <- substr(x=storet$Activity.Start,  12, 16)
  table(storet$time, useNA="ifany")  # looks good
  storet$year <- format(storet$rdate, "%Y")

# Depth
  storet$depth_ft <- storet$Activity.Depth
  table(storet$depth_ft, useNA="ifany")  # Looks good, but need to add leading 0's for ID
  depthWithZeros <- with(storet,
                         ifelse(nchar(depth_ft) == 1,
                                      paste("00", depth_ft, sep=""),
                         ifelse(nchar(depth_ft) == 2,
                                      paste("0", depth_ft, sep=""),
                                      depth_ft)))

# Construct ID
  storet$ID <- with(storet,
                    paste(Station.ID, 
                          format(rdate, "%Y%m%d"), 
                          gsub(":", "", time),  # strip colon from time
                          depthWithZeros,
                          sep = ""))  # add depth with leading zeroes
  table(nchar(storet$ID))  # Good, mostly 24.  23 can be attributed to unusual, but correct, site names.

# CLEAN UP MEASUREMENT VALUES AND ADDRESS DETECTION LIMIT ISSUES--------------
# Result.Value.as.Text
# Clean up character values and replace "Not Detected" with dl for analytes of interest.
  storet$Result.Value.as.Text <- gsub("^\\s+|\\s+$", "", storet$Result.Value.as.Text)  # strip leading and trailing spaces
  storet$Result.Value.as.Text <- gsub(",", "", storet$Result.Value.as.Text)  # strip any commas
  foo <- is.na(as.numeric(storet$Result.Value.as.Text))  # Index for non-numeric values
  unique(storet$Result.Value.as.Text[foo])  # All non-numeric values.  Not Detected and 0..1
  storet[storet$Result.Value.as.Text == "0..1", "Result.Value.as.Text"] = "0.1"
  sum(foo)  # 9592 instances of "Not Detected".  Should provide dl for analytes I care about

# List of analytes I care about
  analyte.list <- c("Ammonia-nitrogen", "Copper", "Inorganic nitrogen (nitrate and nitrite)",
                    "Iron", "Kjeldahl nitrogen", "Manganese", "Molybdenum", "Nitrogen", 
                    "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)",
                    "Phosphorus", "Silica", "Sodium", "Total solids", "Total suspended solids")

# First, define the "Units" as mg/l for all instances of "Not Detected".
  for (i in 1:length(analyte.list)){
    storet[storet$Characteristic.Name == analyte.list[i] &  # For each analyte I care about
             storet$Result.Value.as.Text == "Not Detected",  # Find censored observations
           "Units"] = "mg/l"  # Change Units from NA to "mg/l"
  }

# Second, create "qual1" field for "<" or ">" for censored observations.  
# Adopted this nomenclature from water chem file Matt pulled together.
  storet$qual1 <- ifelse(storet$Characteristic.Name %in% analyte.list &
                           storet$Result.Value.as.Text == "Not Detected",
                         "<", NA)  # All censored values are below detection limit

# Now, replace "Not Detected" with detection limit for all analytes of interest.
{
  # Any Ammonia-nitrogen "Not Detected"
  storet[storet$Characteristic.Name == "Ammonia-nitrogen" & foo, # APHA~4500-NH3(D) mdl=0.03mg/L
         c("Result.Value.as.Text", "Analytical.Proc.ID")]
  storet[storet$Characteristic.Name == "Ammonia-nitrogen" &  # Replace "Not Detected" with 0.03
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.03"
  
  
  # Any Copper "Not Detected"
  storet[storet$Characteristic.Name == "Copper" & foo, # USEPA~200.8 mdl=0.004mg/L
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  
  storet[storet$Characteristic.Name == "Copper" &  # Replace "Not Detected" with 0.004
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.004"
  
  
  # Any Inorganic nitrogen (nitrate and nitrite) "Not Detected"
  storet[storet$Characteristic.Name == "Inorganic nitrogen (nitrate and nitrite)" & foo, 
         c("Result.Value.as.Text", "Analytical.Proc.ID")] 
  # APHA~4500-NO3(H) mdl=0.01mg/L; USEPA~300(A) mdl=0.01; 
  storet[storet$Characteristic.Name == "Inorganic nitrogen (nitrate and nitrite)" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.01"
  
  
  # Any Iron "Not Detected"
  storet[storet$Characteristic.Name == "Iron" & foo, 
         c("Result.Value.as.Text", "Analytical.Proc.ID")] 
  # Both referenced methods are for ICP-MS, but neither method actually addresses Iron.
  # Using Fe mdl reported for ICP in APHA
  # AUSCOEKY~SW6020A reported as ICP-MS mdl=0.013mg/L; USEPA~200.8 mdl=0.013mg/L; 
  storet[storet$Characteristic.Name == "Iron" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.013"
  
  
  # Any Kjeldahl nitrogen "Not Detected"
  storet[storet$Characteristic.Name == "Kjeldahl nitrogen" & foo, # USEPA~351.2 mdl=0.1
         c("Result.Value.as.Text", "Analytical.Proc.ID")]
  storet[storet$Characteristic.Name == "Kjeldahl nitrogen" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.1"
  
  
  # Any Manganese "Not Detected"
  storet[storet$Characteristic.Name == "Manganese" & foo, # USEPA~200.8 mdl=0.007mg/L
         c("Result.Value.as.Text", "Analytical.Proc.ID")]
  storet[storet$Characteristic.Name == "Manganese" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.007"
  
  
  # Any Molybdenum "Not Detected"
  storet[storet$Characteristic.Name == "Molybdenum" & foo, # None
         c("Result.Value.as.Text", "Analytical.Proc.ID")]
  # Any Nitrogen "Not Detected"
  storet[storet$Characteristic.Name == "Nitrogen" & foo, # None
         c("Result.Value.as.Text", "Analytical.Proc.ID")]
  
  # Any Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3) "Not Detected"
  storet[storet$Characteristic.Name == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  # USEPA 9056 mdl not reported.  Assume mdl=0.01mg/L
  storet[storet$Characteristic.Name == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.01"
  
  # Any Phosphorus "Not Detected"
  storet[storet$Characteristic.Name == "Phosphorus" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  # APHA~4500-P-F mdl=0.005mg/L
  storet[storet$Characteristic.Name == "Phosphorus" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.005"
  
  
  # Any Silica "Not Detected"
  storet[storet$Characteristic.Name == "Silica" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  # None
  # Any Sodium "Not Detected"
  storet[storet$Characteristic.Name == "Sodium" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  # None
  
  # Any Total solids "Not Detected"
  storet[storet$Characteristic.Name == "Total solids" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")]  
  # APHA~2540-B no mdl reported.  set = to 1/2 lowest reported value in dataset = 0.5
  storet[storet$Characteristic.Name == "Total solids" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = "0.5"
  
  # Any Total suspended solids "Not Detected"
  storet[storet$Characteristic.Name == "Total suspended solids" & foo,
         c("Result.Value.as.Text", "Analytical.Proc.ID")] 
  # APHA~2540-D no mdl reported.  set = to 1/2 lowest reported value in dataset
  storet[storet$Characteristic.Name == "Total suspended solids" &  
           storet$Result.Value.as.Text == "Not Detected",
         "Result.Value.as.Text"] = min(storet[storet$Characteristic.Name == "Total suspended solids",
                                              "Result.Value.as.Text"])
}

# Deal with units.  Convert most constituents to mg/l
{
  # First, see what units are included
  unique(storet$Units)
  
  # See why there are NA units
  unique(paste(storet[is.na(storet$Units), "Result.Value.as.Text"],  # Only occur when "Not Detected"
               storet[is.na(storet$Units),"Characteristic.Name"]))   # That is fine.
  
  # Inspect "% recovery"
  table(storet[storet$Units == "% recovery", "Characteristic.Name"])  # No analytes of interest
  # omit all % recovery
  storet <- storet[storet$Units != "% recovery", ]
  
  # Inspect "None"
  table(storet[storet$Units == "None", c("Characteristic.Name")])  # all from pH.  OK
  
  
  # Inspect "%"
  table(storet[storet$Units == "%", c("Characteristic.Name")])  # Most from DO, 1 from Total Solids
  storet <- storet[!(storet$Characteristic.Name == "Total solids" & storet$Units == "%"), ] # remove TS record
  
  # Inspect "in"
  table(storet[storet$Units == "in", c("Characteristic.Name")])  # All Secchi disk depth.  Convert to m
  
  # Inspect "ft"
  table(storet[storet$Units == "ft", c("Characteristic.Name")])  # "Depth, bottom" convert to m
  
  # Inspect "mg/m3", "mg/kg", "g/cm3", "ppb", "um3/l"
  table(storet[storet$Units %in% c("mg/m3", "ppb", "ug/kg", "mg/kg", "g/cm3", "um3/l"), c("Units", "Characteristic.Name")]) # chlorophyll. keep.
  # mg/m3 is chlorophyll. Retain units.  
  # ppb, ug/kg, and mg/kg are a variety of solutes. convert.
  # g/cm3 is "Density" and "Specific gravity".  Retain units.
  # um3/l is biovolume. retain units.
  
  # Inspect Alkalinity and associated units/method
  storet[grepl(pattern="Alkalinity",x=storet$Characteristic.Name),
         c("Characteristic.Name", "Units", "Analytical.Proc.ID")]
  # "Alkalinity, total" via EPA~310.2 is reported as mg/l, but according to method
  # should be mg/l CaCO3.  Change.
  
  # Inspect Depth
  storet[!is.na(storet$Units) & storet$Characteristic.Name == "Depth", 
         c("Result.Value.as.Text", "Units", "Analytical.Proc.ID")]
  # Something isn't right here. Neither units nor characteristic.Name make sense
  # for a Depth measurement.  Omit.  Sample depth is supplied in "Activity.Depth"
  storet <- storet[storet$Characteristic.Name != "Depth", ]
  
}  

# Define desired units.  Convert most chemicals to mg/l and distances to m.
  storet$Units2 <- ifelse(storet$Units %in% c("ug/l", "ppb", "ug/kg", "mg/kg"),
                          "mg/l",
                   ifelse(storet$Units %in% c("in", "ft"), 
                          "m",
                   ifelse(storet$Analytical.Proc.ID == "USEPA~310.2" |
                                          storet$Analytical.Proc.ID == "APHA~2320",
                           "mg/l CaCO3",  # alkalinity initially reported as mg/l
                            storet$Units)))

# Convert original Result.Value.as.Text to new units when necessary.
# Convert values to numeric first.  We already dealt with all text (i.e. "Not Detected") for
# the variables we care about. So this conversion is ok.
  storet$Result.Value <- as.numeric(storet$Result.Value.as.Text) # "Not detected converted to NA.  No problem.  Handled above.

storet$Result.Value.2 <- with(storet,
                              ifelse(Units %in% c("ug/l", "ppb", "ug/kg"), 
                                     Result.Value/1000,  # Divide by 1000 to get mg/l
                                     ifelse( Units == "in", 
                                             Result.Value * (2.54/100),  # in to meters
                                             ifelse(Units == "ft",
                                                    Result.Value/3.28, # foot to meters
                                                    Result.Value))))  # if none of the above, don't change


# Analyte names
{
  # Pull out all unique combination of "Characteristic.Name" and "Analytical.Proc.ID"
  # Write to disk for further inspection
  name.method <- data.frame(stringsAsFactors=FALSE,
                            combined = 
                              unique(paste(storet$Characteristic.Name, 
                                           storet$Analytical.Proc.ID, 
                                           sep = "#")))  # Odd separator used to split on below
  name.method$Characteristic.Name <- sapply(strsplit(name.method$combined, split="#"), "[[", 1)
  name.method$Analytical.Proc.ID <- sapply(strsplit(name.method$combined, split="#"), "[[", 2)
  write.table(name.method, file="processed_data/storetNameMethod.txt",
              row.names=FALSE)
  
  # Based on inspection of file (see above), the following naming conventions will be used:
  storet$Characteristic.Name.2 <- with(storet,
                                       ifelse(Characteristic.Name == "Ammonia-nitrogen",
                                              "NH4-N",
                                              ifelse(Characteristic.Name == "Inorganic nitrogen (nitrate and nitrite)",
                                                     "NO2.3",
                                                     ifelse(Characteristic.Name == "Nitrogen",
                                                            "Kjeldahl nitrogen-N",
                                                            ifelse(Characteristic.Name == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)",
                                                                   "NO2.3",
                                                                   ifelse(Characteristic.Name == "Phosphorus",
                                                                          "reactive.phosphorus",
                                                                          ifelse(Characteristic.Name == "Total solids",
                                                                                 "Total suspended solids",
                                                                                 ifelse(Characteristic.Name == "Alkalinity, total",
                                                                                        "Alkalinity, Total", 
                                                                                        Characteristic.Name))))))))
  
  
  # The data contain 163 reports of "Total solids" referencing APHA~2540-C.  The specified method
  # is for "total dissolved solids", however.  No way to reconcile this, so I will omit the data.
  storet <- storet[with(storet, !(Characteristic.Name == "Total solids" & 
                                    Analytical.Proc.ID == "APHA~2540-C")), ]
  
  # Inspect Magnesium and APHA~2340B
  # Some Magnesium data reference APHA~2340B, which is a hardness method, and use mg/l CaCO3
  # This isn't correct.  Remove these 20 observations.
  storet <- storet[with(storet, Analytical.Proc.ID != "APHA~2340B" & Characteristic.Name != "Magnesium"), ]
  
  # Need to specify total or dissolved fraction
  # Investigate instances where Sample.Fraction == NA
  table(storet[is.na(storet$Sample.Fraction), "Characteristic.Name.2"])
  # For many of these analytes it doesn't make sense to report a Sample.Fraction
  # Specify below
  no.sample.fraction <- c("Density", "Depth", "Depth, bottom", "Depth, Secchi disk depth",
                          "Dissolved oxygen (DO)", "Dissolved oxygen saturation", "pH",
                          "Phytoplankton biovolume", "Specific conductance", "Specific gravity",
                          "Temperature, water", "Total dissolved solids", "Total suspended solids",
                          "Turbidity", "Alkalinity, Total")
  # Paste Sample.Fraction after Characteristic.Name.2, unless it doesn't make sense
  # to report a Sample.Fraction
  storet$Characteristic.Name.2 <- ifelse(storet$Characteristic.Name.2 %in% no.sample.fraction,
                                         storet$Characteristic.Name.2,
                                         ifelse(storet$Characteristic.Name.2 == "Orthophosphate",
                                                "Orthophosphate, Dissolved",
                                                paste(storet$Characteristic.Name.2, 
                                                      storet$Sample.Fraction, sep=", ")))
  
  # Dissolved or Total silica
  # 251 observations where Sample.Fraction is reported as NA from two different methods 
  # Not sure what to do with these.
  table(storet[storet$Characteristic.Name.2 == "Silica, NA", 
               c("Analytical.Proc.ID", "Characteristic.Name.2")])
  # 13 observations where Sample.Fraction is reported as Total
  table(storet[storet$Characteristic.Name.2 == "Silica, Total", 
               c("Analytical.Proc.ID", "Characteristic.Name.2")])  
  
  ##############DEAL WITH SILICA ABOVE AND CONVERT N SPECIES TO UNITS OF N.
  with(storet, unique(paste(Characteristic.Name.2, Units2)))[order(with(storet, unique(paste(Characteristic.Name.2, Units2))))]
}


### crashes
### xxx <- dcast(storet, Station.ID + Date ~ Characteristic.Name )
library(data.table)

DT <- data.table(storet)
setkey(DT,"Station.ID", "Date")


sss=system.time(
  ss <- dcast.data.table( DT, Station.ID + Date ~ Characteristic.Name, length, value.var = "result")
  
  ); sss

