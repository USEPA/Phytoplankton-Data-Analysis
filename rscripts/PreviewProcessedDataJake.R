# PREVIEW PROCESSED DATA FROM MATT

# LIBRARIES-----------------------------------------------------
  library(ggplot2)
  library(reshape)
  library(reshape2)
  library(gdata)
  library(plyr)

# READ IN AND FORMAT algae.csv FROM processed_data FOLDER-------------------------
# Reading from processed_data folder
  algae <- read.delim("processed_data/cleaned_algae_20140717.txt", as.is=TRUE, header = TRUE)
  head(algae)
  str(algae)

# Review number of samples, lakes, dates, etc
  table(algae$date)  # Formatted correctly.
  table(algae$lake)  # Unusual lake names from District 3
  unique(algae$station)  # Still need to consult with Jade on a few of these
  unique(algae$depth_ft) # Good

# Add date
  algae$rdate <- as.Date(as.character(algae$date), format = "%Y%m%d")
  algae$year <- as.numeric(substr(algae$date, 1,4))

# Add district
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
  district2[,c("lake.station", "lake")] <- apply(X=district2[,c("lake.station", "lake")], MARGIN=2, FUN="as.character")
  district3 <-  read.xls("originalData/algae/FY13WQSampleCollectionSiteLocations.xlsx", 
                                       sheet="district 3", as.is=TRUE)  # Read in district3 lakes 
  algae$district <- ifelse(algae$lake %in% district2$lake, 2,
                           ifelse(algae$lake %in% district3$lake, 3, NA))
  algae <- algae[algae$district == 2,]  # Remove district 3

# Are HAB monitoring data coded correctly?
  algae[!is.na(algae$cell_per_l) & is.na(algae$BV.um3.L), c("hab")]  # Should all be TRUE

# Analysis of anamolous site names
  algae$lake.station <- paste(algae$lake, algae$station, sep="")  # Create new id
  anamolous <- algae[!(algae$lake.station %in% district2$lake.station), 
                     c("date", "lake.station", "hab", "sheet_id")]  # extract lake.station combo not in official list
  anamolous <- anamolous[!(substr(x=anamolous$lake.station, start=1, stop=3) %in% district3$lake),]  # remove district3
  anamolous <- anamolous[!(duplicated(anamolous$lake.station)), 
                         c("lake.station", "date", "hab", "sheet_id") ]  # Remove duplicated records
  sheetID <- read.csv("processed_data/summaryStatus_20140716.csv", as.is = TRUE) 
  anamolous <- merge(anamolous, sheetID[, c("sheet_id", "file")])
  anamolous.algae <- anamolous[order(anamolous$lake.station),]  # Order
  write.table(anamolous.algae, file="output/anamolousNamesAlgae.txt", row.name=FALSE)  

# Look at values of algal density and biovolume
  summary(algae$cell_per_l)
  algae[algae$cell_per_l < 0, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$cell_per_l == 9999, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$BV.um3.l  < 0, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$cell_per_l == 9999, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]


# Strip leading and trailing spaces in taxa  
  algae$taxa <- gsub("^\\s+|\\s+$", "", algae$taxa)
  unique(algae$taxa)[order(unique(algae$taxa))][1:700] # Good
  unique(algae$class)  # Good
  unique(algae$hab)  # Good
  unique(algae$qual_replicate)  # Looks good, NA, Q, or R
  # All observations should include a taxa name
  length(algae[is.na(algae$taxa), c("taxa", "ID", "sheet_id", "cell_per_l", "BV.um3.L")][,1])

# SUMMARY TABLES FOR PRESENTATIONS------------------
  date.yr.lk <- aggregate(algae$rdate, by=list(lake=algae$lake, year=algae$year), FUN=function(X1) {length(unique(X1))})
  date.yr.lk <- dcast(date.yr.lk, lake ~ year, value.var="x")
  date.yr.lk$total <- apply(subset(date.yr.lk,  select = -c(lake)), MARGIN=1, FUN=sum, na.rm=T)  # Calculate total per lake
  year.range <- min(as.numeric(names(date.yr.lk)), na.rm=T):2014  # Define range of years included in data
  missing.years <- year.range[!(year.range %in% as.numeric(names(date.yr.lk)))]  # Define missing years
  date.yr.lk[,as.character(missing.years)] <- NA  # Add columns for missing years
  date.yr.lk <- date.yr.lk[, c("lake", as.character(sort(as.numeric(names(date.yr.lk)))), "total")]  # reorder columns
  date.yr.lk <- date.yr.lk[with(date.yr.lk, order(lake)), ]  # reorder rows
  date.yr.lk[is.na(date.yr.lk)] = 0  # If not samples were collected, set equal to 0
  write.table(date.yr.lk, file="processed_data/algaeObservationsSummary.txt")
  date.yr.lk.melt <- melt(date.yr.lk)
  # Summary plot of # of sampling dates per year
    ggplot(date.yr.lk.melt[with(date.yr.lk.melt, variable != "total"), ], aes(value)) +
    geom_histogram(binwidth = 0.2) +
    xlab("Number of sampling dates per year by lake")
  # Summary plot of # of sampling dates per lake
    date.yr.lk$lake <- factor(date.yr.lk$lake, date.yr.lk[order(date.yr.lk$total, decreasing=T), "lake"])  # Needed to order bars
    ggplot(date.yr.lk, aes(lake, total)) + geom_bar() +
    ylab("Total number of sampling dates per lake") 
  # Summary plot of sites per lake
    sites.lake <- aggregate(algae$lake.station, by=list(lake=algae$lake, year=algae$rdate), FUN=function(X1) {length(unique(X1))})
    ggplot(sites.lake, aes(x)) + 
      geom_histogram(binwidth = 0.5) +
      xlab("Number of sampling sites per lake per sampling date")
  # Which observations are hab = TRUE
    unique(algae[algae$hab == TRUE, c("lake", "date")])

# SUMMARY STATS FOR TAXONOMY CONTRACT-------------------
  #1.  How many samples were taken over the sampling period?
  length(unique(algae[algae$district == 2, "ID"]))  #8200, should be number of unique samples

  #2.  How many data points are in the historic data set?
  length(algae[!is.na(algae$taxa), "ID"])  # Exlude observations with no Taxa. 236,993
  #How many in Missing Biovolume List?
  miss.biov <- algae[is.na(algae$BV.um3.L) & !is.na(algae$cell_per_l),  # Extract taxa per lake x date. Doesn't account for depth x station
                     c("lake", "rdate", "taxa")]
  miss.biov <- miss.biov[with(miss.biov, order(lake, rdate)), ]  # Reorder
  u.miss.biov <- unique(miss.biov)  # Pull out unique taxa per lake x date.  Doesn't account for depth x station
  length(u.miss.biov[,1])

# POPULATE 'group' FIELD----------------------------------------
  unique(algae$class)  # Not filled out yet
  algae <- subset(algae, select = -c(class))  # Remove class field.  Merge it in below.
  # Lisa's algal classification data
    clas <- read.delim('originalData/algae/LouisvilleDistrictPhytoClassification.06052014.txt', 
                     header = TRUE,
                       sep="\t",
                       comment.char="",
                     na.strings=c('', 'NA'),
                     as.is = TRUE)  
    str(clas)
    clas <- clas[,c('Taxa', 'Group')]  # Remove notes columns
    names(clas) <- c('taxa', 'group')  # Change names per phytoplankton data precedent
    clas$taxa <- gsub("^\\s+|\\s+$", "", clas$taxa)  # Remove white spaces
    clas$group <- gsub("^\\s+|\\s+$", "", clas$group)  # Remove white spaces
  # Merge clas with phytoplankton data  
    ld.algae <- merge(algae, clas, by = 'taxa', all=T)  # 'ld' for Louisville District
    str(ld.algae)
    ld.algae$Bio.per.cell <- with(ld.algae, BV.um3.L / cell_per_l)
# Pull out algal taxa not included in class from Lisa
  no.taxa <- unique(ld.algae[!(ld.algae$taxa %in% clas$taxa), "taxa"])
  ld.algae[ld.algae$taxa %in% no.taxa, c("taxa", "sheet_id")]
  write.table(no.taxa, 
                file = paste("output/no.taxa.", Sys.Date(), ".txt", sep=""),
                row.names=FALSE)

# Investigate strangely formatted taxa names
  ld.algae[ld.algae$taxa %in% unique(ld.algae$taxa)[c(22,1851,1852,1856,1917,1919)], c("taxa", "sheet_id", 
                                                                                         "cell_per_l", "BV.um3.L")] 
# Taxa reported w/out sheet_id, cell_per_l, or BV.um3.L
  algae[with(algae, is.na(sheet_id) & is.na(cell_per_l) & is.na(BV.um3.L) & !is.na(taxa)),
           c("taxa", "sheet_id", "cell_per_l", "BV.um3.L")]
# Pull out algal taxa w/out a corresponding group ID from Lisa      
  no.group <- unique(ld.algae[is.na(ld.algae$group) & !is.na(ld.algae$taxa), 'taxa' ])  # Where group is NA, but taxa is known.  Unique to reduce redundancies.  Send to Lisa for updating.
  no.group[order(no.group)]  # Only a few, very god.
  write.table(no.class, 
              file = paste("output/no.class.", Sys.Date(), ".txt", sep=""), 
              row.names=F)
# Look at some taxa
  ld.algae[ld.algae$taxa == "Anabaena #112422", c("taxa", "sheet_id")]

# CONVERT BLUE-GREEN CELL COUNTS TO BIOVOLUME-----------------------
# First, identify all taxa that need biovolume (all HAB == TRUE records)
# Second, identify all examples where these taxa were found in routine monitoring
# and have biovolumes
  ld.algae[ld.algae$hab == TRUE & is.na(ld.algae$group), c("group", "taxa")] <- "Blue-green"  # Check for HAB w/out class field populated
  #ld.algae[19736, c("group")] <- "blue-green"  # Wont need this after issue #38 is resolved
  needBio <- unique(ld.algae[ld.algae$hab == TRUE, c("lake", "rdate", "taxa")])  # Pull out unique taxa per lake x date.  Doesn't account for depth x station
  needBio <- needBio[!apply(needBio, FUN=function(x) all(is.na(x)), MARGIN=1),]  # Eliminate rows with all NAs
  # Logical indicating which lakextaxa combinations in needBio are in the ld.algae where hab=F
  bioSourceIndex <- paste(ld.algae$lake, ld.algae$taxa, sep="") %in%  # lake x taxa vector from ld.algae
    paste(needBio$lake, needBio$taxa, sep="")  # lake x taxa vector from needBio
  bioSource <- ld.algae[bioSourceIndex & ld.algae$hab != TRUE & 
                          !is.na(ld.algae$taxa),]  # Data for taxa that need biovolume

# Now, see if any of the critters that need biovolume were not included in the monitoring
# data.
  notInld.algae.index <- paste(needBio$lake, needBio$taxa, sep="") %in% paste(ld.algae[ld.algae$hab != TRUE, "lake"],
                                                       ld.algae[ld.algae$hab != TRUE, "taxa"], sep="")
  # should all = TRUE
    notInld.algae.index
    notInld.algae <- needBio[!notInld.algae.index,]  # Which ones are missing
    unique(with(notInld.algae, paste(lake, rdate, taxa, sep = " ")))
    # Need to come back and re-run this using "Suggested" names.

# Revisit after issues #26, #27, and #3 have been resolved.
# Loop to plot the measured per cell biovolumes for each lake x taxa combination
# included in the HAB data set.
for(j in 1:length(unique(bioSource$lake))) {
  lake.j <- unique(bioSource$lake)[j]
  bioSource.j <- bioSource[bioSource$lake == lake.j,]
  needBio.j <- needBio[needBio$lake == lake.j,]
  for (i in 1:length(unique(bioSource.j$taxa))) { 
    taxa.i <- unique(bioSource.j$taxa)[i]
    pdf(file = paste("output/Figures/", lake.j, ".", taxa.i, ".pdf", sep=""))
    try(print(
      ggplot(bioSource.j[bioSource.j$taxa == taxa.i,], aes(rdate, Bio.per.cell)) + 
        geom_point() + 
        ylab("Biovolume per cell (um3)") +
        ggtitle(bquote(atop(paste(.(lake.j), " ", .(taxa.i), sep=""), 
                            "Red points indicate biovolume is needed"))) +
        geom_point(data=needBio.j[needBio.j$taxa == taxa.i, ], 
                   aes(rdate, 0),
                   color="red")
    ),
        silent=TRUE
    )
    dev.off()
  }
}

# A FEW VERY BASIC FIGURES-----------------------------------------
  # EFR
    ggplot(algae[algae$lake == 'EFR',], aes(rdate, cell_per_l)) + 
      geom_point(aes(color=hab))
  # All data
    ggplot(algae, aes(rdate, cell_per_l)) + geom_point(aes(color=lake))

# AGGREGATE AND CALCULATE DERIVED QUANTITIES-----------------------
  # Algal counts first
    # Calculate total cells.L per lake, station, depth, and date
      t.cells <- with(ld.algae, aggregate(cell_per_l ~ lake + station + depth_ft + rdate + hab, FUN = sum))  
      colnames(t.cells)[which(colnames(t.cells) == 'cell_per_l')] = 't.cell_per_l'  # Rename
    # Calculate Blue-Green cells_per_l per lake, station, depth, and date           
      bg.cells <- with(ld.algae[ld.algae$group == 'blue-green', ],
                       aggregate(cell_per_l ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(bg.cells)[which(colnames(bg.cells) == 'cell_per_l')] = 'bg.cell_per_l'   # Rename

  # Algal biovolumes
    # Calculate total biovolume per lake, station, depth, and date
      t.biov <- with(ld.algae, aggregate(BV.um3.L ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(t.biov)[which(colnames(t.biov) == 'BV.um3.L')] = 't.BV.um3.L'  # Rename
    # Calculate Blue-Green biovolume per lake, station, depth, and date           
      bg.biov <- with(ld.algae[ld.algae$group == 'blue-green', ],
                    aggregate(BV.um3.L ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(bg.biov)[which(colnames(bg.biov) == 'BV.um3.L')] = 'bg.BV.um3.L'  # Rename  

  # Merge aggregated data
    ld.algae.list <- list(t.cells, bg.cells, t.biov, bg.biov)
    ld.algae.agg <- merge_recurse(ld.algae.list)
    str(ld.algae.agg)

  # Calculate Prop BG by biovolume  
    ld.algae.agg$prop.bg.BV <- with(ld.algae.agg, bg.BV.um3.L / t.BV.um3.L)             
    ld.algae.agg[ld.algae.agg$hab == TRUE, 'prop.bg.BV'] = NA  # This code eliminates any data from HAB monitoring where only BG where quantified.  Is NA anyway, since HAB response data have no biovolume, just in case
  
  # Calculate Prop BG by Cells counts  
    ld.algae.agg$prop.bg.cell <- with(ld.algae.agg, bg.cell_per_l / t.cell_per_l)                 
    ld.algae.agg[ld.algae.agg$hab == TRUE, 'prop.bg.cell'] = NA   # This code eliminates any data from HAB monitoring where only BG where quantified. 

  # Reinforce order
    ld.algae.agg <- ld.algae.agg[with(ld.algae.agg, order(rdate, station, depth_ft)), ]

# QUICK GGPLOT FIGURES OF ALGAE-------------------------------------
  # Formatting data for x-axis
    x_breaks <- seq(as.Date('1986/1/1'), as.Date('2014/1/1'), by = '2 year')
    x_labels <- as.character(x_breaks, format='%Y')

  # Biovolume plots
  # Total Biovolume
      ggplot(ld.algae.agg, aes(rdate, t.BV.um3.L)) + geom_point() + ylab(expression(paste('Total Biovolume ( ', mu, m^3, '/L)'))) + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    # BG Biovolume      
      ggplot(ld.algae.agg, aes(rdate, bg.BV.um3.L)) + geom_point() + ylab(expression(paste('BG Biovolume ( ', mu, m^3, '/L)'))) + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    # Proportion BG biovolume
      ggplot(ld.algae.agg, aes(rdate, prop.bg.BV)) + geom_point() + ylab('Proportion BG Biovolume') + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    # Biovolume by site.  Define name factor for plotting
      mean.bg.bv.site <- ddply(ld.algae.agg, .(lake), summarize, 
                               mean = mean(bg.BV.um3.L, na.rm=TRUE))
      ld.algae.agg$flake <- factor(ld.algae.agg$lake, 
                                   levels=mean.bg.bv.site[order(mean.bg.bv.site$mean, decreasing=TRUE), "lake"],
                                   ordered=TRUE)
      ggplot(ld.algae.agg, aes(flake, bg.BV.um3.L)) + geom_boxplot() +
        scale_y_log10() +
        ylab("Blue-Green biovolume (um3/L") +
        theme(axis.title.x = element_blank())
    
  # Cells per L plots                  
  # Total cells
    ggplot(ld.algae.agg, aes(rdate, t.cell_per_l)) + geom_point() + ylab('Total Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) 
  # BG cells  
    ggplot(ld.algae.agg, aes(rdate, bg.cell_per_l)) + geom_point() + ylab(expression(paste('Blue Green Cell count (cells ', L^-1, ')'))) + 
      scale_x_date(breaks=x_breaks, labels = x_labels) 
  
  # Proportion BG cells  
    ggplot(ld.algae.agg[ld.algae.agg$lake == "EFR", ], aes(rdate, prop.bg.cell)) + geom_point() + ylab('Proportion Blue Green by Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)

# PREVIEW WATER CHEM DATA THAT MATT IMPORTED AND JADE/DREW PROVIDED------------------------------------------
# Reading from output/processed_data folder
  chem <- read.table("processed_data/cleaned_wq_20141121.txt", sep = "\t", 
                     header = TRUE, fill=TRUE, comment.char="",
                      as.is = TRUE)
  head(chem)
  str(chem)

# 9999 or 99999999 are used for time and date when data are not provided.  Inspect occurence.
  table(chem$sample_date_fixed, useNA="always")  # No 999 entries, but 1069 instances of 2020-01-01
  chem$rdate <- as.Date(as.character(chem$sample_date_fixed), format = '%Y-%m-%d')
  table(chem[chem$rdate == as.Date("2020-01-01"), "sheet_id"])  # Sheets 26, 1303, 1440
  # Dates in sheets 26 and 1303 are unambiguous.  Dates can be found in column B
  # of sheet 1440 as %y%m%d.
  sum(is.na(chem$sample_date_fixed))  # No missing values
  table(chem$sample_time)  # 74 occurences of 9999

# Pull out observations that are not included in STORET data.
# Prior to 2000-04-03 and after 2012-08-30
  time.index <- with(chem, (rdate < as.Date("2000-04-03") | rdate > as.Date("2012-08-30")) &
                       rdate != as.Date("2020-01-01"))
  chem.sub <- chem[time.index, ]
  str(chem.sub)  # 1212 observations              


# Inspect ID
  unique(chem.sub$ID)  # a space in some EFR.  These are missing sample collection depth, which is reported as 0.
  # These have all been read incorrectly from sheet 1303.  Issue #42 @ github.
  # Lets exclude those above and look at the rest.
  table(nchar(chem.sub[chem.sub$sheet_id != 1303, "ID"]))  # 36 IDs with 23 characters, 703 with 24 characters. 24 is expected
  chem.sub[chem.sub$sheet_id != 1303 & nchar(chem.sub$ID) == 23, "ID"]  # these are OK, all have 4 character station names. 

# Inspect lake
  table(chem.sub$lake, useNA="always")  # 495 instances of NA
  chem.sub[is.na(chem.sub$lake), "ID"]  # Lake is identified in ID.
  chem.sub[is.na(chem.sub$lake), "lake"] = substr(chem.sub[is.na(chem.sub$lake), "ID"], start=2, stop=4)  # Assign values based on ID

# Inspect station  
  table(chem.sub$station, useNA="always")  # 36 w/out station
  chem.sub[is.na(chem.sub$station), "ID"]  # station is identified in ID.
  chem.sub[is.na(chem.sub$station), "station"] = substr(chem.sub[is.na(chem.sub$station), "ID"], start=5, stop=8)  # Assign values based on ID
  
# Inspect depth
  table(chem.sub$sample_depth, useNA="always")  # Looks good.
  names(chem.sub)[names(chem.sub) %in% "sample_depth"] = "depth_ft"  # Change name to be consistent with algae file

# Create year vector to be consistent with algae file
  chem.sub$year <- format(chem.sub$rdate, "%Y")
  table(chem.sub$year, useNA="always")  # all good

# Create Disctric vector to be consistent with algae
  chem.sub$district <- substr(chem.sub$ID, start=1, stop=1)
  table(chem.sub$district, useNA="always")  # all good

# Create lake.station vector to be consistent with alage
  chem.sub$lake.station <- paste(chem.sub$lake, chem.sub$station, sep="")
  table(chem.sub$lake.station, useNA="always")  # good

# Take a quick look at reported values
  chem.sub[, c("rdate", "ID", "analyte", "result_num", "result_num2", "sheet_id")]  # Quite a lot of NA values
  chem.sub[is.na(chem.sub$result_num),  # All result_num == NA are from sheet 1303.  FIX THIS.
           c("rdate", "ID", "analyte", "result_num", "result_num2", "sheet_id")]
  chem.sub[chem.sub$sheet_id == 1303, "result_num"]

# CENSORED WATER CHEM DATA-------------------------------------------------------------
# Dual censored values: none
  chem.sub[!is.na(chem.sub$qual1) & !is.na(chem.sub$qual2), 
       c('analyte', 'original', 'qual1', 'qual2', 'result_num2', 'result_num')
       ]  # No dual censored observations
  
# Other censoring?
  table(chem.sub$qualifiers)  # A few.  "JJ", "UU".  Don't know what these are
  table(chem.sub$qual1)  # 80 "<"
  with(chem.sub[chem.sub$qual1 == "<", ], unique(paste(qual1, result_num)))  # all "<" report a MDL.
  # Assign final number as = to result_num, but carry "<" flag
  table(chem.sub$qual2)  # none

# ND: when the data are reported as ND with a U qualifier, Matt set the value equal to -777.
  chem.sub[chem.sub$qual1 == 'ND' & !is.na(chem.sub$qual1),  # None 
       c('ID', 'analyte', 'qualifiers', 'qual1', 'detect_limit', 'original', 'result_num')]

# Final value assignment
  chem.sub$result_final <- chem.sub$result_num  #  Can ignore result_num2, which is upper censor

# TAKE A LOOK AT WATER CHEM ANALYTES---------------------------------------------------
  length(unique(chem.sub$analyte))  # 63 analytes

# Want unique combinations of analyte, prep_method, test_method, and units
  length(unique(with(chem.sub, paste(analyte, prep_method, test_method, units, sep = " "))))  # 73!

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
# the information to update the analyte names in chem.sub.  
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
  unique(chem.sub[, c("analyte", "analyte.2")])  # looks good!

# Remove "Atrazine, Sediment (wet wt.)"
  chem.sub <- chem.sub[chem.sub$analyte.2 != "Atrazine, Sediment (wet wt.)", ]  # sediment variable

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

# Inspect duplicates
  dups.chem.sub <- chem.sub[duplicated(chem.sub[, c('ID', 'analyte.2')]),
                            c('ID', 'analyte.2')] # Have a handul of duplicate unique IDs.
# Find all instances of duplicated observations
  dups.chem.sub.all <- chem.sub[paste(chem.sub$ID, chem.sub$analyte.2) %in% paste(dups.chem.sub$ID, dups.chem.sub$analyte.2),
           c('ID', "analyte", 'analyte.2', "result_final", "sheet_id")]  # All NA
  dups.chem.sub.all[with(dups.chem.sub.all, order(ID, analyte.2)), ]  # these are OK.  Result of subtle differences in
  # measurement method that led to the use of different analyte names (i.e. Phosphourus-Total vs Phosphorus- Total Inorganic).
  # These were assigned an analyte.2 value of reactive.phosporus, Total
  


# Remove commas from analyte names, confuses ggplot
  unique(sub(pattern=',', replacement=".", x=chem.short$analyte))
  str(chem.short)

# Cast into wide format
  head(dcast(chem.short, ID ~ analyte, 
             value.var = "result_final"))  # Defaults to length,  Must be duplicates



  chem.short[chem.short$ID == "2EFR20004201108231100010" & chem.short$analyte ==  "Dissolved Organic Carbon",]


# STORET DATA----------------------
# See readStoret.R
  storet <- read.csv("processed_data/storet.060514.csv",
                       na.strings="NA", as.is=TRUE)
  str(storet)


# Duration & completeness of data set
  date.yr.lk.chem <- aggregate(storet$rdate, by=list(lake=storet$lake, year=storet$year), FUN=function(X1) {length(unique(X1))})
  date.yr.lk.chem <- dcast(date.yr.lk.chem, lake ~ year, value.var="x")  # Good
  date.yr.lk.chem$total <- apply(subset(date.yr.lk.chem,  select = -c(lake)), MARGIN=1, FUN=sum, na.rm=T)  # Calculate total per lake
  year.range <- min(as.numeric(names(date.yr.lk.chem)), na.rm=T):2014  # Define range of years included in data
  missing.years <- year.range[!(year.range %in% as.numeric(names(date.yr.lk.chem)))]  # Define missing years
  date.yr.lk.chem[,as.character(missing.years)] <- NA  # Add columns for missing years
  date.yr.lk.chem <- date.yr.lk.chem[, c("lake", as.character(sort(as.numeric(names(date.yr.lk.chem)))), "total")]  # reorder columns
  date.yr.lk.chem <- date.yr.lk.chem[with(date.yr.lk.chem, order(lake)), ]  # reorder rows
  date.yr.lk.chem[is.na(date.yr.lk.chem)] = 0  # If no samples were collected, set equal to 0
  write.table(date.yr.lk.chem, file="processed_data/chemObservationsSummary.txt")  # Good, extra days for DO and temp
  date.yr.lk.chem.melt <- melt(date.yr.lk.chem)

# Summary plot of # of sampling dates per year
  ggplot(date.yr.lk.chem.melt[with(date.yr.lk.chem.melt, variable != "total" ),], aes(value)) +
    geom_histogram(binwidth = 0.5) +
    xlab("Number of sampling dates per year")

# Summary plot of # of sampling dates per lake
  date.yr.lk.chem$lake <- factor(date.yr.lk.chem$lake, date.yr.lk[order(date.yr.lk.chem$total, decreasing=T), "lake"])  # Needed to order bars
  ggplot(date.yr.lk.chem, aes(lake, total)) + geom_bar() +
    ylab("Total number of sampling dates per lake")  

# Water Temperature
  # Prepare data for plotting
    storet.wide.temp <- dcast(storet[storet$Characteristic.Name == "Temperature, water", ], lake + Station.ID + Activity.Depth + rdate + time ~ 
                           Characteristic.Name, value.var="Result.Value.as.Text")
    names(storet.wide.temp) = c("lake", "Station.ID", "Activity.Depth", "rdate", "time", "Water.Temperature")
    head(storet.wide.temp)
    storet.wide.temp <- storet.wide.temp[with(storet.wide.temp, order(lake, Station.ID, rdate, Activity.Depth)), ]
    unique(paste(storet.wide.temp$Station.ID, storet.wide.temp$rdate))  # over 6000 station x date combinations
  # Plot in one 6000 page .pdf!  Stopped after 15 minutes.  Wrote 3907 pages.
    pdf(file = "output/Figures/tempProfiles.pdf")
    for(i in 1:length(unique(storet.wide.temp$Station.ID))) {
      Station.ID.i <- unique(storet.wide.temp$Station.ID)[i]
      data.i <- storet.wide.temp[storet.wide.temp$Station.ID == Station.ID.i, ]
      for(j in 1:length(unique(data.i$rdate))) {
        date.j <- unique(data.i$rdate)[j]
        data.j <- data.i[data.i$rdate == date.j,]
        try(print(
          ggplot(data.j, aes_string(x="Water.Temperature", y="Activity.Depth")) +
            geom_point() +
            scale_y_reverse() +
            ggtitle(paste(data.j$Station.ID, data.j$rdate))),
            silent=TRUE)
      }
    }
    dev.off()


foo <- LETTERS[1:10]
doo <- LETTERS[c(5,7,2,6,9,6,4,6,8,6)]
match(doo, foo)