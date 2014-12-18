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

# 9999 or 99999999 are used for time and data when data are not provided.  Inspect occurence.
  table(chem$sample_date_fixed)  # No 999 entries, but 1069 instances of 2020-01-01
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
  table(nchar(chem.sub$ID))  # All are 22, 23, and 24.  24 is expected
  table(substr(chem.sub$ID, start=2, stop=4))  # EFR and SRR  
  table(chem.sub$station)  # All have stations
  chem.sub$lake.station <- paste(chem.sub$location, chem.sub$station, sep="")
  table(chem.sub$sample_depth)  # multiple depths

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

  # Pull out all unique combination of "Characteristic.Name" and "Analytical.Proc.ID"
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
    
  # Based on inspection of file (see above), the following naming conventions will be used:
  # Analyte names below are consistent with STORET formatting.  Must use consistent analyte names!   
    chem.sub.analyte.names <- read.table("processed_data/cleanedChemNameMethodCorrected.txt", 
                                         header=TRUE, sep="\t",as.is=TRUE, na.string = "", comment.char="")
    names.index <- match(chem.sub$analyte, chem.sub.analyte.names$analyte)  # position of each analyte name
  # in each row of chem.sub in cleanedChemNameMethod.txt
    chem.sub$analyte.2 <- ifelse(is.na(chem.sub.analyte.names$analyte.2[names.index]),
                                 chem.sub$analyte,
                                 chem.sub.analyte.names$analyte.2[names.index])
#############I think the above replaces all the analyte name code below!!!!!!!!!!!!
    chem.sub$analyte.2 <- {with(chem.sub,
                                ifelse(grepl(pattern="Alkalinity", x=analyte),
                                       "Alkalinity, Total",
                                         ifelse(analyte == "Ammonia, Nitrogen, Dissolved (as N)",
                                                 "NH4-N, Dissolved",
                                                ifelse(analyte == "Ammonia Nitrogen",
                                                       "NH4-N, NA",
                                                       ifelse(analyte=="Ammonia, Nitrogen, Total (as N)",
                                                              "NH4-N, Total",
                                                              
                                         ifelse(analyte == "Carbon, Dissolved Organic",
                                                 "Organic carbon, Dissolved",
                                         ifelse(analyte == "Carbon, Organic, Total",
                                                 "Organic carbon, Total",
                                         ifelse(analyte == "Nitrite+Nitrate Nitrogen, Dissolved",
                                                 "NO2.3-N, Dissolved",
                                         ifelse(analyte == "Oxygen, Dissolved",
                                                 "Dissolved oxygen (DO)",
                                         ifelse(analyte == "Phosphorus, Dissolved",
                                                "reactive.phosphorus, Dissolved",
                                         ifelse(analyte == "Phosphorus, Total",
                                                 "reactive.phosphorus, Total",
                                         ifelse(analyte == "Residue, Dissolved",
                                                 "Total dissolved solids",
                                         ifelse(analyte == "Residue, Suspended",
                                                 "Total suspended solids",
                                         ifelse(analyte == "Residue, Total",
                                                 "Total solids",
                                         ifelse(analyte == "Specific Conductance @ 25C",
                                                 "Specific conductance",
                                         ifelse(analyte == "Temperature, Water",
                                                  "Temperature, water",
                                         ifelse(analyte == "Kjeldahl Nitrogen, Total (as N)",
                                                  "Kjeldahl nitrogen-N, Total",
                                         ifelse(analyte == "Nitrite+Nitrate Nitrogen, Total",
                                                  "NO2.3-N, Total",
                                         ifelse(analyte == "Phosphorus - Dissolved Inorganic",
                                                  "reactive.phosphorus, Dissolved",
                                         ifelse(analyte == "Phosphorus - Total Inorganic",
                                                  "reactive.phosphorus, Total",
                                         ifelse(analyte == "Silica - Dissolved (SiO2)",
                                                  "Silica, Dissolved",
                                         ifelse(analyte == "Hardness, Total (as CaCO3)",
                                                  "Hardness, Ca, Mg, Total",
                                                  analyte))))))))))))))))))))
}
              
  # Remove "Atrazine, Sediment (wet wt.)"
    chem.sub <- chem.sub[chem.sub$analyte.2 != "Atrazine, Sediment (wet wt.)", ]

# DEAL WITH UNITS
# First, strip trailing spaces from units
  chem.sub$units <- gsub("^\\s+|\\s+$", "", chem.sub$units)  # strip leading and trailing spaces

# Change names as necessary
  chem.sub$units2 <- with(chem.sub,
                          ifelse(analyte.2 == "Alkalinity, Total",
                                 "mg/l CaCO3",  # was reported as mg/L
                          ifelse(units == "mg/L" & analyte.2 != "Alkalinity, Total",  # Fixed alkalinity above
                                 "mg/l",
                          ifelse(units == "ug/L",
                                 "mg/l",
                          ifelse(units == "units",  # reported units for pH
                                "none",  # units reported in storet
                                 units)))))

# Convert values based on units
  chem.sub$result_final <- with(chem.sub, 
                                ifelse(units == "ug/L" & units2 == "mg/l",
                                       result_final/1000,
                                       result_final))  
  
# Remove commas from analyte names, confuses ggplot
  unique(sub(pattern=',', replacement=".", x=chem.short$analyte))
  str(chem.short)
# Cast into wide format
  head(dcast(chem.short, ID ~ analyte, 
             value.var = "result_final"))  # Defaults to length,  Must be duplicates

# Inspect duplicates
  length(chem.short[duplicated(chem.short[, c('ID', 'sample_depth', 'rdate', 'lake', 'analyte')]),
             c('ID', 'sample_depth', 'rdate', 'lake', 'analyte')])  # 5308 dups!

  chem.short[chem.short$ID == "2EFR20004201108231100010" & chem.short$analyte ==  "Dissolved Organic Carbon",]


# STORET DATA----------------------
# Read data and clean things up a bit
{
  storet <- read.csv("processed_data/storet.060514.csv",
                       na.strings="NA", as.is=TRUE)
  str(storet)
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
  unique(storet$lake)  # OR stand for Ohio River and can be eliminated
  storet <- storet[!grepl("OR", storet$lake), ]  # Eliminate Ohio River data
  unique(district2$lake) %in% unique(storet$lake)  # All LD lakes represented
  notInLD <- unique(storet$lake) %in% unique(district2$lake)  # 3 lakes not in LD
  unique(storet$lake)[!notInLD]  #TAC = Ohio River, SPC =1 record no lat long, BBC = Ohio River
  storet <- storet[!(storet$lake %in% unique(storet$lake)[!notInLD]), ]  # Remove 3 lakes above
  length(unique(storet$lake))  # 20 lakes, all in district 2, in final data set

# Time  
  storet$time <- substr(x=storet$Activity.Start,  12, 16)  # Will be useful for constructing ID
  storet$rdate <- as.Date(storet$Date)
  storet$year <- substr(storet$rdate, 1, 4)

# Stations
  # TRUE if lake.station in storet is also a routine site in District 2
  station.index <- substr(storet$Station.ID, 2, nchar(storet$Station.ID)) %in% 
    district2$lake.station  
  # 157 lake.station combinations not included in District 2 routine sampling
    unique(storet[!station.index, "Station.ID"])[order(unique(storet[!station.index, "Station.ID"]))]
  # Remove stations that represent inflow or outflow
  # Stations begining with "1" represent in or outflows
    storet <- storet[substr(storet$Station.ID, 5, 5) != 1, ]
}

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
                               ifelse(storet$Units %in% c("in", "ft"), "m",
                               ifelse(storet$Analytical.Proc.ID == "USEPA~310.2" |
                                        storet$Analytical.Proc.ID == "APHA~2320",
                                      "mg/l CaCO3",  # alkalinity initially reported as mg/l
                                      storet$Units)))
# Convert original Result.Value.as.Text to new units when necessary.
# Convert values to numeric first.  We already dealt with all text (i.e. "Not Detected") for
# the variables we care about. So this conversion is ok.
  storet$Result.Value <- as.numeric(storet$Result.Value.as.Text) 

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

  
  with(storet, unique(paste(Characteristic.Name.2, Units2)))[order(with(storet, unique(paste(Characteristic.Name.2, Units2))))]
}

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