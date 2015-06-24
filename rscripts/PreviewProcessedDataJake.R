# PREVIEW PROCESSED DATA FROM MATT

# LIBRARIES-----------------------------------------------------
  library(ggplot2)
  library(reshape)
  library(reshape2)
  library(gdata)
  library(plyr)
  library(dplyr)
  library(readxl)

# READ IN AND FORMAT algae.csv FROM processed_data FOLDER-------------------------
# Reading from processed_data folder
# csv file has umlauts, accents, etc
#  algae <- read.table("processed_data/cleaned_algae_20150619.csv", 
#                      as.is=TRUE, header = TRUE, sep=" ")
  algae <- read_excel("processed_data/cleaned_algae_20150619.xlsx")
  head(algae)
  str(algae)

# Review number of samples, lakes, dates, etc
  table(algae$date)  # Formatted correctly.
  table(algae$lake)  # Unusual lake names from District 3
  unique(algae$station)  # Still need to consult with Jade on a few of these
  unique(algae$depth_ft) # 400, 120, 600, 200 all from RRR (Rough River Lake) 

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
  algae <- filter(algae, district == 2)  # Remove district 3

# Are HAB monitoring data coded correctly?
# All cases where we have cell l-1, but not bv, is from the HAB campaign
  algae[!is.na(algae$cell_per_l) & is.na(algae$BV.um3.L), c("hab")]  # Should all be TRUE. 1331 instances
  sum(algae$hab == TRUE)  # 12237.  This seems like too many.
  filter(algae, hab == TRUE) %>% select(sheet_id) %>% distinct(sheet_id)  # 148 sheets.  Many not hab sheets.

# Analysis of anamolous site names
  algae$lake.station <- paste(algae$lake, algae$station, sep="")  # Create new id
  anamolous <- algae[!(algae$lake.station %in% district2$lake.station), 
                     c("date", "lake.station", "hab", "sheet_id")]  # extract lake.station combo not in official list
  anamolous <- anamolous[!(duplicated(anamolous$lake.station)), 
                         c("lake.station", "date", "hab", "sheet_id") ]  # Remove duplicated records
  sheetID <- read.csv("processed_data/summaryStatus_20140716.csv", as.is = TRUE) 
  anamolous <- merge(anamolous, sheetID[, c("sheet_id", "file")])
  anamolous.algae <- anamolous[order(anamolous$lake.station),]  # Order
  write.table(anamolous.algae, file="output/anamolousNamesAlgae.txt", row.name=FALSE)  # These are mostly fine.

# Weird values in cell_per_l
# NA first
  summary(algae$cell_per_l)  # 21 NA?
  filter(algae, is.na(cell_per_l)) %>% select(BV.um3.L) %>% 
    summarize(total=sum(!is.na(BV.um3.L)))  #21 records have BV, but no cell.L
  filter(algae, is.na(cell_per_l)) %>% select(sheet_id) # all from sheet 1379. ok.  See github issue #44
# <0 or 9999
  filter(algae, cell_per_l < 0) %>% select(lake)  # none
  filter(algae, cell_per_l == 9999) %>% select(lake, rdate, ID, sheet_id, cell_per_l, taxa)  #only one.  EFR 2005-05-11
# =0
  filter(algae, cell_per_l == 0) %>% 
    select(lake, cell_per_l, BV.um3.L, sheet_id)  # These all have a BV of 0 or -9999.  remove
  algae <- filter(algae, cell_per_l != 0)

# Weird values in BV.um3.L
# NA first
  summary(algae$BV.um3.L)  #1331 NA
  filter(algae, is.na(BV.um3.L)) %>% select(BV.um3.L, cell_per_l, hab)  # revisit after issue 14 is fixed
# <0 or 9999
  filter(algae, BV.um3.L < 0) %>% select(lake, BV.um3.L, cell_per_l)  # 87 with -9999, but all have cell_per_l
  filter(algae, BV.um3.L == 9999) %>% select(lake, rdate, ID, sheet_id, cell_per_l, taxa)  # none with 9999
# = 0
  filter(algae, BV.um3.L == 0) %>% select(lake, sheet_id, BV.um3.L, cell_per_l)  # 284, but all have cell_per_l values.  convert to NA.
  algae <- mutate(algae, BV.um3.L=replace(BV.um3.L, BV.um3.L == 0, NA))  # replace 0 with NA 

# Strip leading and trailing spaces in taxa  
  algae$taxa <- gsub("^\\s+|\\s+$", "", algae$taxa)
  unique(algae$taxa)[order(unique(algae$taxa))][1:700] # Good
  unique(algae$class)  # Good
  unique(algae$hab)  # Good
  unique(algae$qual_replicate)  # Looks good, NA, Q, or R
  # All observations should include a taxa name
  length(algae[is.na(algae$taxa), c("taxa", "ID", "sheet_id", "cell_per_l", "BV.um3.L")][,1])  # good

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
    ggplot(date.yr.lk, aes(lake, total)) + geom_bar(stat="identity") +
    ylab("Total number of sampling dates per lake") 
  # Summary plot of sites per lake
    sites.lake <- aggregate(algae$lake.station, by=list(lake=algae$lake, year=algae$rdate), FUN=function(X1) {length(unique(X1))})
    ggplot(sites.lake, aes(x)) + 
      geom_histogram(binwidth = 0.5) +
      xlab("Number of sampling sites per lake per sampling date")
  # Which observations are hab = TRUE
    unique(algae[algae$hab == TRUE, c("lake", "date")])

# SUMMARY STATS FOR TAXONOMY CONTRACT-------------------
# This has changed since the taxonomist found a few problems.
  #1.  How many samples were taken over the sampling period?
  length(unique(algae$ID))  #8200 (now 7470), should be number of unique samples

  #2.  How many data points are in the historic data set?
  length(algae$ID)  # could execute against any column.  211958
  #How many in Missing Biovolume List?
  miss.biov <- algae[is.na(algae$BV.um3.L) & !is.na(algae$cell_per_l),  # Extract taxa per lake x date. Doesn't account for depth x station
                     c("lake", "rdate", "taxa")]
  miss.biov <- miss.biov[with(miss.biov, order(lake, rdate)), ]  # Reorder
  length(miss.biov$lake)  # 1331 observations with cell per l, but no BV.  
  u.miss.biov <- unique(miss.biov)  # Pull out unique taxa per lake x date.  Doesn't account for depth x station
  length(u.miss.biov$lake)  # 217 unique taxa x lake x date combination

# RECONCILE TAXONOMY AGAINST CONTRACTORS CORRECTED LIST----------------------------------------
# MUST REVISIT AFTER WILL GENERATES UPDATED TAXA LIST
  taxa.bsa <- read.xls("originalData/algae/BSA DRAFT EXPANDED TAXA LIST V3_06_18_2015.xls", 
                       sheet = "BSA DRAFT EXPANDED TAXA LIST V3", as.is = TRUE)
  length(taxa.bsa$Original.Taxa.Name)  #1834 taxa

# Strip leading and trailing spaces in taxa  
  taxa.bsa$Original.Taxa.Name <- gsub("^\\s+|\\s+$", "", taxa.bsa$Original.Taxa.Name)
# Inspect for duplicate Original.Taxa.Name
  sum(duplicated(taxa.bsa$Original.Taxa.Name))  #3
  filter(taxa.bsa, duplicated(taxa.bsa$Original.Taxa.Name) | # view specifics
           duplicated(taxa.bsa$Original.Taxa.Name, fromLast = TRUE)) %>%
    select(Original.Taxa.Name, Accepted.Name)
  taxa.bsa <- filter(taxa.bsa, !duplicated(Original.Taxa.Name))

# Repetitive columns in BSA file, but all are identical
  sum(taxa.bsa$Genera != taxa.bsa$Genera.1)

# Are all unique taxa names in BSA's list
  sum(!(unique(algae$taxa) %in% taxa.bsa$Original.Taxa.Name))  # 5 values not in BSA list.  Take a look
  filter(algae, !(algae$taxa %in% taxa.bsa$Original.Taxa.Name)) %>% # not in BSA list
    select(taxa) %>% # pull out taxa
    distinct(taxa)  # pull out unique.
# Tweak values to conform with BSA list
# What is the problem with Crucigenia quadrata C. Morren!?  I see the value in BSA's excell sheet
  filter(taxa.bsa, grepl(pattern = "quadrata", x = Original.Taxa.Name)) %>%
    select(Original.Taxa.Name)# Here it is
# Lets make sure there are no weird invisible symbols in taxa name for value in two datasets
  bad.taxa.index <- grepl(pattern = "quadrata", x = taxa.bsa$Original.Taxa.Name)
  taxa.bsa[bad.taxa.index, "Original.Taxa.Name"] = "Crucigenia quadrata C. Morren"
  bad.taxa.index <- grepl(pattern = "quadrata", x = algae$taxa)
  algae[bad.taxa.index, "taxa"] = "Crucigenia quadrata C. Morren"
# Now check for mismatches
  filter(algae, !(algae$taxa %in% taxa.bsa$Original.Taxa.Name)) %>% # not in BSA list
    select(taxa) %>% # pull out taxa
    distinct(taxa)  # pull out unique.  Only 3.  fix manually

  algae <- mutate(algae, taxa = replace(taxa, taxa == "Acanthoceras (Attheya) zachari", 
                                        "Acanthoceras  zachari (Attheya)"))  %>%
    mutate(taxa = replace(taxa, taxa == "Carteria", 
                          "Carteria #1"))  %>%
    mutate(taxa = replace(taxa, taxa == "Trachelomonas spp", 
                          "Trachelomonas spp."))
# Now check for mismatches
  filter(algae, !(algae$taxa %in% taxa.bsa$Original.Taxa.Name)) %>% # not in BSA list
    select(taxa) %>% # pull out taxa
    distinct(taxa)  # pull out unique.  None, got them all.

# Are all of BSA's names in the algae file?
  sum(!(unique(taxa.bsa$Original.Taxa.Name) %in% algae$taxa))  # 18 not in algae file
  filter(taxa.bsa, !(taxa.bsa$Original.Taxa.Name %in% algae$taxa)) %>% # not in algae file
    select(Original.Taxa.Name) %>% # pull out taxa
    distinct(Original.Taxa.Name)  # pull out unique.  Most, but not all, have a strange character.

# Merge
  algae.bsa <- merge(algae, subset(taxa.bsa, select = -c(X)),  # #obs should = that in algae.
                     by.x = "taxa", 
                     by.y = "Original.Taxa.Name")


# CONVERT BLUE-GREEN CELL COUNTS TO BIOVOLUME-----------------------
# First, identify all taxa that need biovolume (should be all HAB == TRUE records)
# Second, identify all examples where these taxa were found in routine monitoring
# and have biovolumes

# Find missing biovolumes
  filter(algae.bsa, BV.um3.L == -9999 | is.na((BV.um3.L))) %>% 
    select(lake) %>% 
    summarize(total = length(lake))  # 1702 total instances where BV is NA or -9999
  filter(algae.bsa, (BV.um3.L == -9999 | is.na((BV.um3.L))) & is.na(cell_per_l)) %>% 
    select(lake) %>% 
    summarize(total = length(lake))  # 0, all instances where BV is NA or -9999 has cell_per_l values
# Unique lake x data x taxa combinations where BV is needed
  needBio <- filter(algae.bsa, BV.um3.L == -9999 | is.na((BV.um3.L))) %>% 
  select(lake, rdate, Accepted.Name) %>%
  distinct(lake, rdate, Accepted.Name)  # 328 unique lake x date x taxa occurences
# Full list of where BV is needed, including reported BV and cell counts,
# This list will go to BSA
  MissingBiovolume <- filter(algae.bsa, BV.um3.L == -9999 | is.na((BV.um3.L))) %>% 
                      select(lake, rdate, Accepted.Name, BV.um3.L, cell_per_l)

# Find biosource data
# Logical indicating which lake x taxa combinations in needBio are in the ld.algae where hab=F
  bioSourceIndex <- paste(algae.bsa$lake, algae.bsa$Accepted.Name, sep="") %in%  #16060 TRUE values
    paste(needBio$lake, needBio$Accepted.Name, sep="")
  bioSource <- filter(algae.bsa, bioSourceIndex & 
                        (!is.na(BV.um3.L) & BV.um3.L != -9999 & 
                           !is.na(cell_per_l) & cell_per_l != 9999)) %>%  # 14358 observations
               mutate(Bio.per.cell = BV.um3.L / cell_per_l)  %>%
               select(Accepted.Name, lake, rdate, BV.um3.L, cell_per_l, Bio.per.cell)
  summary(bioSource$Bio.per.cell)


# Now, see if any of the critters that need biovolume were not included in the monitoring
# data.
  notInalgae.bsa.index <- paste(needBio$lake, needBio$Accepted.Name, sep="") %in% 
      paste(algae.bsa[!is.na(algae.bsa$BV.um3.L), "lake"],
            algae.bsa[!is.na(algae.bsa$BV.um3.L), "Accepted.Name"], sep="")
  sum(!notInalgae.bsa.index)  # 51 do not have corresponding BV in algae.bsa    
  needBio[!notInalgae.bsa.index,]  # Which ones are missing

# Revisit after issues #26, #27, and #3 have been resolved.
# Loop to plot the measured per cell biovolumes for each lake x taxa combination
# included in the HAB data set.
for(j in 1:length(unique(bioSource$lake))) {
  lake.j <- unique(bioSource$lake)[j]
  bioSource.j <- bioSource[bioSource$lake == lake.j,]
  needBio.j <- needBio[needBio$lake == lake.j,]
  for (i in 1:length(unique(bioSource.j$Accepted.Name))) { 
    taxa.i <- unique(bioSource.j$Accepted.Name)[i]
    pdf(file = paste("output/Figures/", lake.j, ".", taxa.i, ".pdf", sep=""))
    try(print(
      ggplot(bioSource.j[bioSource.j$Accepted.Name == taxa.i,], aes(rdate, Bio.per.cell)) + 
        geom_point() + 
        ylab("Biovolume per cell (um3)") +
        ggtitle(bquote(atop(paste(.(lake.j), " ", .(taxa.i), sep=""), 
                            "Red points indicate biovolume is needed"))) +
        geom_point(data=needBio.j[needBio.j$Accepted.Name == taxa.i, ], 
                   aes(rdate, 0),
                   color="red")
    ),
        silent=TRUE
    )
    dev.off()
  }
}  # About 3 minutes

# Products for BSA
  MissingBiovolumeBSA <- filter(MissingBiovolume, lake == "CCK", Accepted.Name == "Cylindrospermopsis raciborskii")
  BiovolumeSourceBSA <- filter(bioSource, lake == "CCK", Accepted.Name == "Cylindrospermopsis raciborskii")

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
              "processed_data/cleaned_wq_20150105.csv", 
              sep = ",", row.names=FALSE)

# REVIEW STORET DATA----------------------
# See readStoret.R
  storet.final <- read.csv("processed_data/storet.010515.csv",
                       na.strings="NA", as.is=TRUE)
  str(storet.final)


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

# MERGE STORET AND PROCESSED CHEM DATA FROM JADE AND DREW------------------
# Read chem data
  chem.processed <- read.csv("processed_data/cleaned_wq_20150105.csv",
                   na.strings="NA", as.is=TRUE)
  str(chem.processed)

# rbind
  all.chem <- rbind(subset(x=storet.final, select=-c(station.latitude, station.longitude)), # exlcude lat/lon
                    chem.processed)
  str(all.chem)
  sum(duplicated(all.chem[, c("id", "analyte")]))  #0!

# A bit of formatting
  all.chem$rdate <- as.Date(all.chem$date)
  all.chem$year <- format(all.chem$rdate, "%Y")


# Duration & completeness of data set
  date.yr.lk.all.chem <- aggregate(all.chem$rdate, by=list(lake=all.chem$lake, year=all.chem$year), FUN=function(X1) {length(unique(X1))})
  date.yr.lk.all.chem <- dcast(date.yr.lk.all.chem, lake ~ year, value.var="x")  # Good
  date.yr.lk.all.chem$total <- apply(subset(date.yr.lk.all.chem,  select = -c(lake)), MARGIN=1, FUN=sum, na.rm=T)  # Calculate total per lake
  year.range <- min(as.numeric(names(date.yr.lk.all.chem)), na.rm=T):2014  # Define range of years included in data
  missing.years <- year.range[!(year.range %in% as.numeric(names(date.yr.lk.all.chem)))]  # Define missing years
  date.yr.lk.all.chem[,as.character(missing.years)] <- NA  # Add columns for missing years
  date.yr.lk.all.chem <- date.yr.lk.all.chem[, c("lake", as.character(sort(as.numeric(names(date.yr.lk.all.chem)))), "total")]  # reorder columns
  date.yr.lk.all.chem <- date.yr.lk.all.chem[with(date.yr.lk.all.chem, order(lake)), ]  # reorder rows
  date.yr.lk.all.chem[is.na(date.yr.lk.all.chem)] = 0  # If no samples were collected, set equal to 0
  write.table(date.yr.lk.all.chem, file="processed_data/chemObservationsSummary.txt")  # Good, extra days for DO and temp
  date.yr.lk.all.all.chem.melt <- melt(date.yr.lk.all.chem)

# Summary plot of # of sampling dates per year
ggplot(date.yr.lk.all.chem.melt[with(date.yr.lk.all.chem.melt, variable != "total" ),], aes(value)) +
  geom_histogram(binwidth = 0.5) +
  xlab("Number of sampling dates per year")

# Summary plot of # of sampling dates per lake
  date.yr.lk.all.chem$lake <- factor(date.yr.lk.all.chem$lake, date.yr.lk[order(date.yr.lk.all.chem$total, decreasing=T), "lake"])  # Needed to order bars
  ggplot(date.yr.lk.all.chem, aes(lake, total)) + geom_point() +
    ylab("Total number of sampling dates per lake")  




# Some plots
  ggplot(all.chem[all.chem$analyte == "NO2.3-N, Total", ], aes(lake, result)) + geom_point()
  ggplot(all.chem[all.chem$analyte == "NO2.3-N, NA", ], aes(lake, result)) + geom_point()
  ggplot(all.chem[all.chem$analyte == "NO2.3-N, Dissolved", ], aes(lake, result)) + geom_point()

# Convert to data.table for fast manipultaion
library(data.table)
all.chem.dt <- data.table(all.chem)
setkey(all.chem.dt,"id")

# Remove commas from analyte names, confuses ggplot
all.chem.dt$analyte <- sub(pattern=', ', replacement=".", x=all.chem.dt$analyte)

# Cast to wide
all.chem.dt.wide <- dcast.data.table(all.chem.dt, 
                                     id + district + lake + station + depth.ft + date + time + qual ~ analyte, 
                                     value.var = "result")

# EVALUATE BSA PHASE-1 TAXONOMY PRODUCT---------------------
  bsa.taxa <- read.xls("originalData/algae/BSA DRAFT EXPANDED TAXA LIST V1_03_20_2015.xls", 
                       sheet = "BSA DRAFT EXPANDED TAXA LIST V1", 
                       na.strings = "", as.is=TRUE)

# Strip leading and trailing spaces from "Actions" column
  bsa.taxa <- mutate(bsa.taxa, Actions = gsub("^\\s+|\\s+$", "", Actions))

# Review "Actions"
  table(bsa.taxa$Actions)  # 42 deleted entries

# Review "Taxa Type"
  table(bsa.taxa$Taxa.Type)  # 43 observations of Taxa.Type == na
  filter(bsa.taxa, Taxa.Type == "na") %>%
  select(Actions, Accepted.Name)  # all but one are Action = "invalid deleted"
  




