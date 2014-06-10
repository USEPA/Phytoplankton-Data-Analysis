# PREVIEW PROCESSED DATA FROM MATT

# LIBRARIES-----------------------------------------------------
  library(ggplot2)
  library(reshape)
  library(reshape2)
  library(gdata)

# READ IN AND FORMAT algae.csv FROM processed_data FOLDER-------------------------
# Reading from processed_data folder
  algae <- read.delim("processed_data/cleaned_algae_20140509.txt", as.is=TRUE, header = TRUE)
  head(algae)
  str(algae)

# Review number of samples, lakes, dates, etc
  table(algae$date)  # Formatted correctly.
  table(algae$lake)  # Unusual lake names from District 3
  unique(algae$station)  # Still need to consult with Jade on a few of these
  unique(algae$depth_ft) # Good

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
  sheetID <- read.csv("processed_data/summaryStatus_20140423.csv", as.is = TRUE) 
  anamolous <- merge(anamolous, sheetID[, c("sheet_id", "file")])
  anamolous.algae <- anamolous[order(anamolous$lake.station),]  # Order
  write.table(anamolous.algae, file="output/anamolousNamesAlgae.txt", row.name=FALSE)  

# Look at values of algal density and biovolume
  summary(algae$cell_per_l)
  algae[algae$cell_per_l < 0, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$cell_per_l == -9999, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$BV.um3.l  < 0, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]
  algae[algae$cell_per_l == -9999, c("lake", "rdate", "ID", "sheet_id", "cell_per_l", "taxa")]


# Strip leading and trailing spaces in taxa  
  algae$taxa <- gsub("^\\s+|\\s+$", "", algae$taxa)
  unique(algae$taxa)[order(unique(algae$taxa))][1:700] # Good
  unique(algae$class)  # Good
  unique(algae$hab)  # Good
  unique(algae$qual_replicate)  # Looks good, NA, Q, or R
  # All observations should include a taxa name
  length(algae[is.na(algae$taxa), c("taxa", "ID", "sheet_id", "cell_per_l", "BV.um3.L")][,1])

# SUMMARY TABLES FOR PRESENTATIONS------------------
  algae$rdate <- as.Date(as.character(algae$date), format = '%Y%m%d')
  algae$year <- as.numeric(substr(algae$date, 1,4))
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
    geom_histogram(binwidth = 0.5) +
    xlab("Number of sampling dates per year")
  # Summary plot of # of sampling dates per lake
    date.yr.lk$lake <- factor(date.yr.lk$lake, date.yr.lk[order(date.yr.lk$total, decreasing=T), "lake"])  # Needed to order bars
    ggplot(date.yr.lk, aes(lake, total)) + geom_bar() +
    ylab("Total number of sampling dates per lake") 
  # Summary plot of sites per lake
    sites.lake <- aggregate(algae$lake.station, by=list(lake=algae$lake, year=algae$year), FUN=function(X1) {length(unique(X1))})
    ggplot(sites.lake, aes(x)) + 
      geom_histogram(binwidth = 0.5) +
      xlab("Number of sampling sites per lake")
  # Which observations are hab = TRUE
    unique(algae[algae$hab == TRUE, c("lake", "date")])

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
  chem <- read.table("processed_data/combined_wq_20140509.txt", sep = "\t", 
                     header = TRUE, fill=TRUE, comment.char="",
                      as.is = TRUE)
  head(chem)
  str(chem)

# 9999 or 99999999 are used for time and data when data are not provided.  Inspect occurence.
  table(chem$sample_date)  # No 999 entries, but many other strange dates
  sum(is.na(chem$sample_date))  # No missing values
  table(chem$sample_time)  # 74 occurences of 9999
  chem$rdate <- as.Date(as.character(chem$sample_date), format = '%Y%m%d')
  table(chem$rdate)

# Inspect ID
  table(nchar(chem$ID))  # Most are 24, as expected, but also have 21, 22, and 23.
  table(substr(chem$ID, start=2, stop=4))  # Mostly EFR.  9 lakes represented
  table(chem$location)
  table(chem$lake)  # Only 9 lakes 
  sum(is.na(chem$lake))  # 19127 missing values
  nrow(chem[is.na(chem$lake) & !is.na(chem$location), c("lake", "location")][2])
  table(chem$station)

  chem$lake.station <- paste(chem$lake, chem$station, sep="")
  table(chem$lake.station)  # revisit after lake names have been put together
  table(chem$sample_depth)
  sum(is.na(chem$sample_depth))

# Compare names to standard names Jade previously provided.  Write file with weird names.
  isLake.StationInDistrict2 <- chem$station %in% paste(2, district2$lake.station, sep="") 
  str(isLake.StationInDistrict2)  
  unique(chem[!isLake.StationInDistrict2, "station"])
  more.names <- read.xls("originalData/algae/EFR Phytoplankton Data/Drew data/j/EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx",
                         sheet="20102011FIELD_DATA_NOT_IMPORTED", as.is=TRUE)
  anamolous.chem <- unique(more.names[!(more.names$Location  %in% 
                                          paste(2, district2$lake.station, sep="")), "Location"]
  )
  anamolous.chem <- data.frame(lake.station = anamolous.chem, 
                               file = "EFR_NOT_IMPORTED_CHEMICAL_2003-2012_2011_2.xlsx",
                               sheet = "20102011FIELD_DATA_NOT_IMPORTED")
  write.table(anamolous.chem, file = "output/anamolousNamesChem.txt", row.names=FALSE)

# CENSORED WATER CHEM DATA-------------------------------------------------------------
# Dual censored values: microcystis
# Few data, all censored.  Won't use in analysis
  chem[!is.na(chem$qual1) & !is.na(chem$qual2), 
       c('analyte', 'original', 'qual1', 'qual2', 'result_num2', 'result_num')
       ]  # 68 dual censored microsystin values
  table(chem[chem$analyte == 'Microcystin', 'original']) #  All values are censored, mostly left.
  table(chem[chem$analyte == 'Microcystin', c('ID', 'sample_date','original')]) #  2011 data from EFR only.  Won't use in analysis

# Left censored values
  chem$result_final <- chem$result_num  # This vector will contain final numbers for analysis
  table(chem$qualifiers)  # Don't know what most of these are.  U is undetectable
  table(chem$qual1)  # Simplified qualifiers.
# Left censored values will be set to 1/2 dl
  chem[chem$qual1 == '<' & !is.na(chem$qual1), 'result_final'] = 
    chem[chem$qual1 == '<' & !is.na(chem$qual1), 'result_num'] * 0.5

# ND: when the data are reported as ND with a U qualifier, Matt set the value equal to -777.
  chem[chem$qual1 == 'ND' & !is.na(chem$qual1),  # View data 
       c('ID', 'analyte', 'qualifiers', 'qual1', 'detect_limit', 'original', 'result_num')]
# Will change to 1/2 detection limit
  chem[chem$qual1 == 'ND' & !is.na(chem$qual1), 'result_final'] =
    as.numeric(chem[chem$qual1 == 'ND' & !is.na(chem$qual1), 'detect_limit']) * 0.5
length(chem$analyte)

# TAKE A LOOK AT WATER CHEM ANALYTES---------------------------------------------------
  write.table(unique(chem$analyte), #  274 analytes.  Lots of overlap in N species
              file = paste("output/analyte.names.", Sys.Date(), ".txt", sep=""),
              row.names=FALSE)
              
# Look at N species
  unique(chem[grep(pattern='Nitrogen', x=chem$analyte), 'analyte'])
  chem[chem$analyte == 'Ammonia Nitrogen', 'analyte'] = 'Nitrogen, Ammonia'
  chem[chem$analyte == 'Nitrate+Nitrite Nitrogen' | chem$analyte == 'Nitrate + Nitrite Nitrogen',
       'analyte'] = 'Nitrogen, Nitrate-Nitrite'

# Create a list of species I want to retain for analysis
  analyte.list <- c('Alkalinity, Total (As CaCO3)',
                    'Nitrogen, Ammonia',
                    'Nitrogen, Kjeldahl, Total',
                    'Organic Carbon, Dissolved',
                    'Phosphorus, Total (As P)',
                    'Total Hardness (As CaCO3)',
                    'Total Organic Carbon',
                    'Alkalinity',
                    'Dissolved Organic Carbon',
                    "Nitrogen, Nitrate-Nitrite",
                    "Nitrogen Kjeldahl",
                    "Phosphorus",
                    "Total Dissolved Solids",
                    "Total Hardness",
                    "Total Suspended Solids",
                    "Chlorophyll a",
                    "Atrazine",
                    "Temp.Celsius",
                    "D.O...mg.L.",                 
                    "SP.Cond..UMHO.cm.",
                    "pH",
                    "Turbidity..NTU.",             
                    "Secchi..Inches.",
                    'Nitrate Nitrogen'
                    )
  chem.short <- chem[chem$analyte %in% analyte.list, ] 
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


algae <- read.delim("processed_data/cleaned_algae_20140422.txt", 
                    as.is=TRUE, 
                    header = TRUE)
unique(algae$lake)
algae[algae$lake == 'grr', 'sheet_id']

# STORET DATA----------------------
  storet <- read.csv("processed_data/storet.060514.csv",
                       na.strings="NA", as.is=TRUE)
  str(storet)
  unique(storet$Activity.Medium)  # All water samples
  unique(storet$Sample.Fraction)  # Total and Dissolved
  storet <- storet[, c("State", "Station.ID", "Activity.Start", "Activity.Depth", "Characteristic.Name", "Sample.Fraction",
                       "Result.Value.as.Text", "Units", "lake", "Date")]
# Lakes
  storet <- storet[storet$State != "ILLINOIS",]  
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

# Analyte names
  write.table(unique(storet$Characteristic.Name), file="processed_data/storetCharacteristicName.txt",
              row.names=FALSE)
  unique(storet$Characteristic.Name)[grepl("Nitrogen", unique(storet$Characteristic.Name))]
  unique(storet$Characteristic.Name)[grepl("nitrogen", unique(storet$Characteristic.Name))]
  unique(storet$Characteristic.Name)[grepl("Phosphorus", unique(storet$Characteristic.Name))]
  unique(storet$Characteristic.Name)[grepl("phosphorus", unique(storet$Characteristic.Name))]
  unique(storet[storet$Characteristic.Name == "Phosphorus", c("Characteristic.Name", "Sample.Fraction")])
  unique(storet[storet$Characteristic.Name == "Iron", c("Characteristic.Name", "Sample.Fraction")]) 
  unique(storet[storet$Characteristic.Name == "Molybdenum", c("Characteristic.Name", "Sample.Fraction")])   
  unique(storet[storet$Characteristic.Name == "Copper", c("Characteristic.Name", "Sample.Fraction")])  
  unique(storet[storet$Characteristic.Name == "Manganese", c("Characteristic.Name", "Sample.Fraction")]) 
  unique(storet[storet$Characteristic.Name == "Temperature, water", c("Characteristic.Name", "Sample.Fraction")])

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




"Station.ID", "Activity.Start", "Activity.Depth", "Characteristic.Name", "Sample.Fraction",
"Result.Value.as.Text", "Units", "lake", 
