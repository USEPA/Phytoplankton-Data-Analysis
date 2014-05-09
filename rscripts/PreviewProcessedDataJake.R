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

# Revisit after above issues have been resolved
  algae$rdate <- as.Date(as.character(algae$date), format = '%Y%m%d')
  algae$year <- as.numeric(substr(algae$date, 1,4))
  date.yr.lk <- aggregate(algae$rdate, by=list(lake=algae$lake, year=algae$year), FUN=function(X1) {length(unique(X1))})
  date.yr.lk <- dcast(date.yr.lk, lake ~ year, value.var="x")
  date.yr.lk$total <- apply(subset(date.yr.lk,  select = -c(lake)), MARGIN=1, FUN=sum, na.rm=T)  # Calculate total per lake
  date.yr.lk$district <- ifelse(date.yr.lk$lake %in% district2$lake, 2,
                                ifelse(date.yr.lk$lake %in% district3$lake, 3, NA))  # Add district
  year.range <- min(as.numeric(names(date.yr.lk)), na.rm=T):2014  # Define range of years included in data
  missing.years <- year.range[!(year.range %in% as.numeric(names(date.yr.lk)))]  # Define missing years
  date.yr.lk[,as.character(missing.years)] <- NA  # Add columns for missing years
  date.yr.lk <- date.yr.lk[, c("district", "lake", as.character(sort(as.numeric(names(date.yr.lk)))), "total")]  # reorder columns
  date.yr.lk <- date.yr.lk[with(date.yr.lk, order(district, lake)), ]  # reorder rows
  date.yr.lk[is.na(date.yr.lk)] = 0  # If not samples were collected, set equal to 0
  write.table(date.yr.lk, file="processed_data/algaeObservationsSummary.txt")
  date.yr.lk.melt <- melt(date.yr.lk)
  # Summary plot of # of sampling dates per year
    ggplot(date.yr.lk.melt[with(date.yr.lk.melt, variable != "total" & variable != "district"),], aes(value)) +
    geom_histogram(binwidth = 0.5) +
    xlab("Number of sampling dates per year")
  # Summary plot of # of sampling dates per lake for district 2  
    date.yr.lk$lake <- factor(date.yr.lk$lake, date.yr.lk[order(date.yr.lk$total, decreasing=T), "lake"])  # Needed to order bars
    ggplot(date.yr.lk, aes(lake, total)) + geom_bar()    


# POPULATE 'CLASS' FIELD----------------------------------------
  unique(algae$class)  # Not filled out yet
  algae <- subset(algae, select = -c(class))  # Remove class field.  Merge it in below.
  # Lisa's algal classification data
    clas <- read.xls('originalData/algae/LouisvilleDistrictPhytoClassification.05092014.xlsx', 
                     header = TRUE,
                     na.strings=c('', 'NA'),
                     as.is = TRUE)  # read.delim works while read.table doesn't.  Related to quote specification (http://stackoverflow.com/questions/3016333/r-why-does-read-table-stop-reading-a-file)
    str(clas)
    clas <- clas[,c('Taxa', 'Class')]  # Remove notes columns
    names(clas) <- c('taxa', 'class')  # Change names per phytoplankton data precedent
    clas$taxa <- gsub("^\\s+|\\s+$", "", clas$taxa)  # Remove white spaces
    clas$class <- gsub("^\\s+|\\s+$", "", clas$class)  # Remove white spaces
  # Merge clas with phytoplankton data  
    ld.algae <- merge(algae, clas, by = 'taxa', all=T)  # 'ld' for Louisville District
    str(ld.algae)  
    ld.algae$Bio.per.cell <- with(ld.algae, BV.um3.L / cell_per_l)
  # Pull out algal taxa w/out a corresponding class ID from Lisa
    no.class <- unique(ld.algae[is.na(ld.algae$class) & !is.na(ld.algae$taxa), 'taxa' ])  # Where Class is NA, but taxa is known.  Unique to reduce redundancies.  Send to Lisa for updating.
    no.class[order(no.class)]  # Only a few, very god.
    write.table(no.class, file = "output/no.class.txt", row.names=F)

# CONVERT BLUE-GREEN CELL COUNTS TO BIOVOLUME---------------------
# First, identify all taxa that need biovolume (all HAB == TRUE records)
# Second, identify all examples where these taxa were found in routine monitoring
# and have biovolumes
  ld.algae[ld.algae$hab == TRUE & is.na(ld.algae$class), "class"] <- "Blue-green"  # Some hab data didn't have the class field populated
  needBio <- unique(ld.algae[ld.algae$hab == TRUE, c("lake", "rdate", "taxa")])  # Pull out unique taxa per lake x date.  Doesn't account for depth x station
  needBio <- needBio[!apply(needBio, FUN=function(x) all(is.na(x)), MARGIN=1),]  # Eliminate rows with all NAs
  needBioEFR <- unique(needBio[needBio$lake == "EFR", "taxa"])  # Pull out taxa from EFR that need biovolume
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
      bg.cells <- with(ld.algae[ld.algae$class == 'Blue-green', ],
                       aggregate(cell_per_l ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(bg.cells)[which(colnames(bg.cells) == 'cell_per_l')] = 'bg.cell_per_l'   # Rename

  # Algal biovolumes
    # Calculate total biovolume per lake, station, depth, and date
      t.biov <- with(ld.algae, aggregate(BV.um3.L ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(t.biov)[which(colnames(t.biov) == 'BV.um3.L')] = 't.BV.um3.L'  # Rename
    # Calculate Blue-Green biovolume per lake, station, depth, and date           
      bg.biov <- with(ld.algae[ld.algae$class == 'Blue-green', ],
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
    ld.algae.agg[ld.algae.agg$hab == TRUE, 'prop.bg.cell'] = NA                             #This code eliminates any data from HAB monitoring where only BG where quantified. 

  # Reinforce order
    ld.algae.agg <- ld.algae.agg[with(ld.algae.agg, order(rdate, station, depth_ft)), ]

# QUICK GGPLOT FIGURES-------------------------------------
  # Formatting data for x-axis
    x_breaks <- seq(as.Date('1986/1/1'), as.Date('2014/1/1'), by = '2 year')
    x_labels <- as.character(x_breaks, format='%Y')

  # Biovolume plots
  # Total Biovolume
    ggplot(ld.algae.agg, aes(rdate, t.BV.um3.L)) + geom_point() + ylab('Total Biovolume') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
    # BG Biovolume      
      ggplot(ld.algae.agg[ld.algae.agg$lake == "EFR",], aes(rdate, bg.BV.um3.L)) + geom_point() + ylab(expression(paste('BG Biovolume ( ', mu, m^3, '/L)'))) + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    # Proportion BG biovolume
      ggplot(ld.algae.agg, aes(rdate, prop.bg.BV)) + geom_point() + ylab('Proportion BG Biovolume') + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    
  # Cells per L plots                  
  # Total cells
    ggplot(ld.algae.agg, aes(rdate, t.cell_per_l)) + geom_point() + ylab('Total Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) 
  # BG cells  
  ggplot(ld.algae.agg[ld.algae.agg$lake == "EFR", ], aes(rdate, bg.cell_per_l)) + geom_point() + ylab('Blue Green Cell count') + 
    scale_x_date(breaks=x_breaks, labels = x_labels) +
    ggtitle("EFR")
  
  # Proportion BG cells  
    ggplot(ld.algae.agg[ld.algae.agg$lake == "EFR", ], aes(rdate, prop.bg.cell)) + geom_point() + ylab('Proportion Blue Green by Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)

# PREVIEW WATER CHEM DATA THAT MATT IMPORTED------------------------------------------
# Reading from output/processed_data folder
  chem <- read.table("processed_data/water_quality.csv", sep = ",", header = TRUE, as.is = TRUE,na.strings=c("", "NA"))
  head(chem)
  str(chem)

# 9999 or 99999999 are used for time and data when data are not provided.  Inspect occurence.
  table(chem$sample_date)  # No 999 entries, but 726 occurences of 30501 and 6 of 30502?
  table(chem$sample_time)  # 74 occurences of 9999
  chem$rdate <- as.Date(as.character(chem$sample_date), format = '%Y%m%d')
  table(chem$rdate)

# Inspect ID
  table(nchar(chem$ID))  # Most are 24, as expected, but also have 21, 22, and 23.
  table(substr(chem$ID, start=2, stop=4))  # Mostly EFR.  9 lakes represented
  chem$lake <-  substr(chem$ID, start=2, stop=4)
  table(chem$lake) 
  table(chem$location)
  chem$station <- ifelse(nchar(chem$location) <= 6, paste(chem$lake, chem$lake, sep=""),
                             chem$location)
  table(chem$station)

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
  table(chem[chem$analyte == 'Microcystin', c('ID', 'rdate','original')]) #  2011 data from EFR only.  Won't use in analysis

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
    chem[chem$qual1 == 'ND' & !is.na(chem$qual1), 'detect_limit'] * 0.5

# TAKE A LOOK AT WATER CHEM ANALYTES---------------------------------------------------
  unique(chem$analyte)  # 101 analytes.  Lots of overlap in N species

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



