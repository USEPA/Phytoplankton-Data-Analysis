# PREVIEW PROCESSED DATA FROM MATT

# LIBRARIES-----------------------------------------------------
  library(ggplot2)
  library(reshape)
  library(reshape2)
  library(gdata)

# READ IN AND FORMAT algae.csv FROM processed_data FOLDER-------------------------
# Reading from processed_data folder
  algae <- read.delim("processed_data/cleaned_algae_20140423.txt", as.is=TRUE, header = TRUE)
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

# Analysis of anamolous site names
  algae$lake.station <- paste(algae$lake, algae$station, sep="")  # Create new id
  anamolous <- algae[!(algae$lake.station %in% district2$lake.station), 
                     c("date", "lake.station", "hab", "sheet_id")]  # extract lake.station combo not in official list
  anamolous <- anamolous[!(substr(x=anamolous$lake.station, start=1, stop=3) %in% district3$lake),]  # remove district3
  anamolous <- anamolous[!(duplicated(anamolous$lake.station)), 
                         c("lake.station", "date", "hab", "sheet_id") ]  # Remove duplicated records
  anamolous <- anamolous[order(anamolous$lake.station),]  # Order

# Strip leading and trailing spaces in taxa  
  algae$taxa <- gsub("^\\s+|\\s+$", "", algae$taxa)
  unique(algae$taxa)  # Good
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
    clas <- read.delim('originalData/algae/Classification_EFR Phyto leu.txt', header = TRUE, fill = TRUE,
                   na.strings=c('', 'NA'), sep='\t', as.is = TRUE)  # read.delim works while read.table doesn't.  Related to quote specification (http://stackoverflow.com/questions/3016333/r-why-does-read-table-stop-reading-a-file)
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
  ld.algae[ld.algae$hab == TRUE & is.na(ld.algae$class), "class"] <- "Blue-green"  # Some hab data didn't have the class field populated
  needBio <- unique(ld.algae[ld.algae$hab == TRUE, c("lake", "rdate", "taxa")])  # Pull out unique taxa per lake x date.  Doesn't account for depth x station
  needBio <- needBio[!apply(needBio, FUN=function(x) all(is.na(x)), MARGIN=1),]  # Eliminate rows with all NAs
  needBioEFR <- unique(needBio[needBio$lake == "EFR", "taxa"])  # Pull out taxa from EFR that need biovolume
  bioSourceEFR <- ld.algae[ld.algae$lake == "EFR" &
                             (ld.algae$taxa %in% needBioEFR) & 
                             ld.algae$hab != TRUE & 
                             !is.na(ld.algae$taxa), ]  # Data for taxa that have biovolume


# Revisit after issue #26 has been resolved.
  for (i in 1:length(unique(bioSourceEFR$taxa))) { 
    pdf(file = paste("output/", "EFR.", unique(bioSourceEFR$taxa)[i], ".pdf", sep=""))
    try(print(
      ggplot(bioSourceEFR[bioSourceEFR$taxa == unique(bioSourceEFR$taxa)[i],], aes(rdate, Bio.per.cell)) + 
        geom_point() + 
        ylab("Biovolume per cell (um3)") +
        ggtitle(paste("EFR:", unique(bioSourceEFR$taxa)[i]))),
        silent=TRUE
    )
    dev.off()
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
  chem$station <- substr(chem$ID, start=5, stop=nchar(chem$ID)-15)
  chem[!is.na(chem$station), c('ID', 'lake', 'station')]  # Some of these have seem to be missing digit for date

# Inspect depth
  table(chem$sample_depth)  # Looks good

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



