################
#### Will Barnett, August 2016
################


################
#### This script cleans the algae file.
#### It attempts to remove non-unique records,
#### corrects site names where necessary,
#### assigns taxa names and imputes biovolume measurements
#### where applicable.
################


## Script is called from masterScript.R
## Assumes the working directory is at the .Rproj level


## Libraries
library(plyr)
library(stringr)
library(readxl)
library(openxlsx)

## Read in the combined algae data
## If this is being run from masterScript.R, the path is in 'xx'
algae_dat <- read.csv(xx, header = TRUE)


## Get rid of certain letters in the algae taxonomic names.
uniqueTaxa <- unique(algae_dat$taxa)
badLetters <- c('á','a',
                'ü','u',
                'ä','a',
                'é','e',
                'ë','e',
                'ö','o',
                'ń','n',
                'è','e')
changeTaxa <- data.frame(matrix(badLetters,ncol=2,byrow=TRUE))
names(changeTaxa) = c("bad","good")
for(i in 1:nrow(changeTaxa)){ # i = 1
  algae_dat$taxa <- gsub(pattern = changeTaxa$bad[i], 
                         replacement = changeTaxa$good[i], 
                         x = algae_dat$taxa)
}


## Get rid of records with 'NA' taxa names.
nNA <- sum(is.na(algae_dat$taxa))
if(nNA>0){
  algae_dat <- subset(algae_dat, !is.na(algae_dat$taxa))
  print(paste("Censored", nNA,
              "records with NA in taxa field", sep=" "))
}


## Make sure certain columns are numeric
numClass <- c("depth_ft", "sheet_id", "cell_per_l", "BV.um3.L")

for( i in 1:4){
  algae_dat[, numClass[i]] <- as.numeric(algae_dat[, numClass[i]])
}


## A few site names fixes
algae_dat$lake<- sub(pattern="grr",replacement="GRR", x=algae_dat$lake)
## Correct site names wherever necessary
wb <- "meta_info/Corrected Site Names.xlsx"
nms <- read_excel(wb, sheet = "Algae")
## Need to re-format the original site names.
original <- NULL
for(i in 1:nrow(nms)){ # i = 1
  tmp <- gsub(pattern = "\\", replacement = "", x = unlist(strsplit(nms[i,1],split = " "))[1:3],
              fixed=TRUE)[2]
  original <- c(original, str_replace_all(tmp,"[^[:alnum:]]",""))
}
nms$Original <- original
nms <- nms[,c("Original","Corrected")]
nms$CorrectedNo2 <- substr(nms$Corrected,2,9)
algae_dat$origlakestation <- paste(algae_dat$lake,algae_dat$station,sep="")
## Some records already have the correct lake and station identifications
algae_dat$corrlakestation <- ifelse(algae_dat$origlakestation %in% nms$CorrectedNo2,
                                    algae_dat$origlakestation, NA)
sum(is.na(algae_dat$corrlakestation))
## Other records might have an original lake and station from the Corrected Site
## Names file
algae_dat$corrlakestation <- ifelse(is.na(algae_dat$corrlakestation),
                                    nms$CorrectedNo2[match(algae_dat$origlakestation,
                                                           nms$Original)],
                                    algae_dat$corrlakestation)
sum(is.na(algae_dat$corrlakestation)) 
## Look at NAs
noCorSiteNm <- subset(algae_dat, is.na(corrlakestation))
table(nchar(noCorSiteNm$lake))
table(nchar(noCorSiteNm$station)) 
## Correct the ID where applicable.
algae_dat$CorrID <- ifelse(is.na(algae_dat$corrlakestation),
                           algae_dat$ID,
                           paste(substr(algae_dat$ID,1,1),algae_dat$corrlakestation,
                                 substr(algae_dat$ID,10,24),
                                 sep=""))
algae_dat$CorrStation <- ifelse(is.na(algae_dat$corrlakestation),
                                algae_dat$station,
                                substr(algae_dat$corrlakestation,4,9))


## Check columns for NA values
f <- function(x) {sum(is.na(x))}
apply(algae_dat, 2, f)
## sheet_id is missing from the records read in from readDBR.R

## Attempt to ID unique records
## Concatenate ID, taxa name, rounded Cells/L, and biovolume
id3 <- paste(algae_dat$ID, algae_dat$taxa, round(algae_dat$cell_per_l), round(algae_dat$BV.um3.L ) )
## if id3 is the same for more than one record, we will assume they are duplicates
algae_dat$id3 <- as.numeric(factor(id3)) # Turns it into a unique number
id <- match(unique(algae_dat$id3), table= algae_dat$id3)
n1 <- nrow(algae_dat)
if(length(id) < n1){
  algae_dat <- algae_dat[id, ]
  n2 <- nrow(algae_dat)
  print(paste("Removed ", n1-n2, " records - apparent duplicate entries", sep=""))
}

## It is possible to have duplicates where the only difference is the 9999 instead of a
## 0000 timestamp.
dupCols <- c("taxa","cell_per_l","BV.um3.L")
tmp1 <- apply(algae_dat[,dupCols],1,paste,collapse="")
tmp2 <- unlist(lapply(algae_dat$ID,FUN = function(x){
  paste(substr(x,1,17),substr(x,22,24),sep="")
}))

dupCheck <- apply(cbind(tmp1,tmp2),1,paste,collapse="")
algae_dat$dupcheck <- dupCheck
n1 <- nrow(algae_dat)
cleanedDups <- ddply(algae_dat, .(dupcheck), .fun = function(x){
  if(nrow(x)>1){
    tmp <- x
    tmpDepths <- substr(tmp$ID,18,21)
    tmp$dup <- NA
    tmp$dup[tmpDepths=='9999'] <- FALSE
    tmp$dup[tmpDepths=='0000'] <- TRUE
    tmp$dup[!(tmpDepths %in% c('9999','0000'))] <- FALSE
    out <- tmp[-which(tmp$dup==FALSE),-which(names(tmp) %in% c("dupcheck","dup"))]
  }else {
    out <- x
  }
  return(out)
})
n2 <- nrow(cleanedDups)
print(paste("Censored", n1-n2, "rows because of duplicate/incomplete",
            "timestamps",sep=" "))
algae_dat <- cleanedDups


## Matt's older code to check for dup rows
id1 <- paste(algae_dat$ID, algae_dat$taxa ) 
algae_dat$id1 <- as.numeric(factor(id1))
indx <- aggregate( ID ~ id1, algae_dat, length)
indx_gt1 <- subset(indx, ID > 1)
ii <- algae_dat$id1  %in% indx_gt1$id1
## Look at example
subset(algae_dat, id1 == indx_gt1$id1[1])
## Big differences in measurements.
algae_good <- algae_dat[!ii, ] ## note !ii are unambiguous records
test <- algae_dat[ii, ] ## Look at these further
test <- test[order(test$id1 ), ]

## clean up records with ID greater than 24 characters.

if(TRUE){
  i <- nchar(algae_good$ID) != 24
  algae_good$ID <- gsub("CAMPBEACH", replacement="99999", algae_good$ID )
  algae_good$ID <- gsub("Campground Beach", replacement="99999", algae_good$ID )
  algae_good$ID <- gsub("Algae Bloom Sample", replacement="99999", algae_good$ID )
  # This at least corrects the depth column for the three above station names
  algae_good$depth_ft[i] <-  as.numeric(substr(algae_good$ID[i], 22, 24))
}


## Write function to determine replicate 
f <- function(x){
  log_diff_cell <- max( abs(diff( log(x$cell_per_l) ) ) ) / mean(log(x$cell_per_l))
  if(!is.finite(log_diff_cell) )  log_diff_cell <- -777
  log_diff_bv <- max( abs(diff( log(x$BV.um3.L) ) ) )/ mean(log(x$BV.um3.L))
  if(!is.finite(log_diff_bv) )  log_diff_bv <- -777
  
  log_diff_cell_min <- min( abs(diff( log(x$cell_per_l) ) ) ) / mean(log(x$cell_per_l))
  if(!is.finite(log_diff_cell_min) )  log_diff_cell_min <- -777
  log_diff_bv_min <- min( abs(diff( log(x$BV.um3.L) ) ) )/ mean(log(x$BV.um3.L))
  if(!is.finite(log_diff_bv_min) )  log_diff_bv_min <- -777
  
  
  nSheet <- length(unique(x$sheet_id))
  nRecords <- nrow(x)
  data.frame( log_diff_cell = log_diff_cell,  log_diff_bv = log_diff_bv, 
              nsheets = nSheet, nRecords = nRecords,
              log_diff_cell_min = log_diff_cell_min,
              log_diff_bv_min = log_diff_bv_min,
              max_nchar_station = max(nchar(x$station))
  )  
  
}
check1 <- ddply( .data=test,.variables=c("id1", "station"), f )
head(check1)
## if all records come from one sheet, assume they are replicate entries - mark as 'R'
ii <- check1$nsheets == 1
check1 <- check1[order(abs(check1$log_diff_cell_min)), ]
check1 <- check1[order(abs(check1$log_diff_bv)), ]
iii <- algae_dat$id1 %in% check1$id1[ii]
temp <- algae_dat[iii,]
temp$qual_replicate <- "R"
algae_good <- rbind(algae_good, temp)


## If from different sheets they might be quality (field) duplicates
## Mark as Q
check1sub <- subset(check1, nsheets > 1 )  ### check records from different sheets.  Possibly suspect
iii <- algae_dat$id1 %in% check1sub$id1
temp <- algae_dat[iii,]
temp$qual_replicate <- "Q"
algae_good <- rbind(algae_good, temp)


## Grab the corrected columns
keepCols <- c("CorrID","lake","CorrStation","depth_ft","date","taxa","cell_per_l",
              "BV.um3.L","class","hab","sheet_id","qual_replicate")
algae_good <- algae_good[,keepCols]
names(algae_good)[which(names(algae_good) %in% c("CorrID","CorrStation"))] <- c("ID","station")

## Look at any additional duplicates
if(sum(duplicated(algae_good)) > 0) algae_good <- subset(algae_good, !duplicated(algae_good))


## Get rid of any rows with less than 24 character ID
algae_good <- subset(algae_good, nchar(ID) ==24)



############################
#### This takes parts of PreviewProcessedDataJake.R and assigns things like
#### site names, taxonomic groups/divisions, etc.

# LIBRARIES-----------------------------------------------------
library(ggplot2)
library(reshape)
library(reshape2)
library(gdata)
library(plyr)
library(dplyr)
library(readxl)
library(openxlsx)
options(dplyr.print_max = 1e9)  # to print entire tbl_df created via dplyr package
algae <- algae_good
head(algae)
str(algae)

# Review number of samples, lakes, dates, etc
table(algae$date)  # Formatted correctly.
table(algae$lake)  # Unusual lake names from District 3
unique(algae$station)  # Includes a few non-routine sites
unique(algae$depth_ft) # A few big ones

## Make depth integer
algae$depth_ft <- as.integer(algae$depth_ft)

# Add date
algae$rdate <- as.Date(as.character(algae$date), format = "%Y%m%d")
algae$year <- as.numeric(substr(algae$date, 1,4))

# Add district and filter out non-district 2 sites
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


## Minimal processing 
# Weird values in cell_per_l
# NA first
summary(algae$cell_per_l) 
filter(algae, is.na(cell_per_l)) %>% select(BV.um3.L) %>% 
  summarize(total=sum(!is.na(BV.um3.L))) 
filter(algae, is.na(cell_per_l)) %>% select(sheet_id) # 
# <0 or 9999
filter(algae, cell_per_l < 0) %>% select(lake)  # none, which is good
filter(algae, cell_per_l == 9999) %>% select(lake, rdate, ID, sheet_id, cell_per_l, taxa)  #only one.  This is correct.  EFR 2005-05-11
# =0
filter(algae, cell_per_l == 0) %>% 
  select(lake, cell_per_l, BV.um3.L, sheet_id) %>%
  distinct(BV.um3.L) # These all have a BV of 0 or -9999.  remove
algae <- filter(algae, cell_per_l != 0)
# Weird values in BV.um3.L
# NA first
summary(algae$BV.um3.L)  #
filter(algae, is.na(BV.um3.L)) %>% select(BV.um3.L, cell_per_l, hab)  # A lot are HAB samples.
# <0 or 9999
filter(algae, BV.um3.L < 0) %>% select(lake, BV.um3.L, cell_per_l)  
filter(algae, BV.um3.L == 9999) %>% select(lake, rdate, ID, sheet_id, cell_per_l, taxa)  # none with 9999
# = 0
filter(algae, BV.um3.L == 0) %>% select(lake, sheet_id, BV.um3.L, cell_per_l)  # 284, but all have cell_per_l values.  convert to NA.
algae <- mutate(algae, BV.um3.L=replace(BV.um3.L, BV.um3.L == 0, NA))  # replace 0 with NA 
# Strip leading and trailing spaces in taxa  
algae$taxa <- gsub("^\\s+|\\s+$", "", algae$taxa)
unique(algae$taxa)[order(unique(algae$taxa))][1:700] # Good
unique(algae$class)  # Some sheets had decent class data
unique(algae$hab)  # Good, either 0 or 1, equivalent to FALSE or TRUE, respectively.
unique(algae$qual_replicate)  # Looks good, NA, Q, or R. See GitHub wiki
# All observations should include a taxa name
sum(is.na(algae$taxa))  # 0, which is good

#### Add toxonomic groups
# RECONCILE TAXONOMY AGAINST CONTRACTORS CORRECTED LIST----------------------------------------
taxa.bsa <- read.xls("originalData/algae/BSA DRAFT EXPANDED TAXA LIST V3_06_18_2015.xls", 
                     sheet = "BSA DRAFT EXPANDED TAXA LIST V3", as.is = TRUE)
# taxa.bsa <- read_excel("originalData/algae/BSA DRAFT EXPANDED TAXA LIST V3_06_18_2015.xls", 
#                      sheet = "BSA DRAFT EXPANDED TAXA LIST V3")
length(taxa.bsa$Original.Taxa.Name)  #1834 taxa

# Strip leading and trailing spaces in taxa  names
taxa.bsa$Original.Taxa.Name <- trimws(taxa.bsa$Original.Taxa.Name, "both")
taxa.bsa$Accepted.Name <- trimws(taxa.bsa$Accepted.Name, "both")


# Inspect for duplicate Original.Taxa.Name
sum(duplicated(taxa.bsa$Original.Taxa.Name))  #3
filter(taxa.bsa, duplicated(taxa.bsa$Original.Taxa.Name) | # view specifics
         duplicated(taxa.bsa$Original.Taxa.Name, fromLast = TRUE)) %>%
  select(Original.Taxa.Name, Accepted.Name)
taxa.bsa <- filter(taxa.bsa, !duplicated(Original.Taxa.Name))

# Repetitive columns in BSA file, but all are identical
sum(taxa.bsa$Genera != taxa.bsa$Genera.1)

# Are all unique taxa names in BSA's list
sum(!(unique(algae$taxa) %in% taxa.bsa$Original.Taxa.Name))  
# With addition of Thomason DASLER data, and 1988 / 2012 / 2014 files,
# the unique taxa names in the BSA list are no longer comprehensive.
# This is addressed with some logic below.
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
  distinct(taxa)  # pull out unique. 

filter(algae, !(algae$taxa %in% taxa.bsa$Original.Taxa.Name)) %>% # not in BSA list
  select(year) %>% # pull out sheet_id
  distinct(year)

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

# Are all of BSA's names in the algae file?  Doesnt' matter, just checking.
sum(!(unique(taxa.bsa$Original.Taxa.Name) %in% algae$taxa))  # 89 not in filtered algae file
filter(taxa.bsa, !(taxa.bsa$Original.Taxa.Name %in% algae$taxa)) %>% # not in algae file
  select(Original.Taxa.Name) %>% # pull out taxa
  distinct(Original.Taxa.Name)  # pull out unique.  Most, but not all, have a strange character.


## Merge
## For names included in the Original.Taxa.Name on BSA's list, 
## merge is straightforward
algae <- as.data.frame(algae)
taxa.bsa <- as.data.frame(taxa.bsa)
algae$ct <- 1:nrow(algae)
## First set has exact match between 'taxa' column in algae df and Original.Taxa.Name from BSA
algae.bsa.1 <- merge(x = subset(algae, taxa %in% taxa.bsa$Original.Taxa.Name),
                     y = subset(taxa.bsa, select = -c(X)),
                     by.x = "taxa", 
                     by.y = "Original.Taxa.Name")
## Second set has exact match between 'taxa' column in algae df and Accepted.Name from BSA
algae.bsa.2 <- merge(x = subset(algae, !(taxa %in% taxa.bsa$Original.Taxa.Name) &
                                  (taxa %in% taxa.bsa$Accepted.Name)),
                     y = subset(taxa.bsa, !duplicated(Accepted.Name), select = -c(X)),
                     by.x = "taxa", 
                     by.y = "Accepted.Name",
                     all.x = TRUE)
algae.bsa.2$Accepted.Name <- algae.bsa.2$taxa

## Third set requires some logic tricks. These data don't have a match between the 
## 'taxa' column in the algae df and either of the 'Original.Taxa.Name' or 'Accepted.Name' columns from BSA
algae.bsa.3 <- subset(algae, !(ct %in% c(algae.bsa.2$ct, algae.bsa.1$ct)))
## Logic here: 
## Loop through each row of algae.bsa.3
## Try substituting out 'Â'
## Try grepping for 'sp' in the second word, and substitute 'sp.'
## Try substituting out 'cf.'
## Lastly, Grab the first word (genus), append 'sp.', and compare to Accepted.Name from BSA
modifiedTaxa <- NULL
for(i in 1:nrow(algae.bsa.3)){
  # i = 15066
  tmpTaxa <- algae.bsa.3$taxa[i]
  # tmpTaxa <- subset(algae.bsa.3, ct == 334422)$taxa
  matchFlag <- FALSE
  if(gsub('Â', "", tmpTaxa) %in% taxa.bsa$Accepted.Name){
    modifiedTaxa <- c(modifiedTaxa,(gsub('Â', "", tmpTaxa)))
    matchFlag <- TRUE
  }
  if(!matchFlag){
    if(grepl("sp", tmpTaxa) & sapply(strsplit(tmpTaxa, "\\s+"), length) == 2 &
       gsub('sp', 'sp.', tmpTaxa) %in% taxa.bsa$Accepted.Name){
      modifiedTaxa <- c(modifiedTaxa, gsub('sp', 'sp.', tmpTaxa))
      matchFlag <- TRUE
    }
  }
  if(!matchFlag){
    if(grepl("cf.", tmpTaxa) & gsub('cf. ', '', tmpTaxa) %in% taxa.bsa$Accepted.Name){
      modifiedTaxa <- c(modifiedTaxa, gsub('cf. ', '', tmpTaxa))
      matchFlag <- TRUE
    }
  }
  if(!matchFlag){
    if(grepl('Â', tmpTaxa) & sapply(strsplit(tmpTaxa, "\\s+"), length) > 2){
      tmpNm <- gsub('Â', '', tmpTaxa)
      tmpNmShort <- paste(unlist(strsplit(tmpNm, "\\s+"))[1:2], collapse = " ")
      if(tmpNmShort %in% taxa.bsa$Accepted.Name){
        modifiedTaxa <- c(modifiedTaxa, tmpNmShort)
        matchFlag <- TRUE
      }
    }
  }
  if(!matchFlag){
    tmpGenus <- paste(unlist(strsplit(tmpTaxa, "\\s+"))[1], "sp.", sep = " ")
    if(tmpGenus %in% taxa.bsa$Accepted.Name){
      modifiedTaxa <- c(modifiedTaxa, tmpGenus)
      matchFlag <- TRUE
    }
  }
  if(!matchFlag){
    if(grepl("Pseudoanabaena", tmpTaxa) | grepl("Pseudoanabaena", tmpTaxa)){
      modifiedTaxa <- c(modifiedTaxa,"Pseudanabaena sp.")
      matchFlag <- TRUE
    }else if(grepl("Chroomonas", tmpTaxa)){
      modifiedTaxa <- c(modifiedTaxa,"Chroomoonas sp.")
      matchFlag <- TRUE
    }
  }
  # If no taxa match, try resolving to class level
  if(!matchFlag){
    modifiedTaxa <- c(modifiedTaxa, tmpTaxa)
    if( (grepl("blue", tmpTaxa) & grepl("green", tmpTaxa)) |
        (grepl("Bluegreen", tmpTaxa)) ){
      algae.bsa.3$class[i] <- "Blue-Green Algae"
    }else if(grepl("green", tmpTaxa)){
      algae.bsa.3$class[i] <- "Green Algae"
    }
  }
  print(i)
}



algae.bsa.3$taxa <- modifiedTaxa
taxa.bsa.no.dups <- taxa.bsa[!duplicated(taxa.bsa$Accepted.Name),]
algae.bsa.3.merge <- merge(x = algae.bsa.3, 
                           y = subset(taxa.bsa.no.dups, select = -c(X,Original.Taxa.Name)),
                           by.x = "taxa",
                           by.y = "Accepted.Name",
                           all.x = TRUE)
# Make sure there is an 'Accepted.Name' column
algae.bsa.3.merge$Accepted.Name <- algae.bsa.3.merge$taxa

# Some of the taxa names only resolve to class;
# Clean these and fill in Taxa.Type as necessary
with(algae.bsa.3.merge, table(class))
with(algae.bsa.3.merge, table(Taxa.Type))
algae.bsa.3.merge$class <- gsub("Blue-greens", "Blue-Green Algae", algae.bsa.3.merge$class)
algae.bsa.3.merge$class <- gsub("Diatom", "Diatoms", algae.bsa.3.merge$class)
algae.bsa.3.merge$class <- gsub("Greens", "Green Algae", algae.bsa.3.merge$class)

# Fill in Taxa.Type column wherever a 'class' entry exists
algae.bsa.3.merge$Taxa.Type <- ifelse(is.na(algae.bsa.3.merge$Taxa.Type) &
                                        !is.na(algae.bsa.3.merge$class),
                                      algae.bsa.3.merge$class,
                                      algae.bsa.3.merge$Taxa.Type)


#### Old code
# algae.bsa.3 <- merge(x = subset(algae, !(taxa %in% taxa.bsa$Original.Taxa.Name) &
#                                   !(taxa %in% taxa.bsa$Accepted.Name)),
#                      y = subset(taxa.bsa, select = -c(X)),
#                      by.x = "taxa", 
#                      by.y = "Original.Taxa.Name",
#                      all.x = TRUE)
# nrow(algae.bsa.1) + nrow(algae.bsa.2) + nrow(algae.bsa.3) == nrow(algae)
#### End


## Rbind them together.
algae.bsa <- rbind(algae.bsa.1, subset(algae.bsa.2, select = -c(Original.Taxa.Name)),
                   algae.bsa.3.merge)

## How many have no taxonomic details?
tmp <- subset(algae.bsa, is.na(Taxa.Type))
nrow(tmp)


## Add a column for toxin-producing species.
toxins <- read_excel("originalData/algae/Toxin Producing Algae Species.xlsx",
                     col_names = TRUE)
algae.bsa$toxin <- FALSE
for(tox in unique(toxins$`Microcystin Producer (Paerl&Otten 2013+)`)){
  # tox <- toxins$`Microcystin Producer (Paerl&Otten 2013+)`[1]
  inds <- grep(tox, algae.bsa$taxa)
  if(length(inds) > 0){
    algae.bsa$toxin[inds] <- TRUE
  }
}


## Missing Biovolume
# CONVERT BLUE-GREEN CELL COUNTS TO BIOVOLUME-----------------------
# First, identify all taxa that need biovolume (should be all HAB == TRUE records)
# Second, identify all examples where these taxa were found in routine monitoring
# and have biovolumes.

# Find missing biovolumes
filter(algae.bsa, BV.um3.L == -9999 | is.na((BV.um3.L))) %>% 
  select(lake) %>% 
  summarize(total = length(lake))  # 1702 total instances where BV is NA or -9999
filter(algae.bsa, (BV.um3.L == -9999 | is.na((BV.um3.L))) & is.na(cell_per_l)) %>% 
  select(lake) %>% 
  summarize(total = length(lake))  # 0, all instances where BV is NA or -9999 has cell_per_l values
# Unique lake x data x taxa combinations where BV is needed.
# Exclude Accepted.Name == "na".  These are case where species level ID was not possible, but
# typically group level (i.e., diatom, BG) was possible.
needBio <- filter(algae.bsa, (BV.um3.L == -9999 | is.na(BV.um3.L)) & Accepted.Name != "na") %>% 
  select(lake, rdate, Accepted.Name) %>%
  distinct(lake, rdate, Accepted.Name)  # 324 unique lake x date x taxa occurences

if(FALSE){
  # Full list of where BV is needed, including reported BV and cell counts,
  # This list will go to BSA
  MissingBiovolume <- filter(algae.bsa, (BV.um3.L == -9999 | is.na(BV.um3.L)) & Accepted.Name != "na") %>% 
    select(lake, rdate, Accepted.Name, BV.um3.L, cell_per_l)
  write.table(MissingBiovolume, file="output/forBSA/MissingBiovolume.txt", row.names = FALSE)
  
  # Find biosource data
  # Logical indicating which lake x taxa combinations in needBio are in the algae file
  # This goes to BSA
  bioSourceIndex <- paste(algae.bsa$lake, algae.bsa$Accepted.Name, sep="") %in%  #16060 TRUE values
    paste(needBio$lake, needBio$Accepted.Name, sep="")
  bioSource <- filter(algae.bsa, bioSourceIndex & 
                        (!is.na(BV.um3.L) & BV.um3.L != -9999 & 
                           !is.na(cell_per_l) & cell_per_l != 9999)) %>%  # 14358 observations
    mutate(Bio.per.cell = BV.um3.L / cell_per_l)  %>%
    select(Accepted.Name, lake, rdate, BV.um3.L, cell_per_l, Bio.per.cell)
  summary(bioSource$Bio.per.cell)
  write.table(bioSource, file="output/forBSA/BiovolumeSource.txt", row.names = FALSE)
  
  
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
  
  # Need to deal with instances where Accepted.Name == "na" and cell counts, but not biovolume,
  # is reported.  Provide to BSA for their inspection
  MissingBiovolume.na <- filter(algae.bsa, (BV.um3.L == -9999 | is.na(BV.um3.L)) & Accepted.Name == "na") %>% 
    select(lake, rdate, taxa, Accepted.Name, Empire, Kingdom, Phylum..Division., Class, Order, Family,
           Genera.1, Species.1, BV.um3.L, cell_per_l)
  write.table(MissingBiovolume.na, file="output/forBSA/MissingBiovolume.na.txt", row.names = FALSE)
}

# Use BSA's per cell biovolume estimates to convert cell counts to biovolume.
# Get data from originalData/algae/Missing Biovolume - OUTPUT.xlsx.  Sheet
# "Missing Biovolume - OUTPUT" contains all observations where BSA provided
# an "Accepted Name" for the taxa.  The few instances where biovolume was needed
# for a taxa where no Accepted Name could be determined (i.e. only coarse level
# taxonomic resolution was possible) are handled in sheet "Missing Biovolume NA -
# "OUTPUT".  Both sheets should be read in and merged with algae.bsa.  

bsa.bv.1 <-  read.xls("originalData/algae/Missing Biovolume - OUTPUT.xlsx", 
                      sheet = "Missing Biovolume - OUTPUT", as.is = TRUE)
bsa.bv.1$rdate <- as.Date(bsa.bv.1$Date, format ="%Y-%m-%d")
duplicated(bsa.bv.1)  # lots of dups in here.  Eliminate dups, complicates merging.
bsa.bv.1 <- filter(bsa.bv.1, !duplicated(bsa.bv.1))  
str(bsa.bv.1)

bsa.bv.2 <-  read.xls("originalData/algae/Missing Biovolume - OUTPUT.xlsx", 
                      sheet = "Missing Biovolume NA - OUTPUT", as.is = TRUE)
bsa.bv.2 <- mutate(bsa.bv.2, 
                   rdate = as.Date(bsa.bv.2$Date, format ="%Y-%m-%d")) %>%
  rename(Mean.Biovolume.cell = Mean.BV.cell, 
         cell_per_l = Density..cells.L.1.) %>%
  select(- NOTES, -Date, -cell_per_l)
bsa.bv.2 <- bsa.bv.2[!duplicated(bsa.bv.2), ] # remove duplicates.  Needed for clean merge.

str(bsa.bv.2)

# First, merge bsa.bv.1 with algae.bsa.  
algae.bsa.bv <- merge(algae.bsa, 
                      select(bsa.bv.1, -Date, -NOTES),
                      by.x = c("Accepted.Name", "lake", "rdate"),
                      by.y = c("Accepted.Name", "Lake", "rdate"),
                      all.x = TRUE)
length(algae.bsa$lake) == length(algae.bsa.bv$lake)  # TRUE, merged as expected

# Next, merge with bsa.bv.2
algae.bsa.bv2 <- merge(algae.bsa.bv, 
                       bsa.bv.2, 
                       by.x = c("taxa", "rdate", "lake"),
                       by.y = c("Taxa", "rdate", "Lake"),
                       all.x = TRUE)
length(algae.bsa$lake) == length(algae.bsa.bv2$lake)  # TRUE, merged as expected

# Merge create two Mean.Biovolume.cell columns.  Merge and clean.  
algae.bsa.bv2 <- mutate(algae.bsa.bv2, 
                        Mean.Biovolume.cell.x = ifelse(is.na(Mean.Biovolume.cell.x) & # if .x is NA
                                                         !is.na(Mean.Biovolume.cell.y), # & .y has data,
                                                       Mean.Biovolume.cell.y, # then .y
                                                       Mean.Biovolume.cell.x))  %>% # else .x
  rename(Mean.Biovolume.cell = Mean.Biovolume.cell.x)  %>%
  select(-Mean.Biovolume.cell.y)

# Check to see that NA and -9999 BV now have bv.cell
filter(algae.bsa.bv2, (is.na(BV.um3.L) | BV.um3.L == -9999) & is.na(Mean.Biovolume.cell)) # Excellent!

# Next, calculate bv from cell_l and Mean.Biovolume.cell
algae.bsa.bv2 <- mutate(algae.bsa.bv2, 
                        BV.um3.L = ifelse(is.na(BV.um3.L) | BV.um3.L == -9999,
                                          Mean.Biovolume.cell * cell_per_l,
                                          BV.um3.L))

# Check to see if we got them all. 
filter(algae.bsa.bv2, (is.na(BV.um3.L) | BV.um3.L == -9999)) # All observations how have biovolume!




## Columns to keep
cols <- names(algae.bsa.bv2)
write.csv(algae.bsa.bv2[,cols], file = paste("processed_data/Cleaned_Algae_",format(Sys.time(), "%Y%m%d"),".csv",sep=""), row.names = FALSE)


