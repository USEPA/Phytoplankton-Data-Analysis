################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in 
#### the Thomason DASLER data provided by Nathan Smucker in April 2016
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset the 'OUT' data frame
id_td <- which(grepl("Thomason_DASLER", OUT$full_file_name) & !OUT$processed)
OUTtd <- OUT[id_td,]
OUT$processed[id_td] <- TRUE
OUT$script[id_td] <- "readThomasonDASLER.R"

## Grab the right DASLER data files
## One file is only examples of duplicates that Nate provided.
## The original 2004-2015 file has columns shifted in weird places. 
## Nathan Smucker fixed these in early September 2016, and supplied
## another 'corrected' file for these data.
algae <- NULL
uniqueFiles <- unique(OUTtd$full_file_name)
uniqueFiles <- uniqueFiles[grepl("1987",uniqueFiles) | 
                             grepl("CORRECTED",uniqueFiles)]

## Read in data
for( i in 1:length(uniqueFiles)){
  # i = 2
  wb <- uniqueFiles[i]
  err <- try( excel_sheets(uniqueFiles[i]) )
  if(class(err) == "try-error"){ print("Error")} else {
    fn <- strsplit(wb, "/")[[1]][2]
    datTmp <- read_excel(wb, sheet = 1)
    
    # Fix DENSITY column where units are #/ml
    if("#/ml" %in% unique(datTmp$DENSITY_UNITS)){
      datTmp$DENSITY <- ifelse(datTmp$DENSITY_UNITS %in% c("#/l",NA),
                               datTmp$DENSITY, datTmp$DENSITY * 1000)
    }
    # Fix BIOVOLUME column where units are um3/mL
    if("um3/mL" %in% unique(datTmp$BV_UNITS)){
      datTmp$BIOVOLUME <- ifelse(datTmp$BV_UNITS %in% c("um3/mL"),
                               datTmp$BIOVOLUME*1000, datTmp$BIOVOLUME)
    }
    
    # Look for possible duplicates - these have been flagged by Nate Smucker
    if("possibleduplicate" %in% names(datTmp) & any(datTmp$possibleduplicate == 2)){
      dups <- subset(datTmp, possibleduplicate == 2)
      notDups <- subset(datTmp, is.na(possibleduplicate))
      dups$shortid <- paste0(substr(dups$Sample_ID, 1, 17),
                             substr(dups$Sample_ID, 22, 24),
                             dups$SCI_NAME)
      dupsAvg <- plyr::ddply(dups, ("shortid"), .fun = function(x){
        return(data.frame("MNBV" = mean(x$BIOVOLUME)))
      })
      dupsMerge <- merge(dups, dupsAvg, all.x = TRUE)
      dupsMerge$BIOVOLUME <- dupsMerge$MNBV
      dups <- subset(dupsMerge, duplicated(shortid),
                     select = -c(MNBV,shortid))
      datTmp <- rbind(dups, notDups)
    }
    
    
    # Some biovolumes are really low numbers, and are clearly wrong.
    datTmp$BIOVOLUME <- ifelse(!is.na(datTmp$DENSITY) & datTmp$BIOVOLUME <= 10,
                               NA, datTmp$BIOVOLUME)
    
    # ID column for substr'ing
    idTmp <- datTmp$Sample_ID
    
    # Assign HAB status based off the 'SAMPLE_TYPE' column
    habCol <- ifelse(datTmp$SAMPLE_TYPE == "HAB", TRUE, FALSE)
    
    
    algaeTmp <- data.frame(ID = idTmp,
                           lake = substr(idTmp, 2,4),
                           station = substr(idTmp, 5,9),
                           depth_ft = substr(idTmp,22,24),
                           date = substr(idTmp, start=10, stop=17),
                           taxa = datTmp$SCI_NAME,
                           cell_per_l = as.numeric(datTmp$DENSITY),
                           BV.um3.L = as.numeric(datTmp$BIOVOLUME),
                           class = NA,
                           hab = habCol,
                           sheet_id = OUTtd$sheet_id[OUTtd$full_file_name == wb])
  }
  algae <- rbind(algae,algaeTmp)
  print(nrow(algae))
  print(i)
}

## Add quality rep column as NA
algae$qual_replicate <- NA

## Append results to the algae file
setwd(homeDir)
write.table(algae, "processed_data/algae.csv", sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)

