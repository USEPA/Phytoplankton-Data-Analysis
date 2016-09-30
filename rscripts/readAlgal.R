################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with Algal in the name
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
## Look for 'Algal' in sheet name
id <- grepl("^Algal", OUT$sheetNames)
## Exclude a few groups that are dealt with in separate scripts
id1 <- grepl("jade042414", OUT$full_file_name)
id2 <- grepl("2014Phyto", OUT$full_file_name)
id3 <- grepl("2012Phyto", OUT$full_file_name)
## Do the subset
OUTsub2 <- OUT[(id & !(id1 | id2 | id3))  & !OUT$processed, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readAlgal.R"


## Loop through files
files <- unique(OUTsub2$full_file_name)
for (j in 1:length(files)) { # j = 2
  err <-    try(excel_sheets(files[j]) )
  if(class(err) == "try-error"){print( "File Missing") }else {
    tmpAlg <- NULL
    wb <- files[j]
    shortList <- subset(OUTsub2, full_file_name == wb )
    sheets <- shortList$sheet
    for(i in 1:length(sheets)){ # i = 4
      temp <- read_excel(wb, sheet=sheets[i], skip=8)
      temp <- temp[!is.na(temp$Division), ]
      if(ncol(temp) < 8 ){
        warning("Check Dimensions")
      }else if(ncol(temp) == 8){
        names(temp) <- c("Div", "Gen", "Syn", "Conc", 
                         "Rel", "Mean", "Tot", "Rel")
      }else if(ncol(temp) > 8){
        if(grepl("Concentration", names(temp)[4]) & 
           grepl("Relative",names(temp)[6]) &
           is.na(names(temp)[5])){
          temp <- temp[,-5]
        }
        if(ncol(temp) == 8){
          names(temp) <- c("Div", "Gen", "Syn", "Conc", 
                           "Rel", "Mean", "Tot", "Rel")
        }else if(ncol(temp) > 8) error("More than 8 original columns - FIX")
      }
      temp$sample_id <- unlist(read_excel(wb, sheet=sheets[i], skip = 4, col_names = FALSE)[1,2]) 
      temp$sheet_id <- shortList$sheet_id[i]
      if(i == 1){
        tmpAlg <- temp 
      }else{
        tmpAlg <- rbind( tmpAlg, temp) }
    } ## close for
    if (j == 1){ alg <- tmpAlg}else{
      alg <-rbind(alg, tmpAlg)
    }
  } ## close else
}  ## close j


## Fix some issues.
# Wherever the sample_id > 24 characters, there appear to be issues
# with both the station and reversed date fields
with(alg, table(nchar(sample_id)))
id27 <- subset(alg, nchar(sample_id) == 27)
substr(id27$sample_id, 5, 12) # All '2NONAMEX'
id27$sample_id <- paste0(substr(id27$sample_id, 1, 4),
                         "99999",
                         substr(id27$sample_id,17,20),
                         substr(id27$sample_id,13,16),
                         substr(id27$sample_id,21,27))
id28 <- subset(alg, nchar(sample_id) == 28)
id28$sample_id <- paste0(substr(id28$sample_id, 1, 4),
                         "99999",
                         substr(id28$sample_id,18,21),
                         substr(id28$sample_id,14,17),
                         substr(id28$sample_id,22,28))
id24 <- subset(alg, nchar(sample_id) == 24)

AAA <- rbind(id27, id28, id24)

# Still some backward dates
bDates <- subset(AAA,substr(AAA$sample_id, 10,10)!="2")
bDates$sample_id
# Some IDs have the time before the date, in the form
# 1300 2009 0806. SRR has this format.
# Some IDs have the month and day before the time, in the form
# 0629 2011, then the time of day and depth in the correct places.
# RRR, MSR, MNR, HTR, BRR, GRR
for(i in 1:nrow(bDates)){ # i = 1
  tmpId <- bDates$sample_id[i]
  tmpStation <- substr(tmpId, 2,4)
  if(tmpStation == "SRR"){
    newId <- paste0(substr(tmpId, 1, 9),
                   substr(tmpId, 14,21),
                   substr(tmpId, 10, 13),
                   substr(tmpId, 22, 24))
    bDates$sample_id[i] <- newId
  }else if(tmpStation %in% c("RRR", "MSR", "MNR", "HTR", "BRR", "GRR")){
    newId <- paste0(substr(tmpId, 1, 9),
                    substr(tmpId, 14,17),
                    substr(tmpId, 10, 13),
                    substr(tmpId, 18, 24))
    bDates$sample_id[i] <- newId
  }else {
    print(paste0("Check ", i, "th iteration - unrecognized station"))
  }
}
# Good dates
gDates <- subset(AAA, substr(AAA$sample_id, 10,10)=="2")

AAA <- rbind(gDates, bDates)

algae <- data.frame(ID = AAA$sample_id,
                     lake = substr(AAA$sample_id, 2,4),
                     station = substr(AAA$sample_id, 5, 9),
                     depth_ft = substr(AAA$sample_id, 22, 24),
                     date = substr(AAA$sample_id, 10,17),
                     taxa = AAA$Gen,
                      cell_per_l = AAA$Conc,
                     BV.um3.L = AAA$Tot,  
                     class = NA,
                     hab = FALSE,
                     sheet_id = AAA$sheet_id)



chunck_check(algae)

setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
}
