#### script to clean algae file.
## Mostly, this involves removing non-uniques and make sure the site names 
## are formatted in a consistent manner
library(plyr)
library(stringr)
library(XLConnect)


algae_dat <- read.delim(file="processed_data/algae_20150126.csv", sep = "\t", header = TRUE, fill = TRUE, as.is = TRUE, comment.char="")

## add check
ii <- algae_dat$taxa == "No cyanobacteria observed"
sum(ii,na.rm = TRUE)


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


info <- read.table(file="processed_data/summaryStatus_20150126.csv", sep = ",", header = TRUE, fill = TRUE, as.is = TRUE)
info$sheet_id <- as.numeric(info$sheet_id)

numClass <- c("depth_ft", "sheet_id", "cell_per_l", "BV.um3.L")

for( i in 1:4){
  
  algae_dat[, numClass[i]] <- as.numeric(algae_dat[, numClass[i]])
}


## A few site names fixes
algae_dat$lake<- sub(pattern="grr",replacement="GRR", x=algae_dat$lake)
## Correct site names wherever necessary
wb <- loadWorkbook("meta_info/Corrected Site Names.xlsx")
nms <- readWorksheet(wb, sheet = "Algae")
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
table(nchar(noCorSiteNm$station)) # Lots of station names with 5 digits not on the list...
## Correct the ID where applicable.
algae_dat$CorrID <- ifelse(is.na(algae_dat$corrlakestation),
                       algae_dat$ID,
                       paste(substr(algae_dat$ID,1,1),algae_dat$corrlakestation,
                             substr(algae_dat$ID,10,24),
                             sep=""))
algae_dat$CorrStation <- ifelse(is.na(algae_dat$corrlakestation),
                            algae_dat$station,
                            substr(algae_dat$corrlakestation,4,9))


### Add replicate column to data frame
### This will be a qualifier column to indicate a record is a replicate using the character "R".
### check for NA
algae_dat$qual_replicate <- NA

## Check columns for NA values
f <- function(x) {sum(is.na(x))}
apply(algae_dat, 2, f)  ### sheet_id should not be missing.

## Attempt to ID unique records
id1 <- paste(algae_dat$ID, algae_dat$taxa ) 
algae_dat$id1 <- as.numeric(factor(id1))
  length(unique(id1) )
id3 <- paste(algae_dat$ID, algae_dat$taxa, round(algae_dat$cell_per_l), round(algae_dat$BV.um3.L ) )
## if id3 is the same for more than one record, we will assume they are duplicates
algae_dat$id3 <- as.numeric(factor(id3))
id <- match(unique(algae_dat$id3), table= algae_dat$id3)
algae_dat <- algae_dat[id, ]

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

### dup rows

indx <- aggregate( ID ~ id1, algae_dat, length)
indx_gt1 <- subset(indx, ID > 1)


ii <- algae_dat$id1  %in% indx_gt1$id1

### big differences, some sheets recording wrong column. Code checked
### note !ii are unambiguous records
algae_good <- algae_dat[!ii, ]

test <- algae_dat[ii, ]
test <- test[order(test$id1 ), ]

### clean up records with ID greater than 24 characters.

if(FALSE){
  i <- nchar(algae_good$ID) != 24
  
  ### recode 2NONAME1 as 99999
  iii <- grep("2NONAME2", algae_good$ID)
  algae_good$ID <- gsub("2NONAME2", replacement="99999", algae_good$ID )
  algae_good$station[iii] <- "2NONAME2"
  
  
  iii <- grep("2NONAME3", algae_good$ID)
  algae_good$ID <- gsub("2NONAME3", replacement="99999", algae_good$ID )
  algae_good$station[iii] <- "2NONAME3"
  
  iii <- grep("2NONAME4", algae_good$ID)
  algae_good$ID <- gsub("2NONAME4", replacement="99999", algae_good$ID )
  algae_good$station[iii] <- "2NONAME4"
  
  # iii <- grep("2NONAME10", algae_good$ID)
  # algae_good$ID <- gsub("2NONAME10", replacement="99999", algae_good$ID )
  # algae_good$station[iii] <- "2NONAME10"
  
  iii <- grep("2NONAME1", algae_good$ID)
  algae_good$ID <- gsub("2NONAME1", replacement="99999", algae_good$ID )
  algae_good$station[iii] <- "2NONAME1"
  
  
  ii <- nchar(algae_good$ID) == 25 
  algae_good$ID[ii] <- paste(substr(algae_good$ID[ii],1,10  ),substr(algae_good$ID[ii], 12,25 ) , sep = "")
  algae_good$station[ii] <-  "2NONAME10"
  
  
  algae_good$ID <- gsub("CAMPBEACH", replacement="99999", algae_good$ID )
  algae_good$ID <- gsub("Campground Beach", replacement="99999", algae_good$ID )
  algae_good$ID <- gsub("Algae Bloom Sample", replacement="99999", algae_good$ID )
  
  
  algae_good$depth_ft[i] <-  substr(algae_good$ID[i], 22, 24)
}



#test2 <- subset(test, !sheet_id %in% c(614, 594))
### write function to determine replicate 

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

## if all records come from one sheet, assume they are replicates

ii <- check1$nsheets == 1
###
head(check1)

check1 <- check1[order(abs(check1$log_diff_cell_min)), ]
check1 <- check1[order(abs(check1$log_diff_bv)), ]

iii <- algae_dat$id1 %in% check1$id1[ii]
temp <- algae_dat[iii,]
temp$qual_replicate <- "R"
algae_good <- rbind(algae_good, temp)


check1sub <- subset(check1, nsheets > 1 )  ### check records from different sheets.  Possibly suspect

### mark all as Q, from different sheets, check.

iii <- algae_dat$id1 %in% check1sub$id1
temp <- algae_dat[iii,]
temp$qual_replicate <- "Q"
algae_good <- rbind(algae_good, temp)

### remove empty taxa, issue 38
i <- is.na(algae_good$taxa)

### check length of tax strings

i <- nchar(algae_good$taxa) > 50
algae_good<- algae_good[!i, ]


## Grab the corrected columns
keepCols <- c("CorrID","lake","CorrStation","depth_ft","date","taxa","cell_per_l",
              "BV.um3.L","class","hab","sheet_id","qual_replicate")
algae_good <- algae_good[,keepCols]
names(algae_good)[which(names(algae_good) %in% c("CorrID","CorrStation"))] <- c("ID","station")

## Look at any additional duplicates
sum(duplicated(algae_good))
algae_good <- subset(algae_good, !duplicated(algae_good))


## Write cleaned algae file.
write.table( algae_good, paste("processed_data/cleaned_algae_", format(Sys.time(), "%Y%m%d"), ".csv", sep = ""), row.names=FALSE)


