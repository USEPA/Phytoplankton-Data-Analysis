### script to clean algae file.  Remove non-uniques
library(plyr)
library(stringr)
library(XLConnect)

#algae_dat <- read.table(file="processed_data/algae_20140325.csv", sep = ",", header = TRUE, fill = TRUE, as.is = TRUE)
algae_dat <- read.delim(file="processed_data/algae_20140610.csv", sep = "\t", header = TRUE, fill = TRUE, as.is = TRUE, comment.char="")

## add check

ii <- algae_dat$taxa == "No cyanobacteria observed"
sum(ii,na.rm = TRUE)
ii <- is.na(algae_dat$taxa)


dim(algae_dat)

# x <- nchar( algae_dat$taxa)
# 
# head( algae_dat[ order(x, decreasing=TRUE), ])
# 
# #x <- unique(algae_dat$taxa)
# test <<- algae_dat[ 99475:99480, ]
# str_replace_all(test$taxa, "[^[:space:]^[:alnum:]^[#]]+", "") 
# 
# algae_dat$taxa <- str_replace_all(algae_dat$taxa, "[^[:space:]^[:alnum:]^[#]]+", "")  ### strip non


# length(unique(algae_dat$taxa))
#z <- data.frame( old = x, new = y)

info <- read.table(file="processed_data/summaryStatus_20140508.csv", sep = ",", header = TRUE, fill = TRUE, as.is = TRUE)
info$sheet_id <- as.numeric(info$sheet_id)

numClass <- c("depth_ft", "sheet_id", "cell_per_l", "BV.um3.L")

for( i in 1:4){
  
  algae_dat[, numClass[i]] <- as.numeric(algae_dat[, numClass[i]])
}


algae_dat$qual_replicate <- NA

### add replicate column to file
### conceptually, this will be a qualifier column to indicate a file is a replicate using the character "R".
### check for NA

f <- function(x) {sum(is.na(x))}
apply(algae_dat, 2, f)  ### sheet_id should not be missing.

unique_taxa <- unique( algae_dat$taxa)
length(unique_taxa)

## id unique records

### conceptually, this should be a unique record
id1 <- paste(algae_dat$ID, algae_dat$taxa ) 

algae_dat$id1 <- as.numeric(factor(id1))
  length(unique(id1) )

### and should equal this 
id3 <- paste(algae_dat$ID, algae_dat$taxa, round(algae_dat$cell_per_l), round(algae_dat$BV.um3.L ) )
length(unique(id3) )
### if id3 are the same, we will assume they 
algae_dat$id3 <- as.numeric(factor(id3))

id <- match(unique(algae_dat$id3), table= algae_dat$id3)
algae_dat <- algae_dat[id, ]

### dup rows

indx <- aggregate( ID ~ id1, algae_dat, length)
indx_gt1 <- subset(indx, ID > 1)


ii <- algae_dat$id1  %in% indx_gt1$id1

### big differences, some sheets recording wrong column. Code checked
### note !ii are unambiguous records
algae_good <- algae_dat[!ii, ]

test <- algae_dat[ii, ]
test <- test[order(test$id1 ), ]

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

### check date 

### check length of tax strings

i <- nchar(algae_good$taxa) > 50



algae_good$lake<- sub(pattern="grr",replacement="GRR", x=algae_good$lake)


write.table( algae_good[,-c(13,14) ], paste("processed_data/cleaned_algae_", format(Sys.time(), "%Y%m%d"), ".txt", sep = ""), sep="\t", row.names=FALSE)


algae <- read.delim("processed_data/cleaned_algae_20140610.txt", as.is=TRUE, header = TRUE)

sumStat <- read.csv("processed_data/summaryStatus_20140610.csv", header = TRUE)

dim(check2)


