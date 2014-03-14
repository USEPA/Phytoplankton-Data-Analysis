### script to clean algae file.  Remove non-uniques
library(plyr)

algae_dat <- read.table(file="processed_data/algae_20140314.csv", sep = ",", header = TRUE, fill = TRUE, as.is = TRUE)
info <- read.table(file="processed_data/summary.status0314.csv", sep = ",", header = TRUE, fill = TRUE, as.is = TRUE)
info$sheet_id <- as.numeric(info$sheet_id)

algae_dat <- algae_dat[ -210022, ]  ### track down row.

numClass <- c("depth_ft", "sheet_id", "cell_per_l", "BV.um3.L")

for( i in 1:4){
  
  algae_dat[, numClass[i]] <- as.numeric(algae_dat[, numClass[i]])
}

### statistics
dim(algae_dat)

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
test <- algae_dat[ii, ]
test <- test[order(test$id1 ), ]

#test2 <- subset(test, !sheet_id %in% c(614, 594))

### write function to determine replicate 
f <- function(x){
  log_diff_cell <- max( abs(diff( log(x$cell_per_l) ) ) ) / mean(log(x$cell_per_l))
  if(!is.finite(log_diff_cell) )  log_diff_cell <- -777
  log_diff_bv <- max( abs(diff( log(x$BV.um3.L) ) ) )/ mean(log(x$BV.um3.L))
  if(!is.finite(log_diff_bv) )  log_diff_bv <- -777
  nSheet <- length(unique(x$sheet_id))
  data.frame( log_diff_cell = log_diff_cell,  log_diff_bv = log_diff_bv, nsheets = nSheet )  
}

check1 <- ddply( .data=test,.variables=c("id1"), f )

check1 <- check1[order(abs(check1$log_diff_cell)), ]
check1 <- check1[order(abs(check1$log_diff_bv)), ]

check1sub <- subset(check1, nsheets > 1 )  ### check records from different sheets.  Possibly suspect

idCells <- check1$log_diff_cell== 0
idBV <- check1$log_diff_bv == 0



subset(test, id1 == 33496)
subset(info, sheet_id == 1144)

### 1144 seem like a legitimate replicate.
test3 <- subset(test2, !sheet_id %in% c(1144))

test4 <- subset(test3, !sheet_id %in% c(22))
for(j in 1:12){
print(test4[1,j]== test4[2,j] )
}

test5 <- subset(test4, !sheet_id %in% c(23))

## these records are possibly the same
test6 <- subset(test5, !sheet_id %in% c(1171, 1143))


