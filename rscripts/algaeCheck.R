### script to clean algae file.  Remove non-uniques

algae_dat <- read.table(file="processed_data/algae_20140307.csv", sep = ",", header = TRUE, fill = TRUE)

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
algae_dat$id1 <- id1 <- paste(algae_dat$ID, algae_dat$taxa )
length(unique(id1) )

### and should equal this 
id2 <- paste(algae_dat$ID, algae_dat$taxa, algae_dat$cell_per_l )
length(unique(id2) )

### and should equal this 
id3 <- paste(algae_dat$ID, algae_dat$taxa, algae_dat$cell_per_l, algae_dat$BV.um3.L )
length(unique(id3) )

### dup rows

indx <- aggregate( ID ~ id1, algae_dat, length)
indx_gt1 <- subset(indx, ID > 1)

ii <- algae_dat$id1  %in% indx_gt1$id1

test <- algae_dat[ii, ]
test <- test[order(test$id1 ), ]


test2 <- subset(test, !sheet_id %in% c(614, 594))

test3 <- subset(test2, !sheet_id %in% c(1144))

test4 <- subset(test3, !sheet_id %in% c(22))
for(j in 1:12){
print(test4[1,j]== test4[2,j] )
}

test5 <- subset(test4, !sheet_id %in% c(23))

## these records are possibly the same
test6 <- subset(test5, !sheet_id %in% c(1171, 1143))


