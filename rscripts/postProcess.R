#### Will Barnett, October 2014
#### UTF-8 encoding.
#### A few post-processing things to take care of.


## Get rid of certain letters in the algae taxonomic names.
uniqueTaxa <- unique(algae$taxa)
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
  algae$taxa <- gsub(pattern = changeTaxa$bad[i], 
                     replacement = changeTaxa$good[i], 
                     x = algae$taxa)
}
print("Replaced any special characters.")


## Get rid of records with 'NA' taxa names.
nNA <- sum(is.na(algae$taxa))
if(length(nNA)>0){
  algae <- subset(algae, !is.na(algae$taxa))
  print(paste("Censored", nNA,
              "records with NA in taxa field", sep=" ")
}


## Check for duplicates.
dupCols <- c("lake","date","taxa","cell_per_l","BV.um3.L")
dupCheck <- apply(algae[,dupCols],1,paste,collapse="")
dupInds <- which(duplicated(dupCheck))
dupDf <- data.frame("dup"=dupInds,
                    "checked"=FALSE)
while(any(dupDf$checked==FALSE)){
  tmpInd <- min(dupDf$dup)
  tmpTaxa <- algae$taxa[tmpInd]
  tmpSub <- subset(algae, taxa == algae$taxa[tmpInd] &
                     cell_per_l == algae$cell_per_l[tmpInd] &
                     BV.um3.L == algae$BV.um3.L[tmpInd] &
                     lake == algae$lake[tmpInd] &
                     date == algae$date[tmpInd])
}