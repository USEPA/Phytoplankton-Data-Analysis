#### Will Barnett, October 2014
#### Checking all the action items from the early October call.
#### UTF-8 encoding.


## Set working directory
setwd("/Users/wbarnett/Documents/Neptune/svn/EPA-ORD/trunk/2013/TO 0016/analysis/Phytoplankton-Data-Analysis")


## Read algae data
algae <- read.csv("processed_data/algae.csv", sep="\t")
uniqueTaxa <- unique(algae$taxa)
write.csv(uniqueTaxa,"output/UniqueTaxaNames2014_10_28.csv")


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


## Get rid of beginning or ending white spaces.
## Get rid of "\n"
