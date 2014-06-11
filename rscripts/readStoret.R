# STORET DATA----------------------
storet <- read.delim("originalData/algae/EFR Phytoplankton Data/storet/Data_JJB_20140512_091843_RegResults.txt",
                     sep="\t", na.strings="", as.is=TRUE)
str(storet)
unique(storet$State)
storet <- storet[storet$State != "ILLINOIS",]
#unique(storet$Station.ID)
#storet$lake <- substr(storet$Station.ID, 2,4)

sum(unique(algae$lake) %in% unique(storet$lake) )

table(storet$Result.Value.Status )
### compare names

x <- sub(",", replacement="", storet$Result.Value.as.Text )
storet$result <- as.numeric(x)

i <- is.na(storet$result)
### not detected recored as -999
storet$result[i] <- -999


unique(storet$Result.Value.as.Text[i] )

unique(district2$lake) %in% unique(substr(storet$Station.ID,2,4))  # All LD lakes represented
storet$Lake <- substr(storet$Station.ID,2,4)  # Create lake vector
storet <- storet[storet$Lake %in% unique(district2$lake), ]  # Extract LD lakes
length(storet$Station.ID)
storet$Date <- as.Date(substr(storet$Activity.Start, 1, 10), format = "%Y-%m-%d")
summary(storet$Date)
table(storet$Date)

####

unique(algae$lake)[ unique(algae$lake) %in% unique(storet$lake)]
unique(algae$lake)[ unique(algae$lake) %in% as.character(unique(wqOld$lake) ) ]


storet$yr <- format(storet$Date, "%Y")
table(storet$yr, storet$lake)

write.table(storet, "processed_data/storet.060514.csv", sep = ",",row.names=FALSE)




### crashes
### xxx <- dcast(storet, Station.ID + Date ~ Characteristic.Name )
library(data.table)

DT <- data.table(storet)
setkey(DT,"Station.ID", "Date")


sss=system.time(
  ss <- dcast.data.table( DT, Station.ID + Date ~ Characteristic.Name, length, value.var = "result")
  
  ); sss

