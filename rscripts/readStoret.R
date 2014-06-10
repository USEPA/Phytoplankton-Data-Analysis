# STORET DATA----------------------
storet <- read.delim("originalData/algae/EFR Phytoplankton Data/storet/Data_JJB_20140512_091843_RegResults.txt",
                     sep="\t", na.strings="", as.is=TRUE)
str(storet)
unique(storet$State)
storet <- storet[storet$State != "ILLINOIS",]
unique(storet$Station.ID)
storet$lake <- substr(storet$Station.ID, 2,4)

sum(unique(algae$lake) %in% unique(storet$lake) )

### compare names





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

write.table(storet, "processed_data/storet.060514.csv", sep = ",",row.names=FALSE)


storet$yr <- format(storet$Date, "%Y")
table(storet$yr, storet$lake)
