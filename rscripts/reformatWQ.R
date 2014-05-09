#### reformat WQ data

xx <- read.table("processed_data/water_quality.csv", sep = "\t", header = TRUE, as.is=TRUE)
xx$lake <- xx$station <- NA
ii <- nchar(xx$location) == 9

unique(xx$location[ii])

xx$lake[ii] <- substr(xx$location, 2,4 )
xx$station[ii] <- substr(xx$location[ii], 5,9 )

temp <- as.numeric(xx$location)
ii <- !is.na(temp)
xx$station[ii] <- xx$location[ii]
head(xx[ii,] )
iii <- nchar(xx$ID) == 24
xx$location <- substr(xx$ID, 2,4)


wq <- merge(xx, all, all=TRUE)

ii <- wq$location == "" | is.na(wq$location)
wq <- wq[!ii, ]

ii <- is.na(wq$result)
wq <- wq[!ii, ]

wq$analyte <- sub(" +$", "", wq$analyte)
wq$analyte <- sub(" \\+ ", "\\+", wq$analyte)

write.table( wq, paste("processed_data/combined_wq_", format(Sys.time(), "%Y%m%d"), ".txt", sep = ""), sep="\t", row.names=FALSE)