## check committed cleaned_algae data

check2 <- read.delim("processed_data/cleaned_algae_20140402.txt", as.is=TRUE, header = TRUE)

dim(check2)

str(check2)

addmargins(table(check2$lake, useNA="always") )

### date check

check2$date <- as.Date(as.character(check2$date), format="%Y%m%d")

check2$year <- format(check2$date, format = "%Y")


range(temp)

X<- addmargins(table(check2$taxa, useNA="always") )
