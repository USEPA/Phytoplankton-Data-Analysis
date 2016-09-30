################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in HAB files
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)


## Subset data
id <- grepl("^HAB", OUT$file)
id2 <- grepl("Results", OUT$sheet) ### only record results
OUT$skip[id & !id2] <- "Only read results, not summary or details"
OUTsub2 <- OUT[id & id2, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readHAB.R"
#OUTsub2$hab <- TRUE


## Read data
for(i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){print( "File Missing") }else{
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet = OUTsub2$sheet[i])
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(i == 1){AAA <- temp} else{
      AAA <- rbind(AAA, temp)
    } 
  }
}


## Formatting
ii <- AAA$Taxa == "No cyanobacteria observed" | AAA$Taxa == "ND" | is.na(AAA$Taxa)
AAA$Taxa[ii] <- "NAF"
## Since ID is inconsistent, use station id as 99999
AAA<- AAA[!is.na(AAA$Location), ]
lk <- substr(AAA$Location, 2,4)
ii <- !grepl("^2", AAA$Location)  ## missing valid lake
lk[ii] <- "EFR" ## hard code see issue 31
stt <- substr(AAA$Location, 5,22)
stt <- sub("^ +", "", stt)  ## spaces only
stt[ii] <- AAA$Location[ii]


## Counts and density columns alternatively record values.  Assuming both are density
val <- apply(AAA[, c("Count per Taxon", "Density")], 1, max, na.rm = TRUE)
val[!is.finite(val)] <- NA

ID <- paste("2", lk, stt, AAA$Sample.Date, AAA$Sample.Time, "000", sep = "")
algae <- data.frame(ID = ID,
                    lake = lk,
                    station = stt,
                    depth_ft = "000",
                    date = AAA$`Sample Date`,
                    taxa = AAA$Taxa,
                    cell_per_l = val * 1000, ### converted from ml
                    BV.um3.L = NA,  
                    class = NA,
                    hab = TRUE ,
                    sheet_id = AAA$sheet_id)


## Fix date
ii  <- algae$taxa == "NAF"
algae$cell_per_l[ii] <- 0
algae$class[ii] <- "Blue-Green"
algae$BV.um3.L[ii] <- 0

ii <- grepl("^0605", algae$date)
algae$date[ii]<- "20130605"


## Write data
chunck_check(algae)
setwd(homeDir)

if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
}


