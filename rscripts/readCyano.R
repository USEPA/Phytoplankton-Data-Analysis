################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with Cyano in the name
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
id <- grepl(pattern="^Cyano", OUT$sheetNames)
OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readCyano.R"

OUTsub2$hab <- grepl("HAB", OUTsub2$full_file_name, ignore.case=TRUE)

dimCheck <- 0
for(i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){print( "File Missing") }else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet=OUTsub2$sheet[i], skip=7)
    temp <- temp[!is.na(temp$Division), -which(names(temp) == "")]
    if(nrow(temp)==0){ 
      temp<- data.frame( Division = "Cynophycota", Genus = "Not Found", "Concentration (cells/mL)" = NA)
    }
    temp$sample_id <- unlist(read_excel(wb, sheet=OUTsub2$sheet[i], skip = 2, col_names = FALSE)[1,1])
    temp$hab <- OUTsub2$hab[i]
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(grepl("Concentration",names(temp))) temp$`Concentration (cells/mL)` <- as.numeric(temp$`Concentration (cells/mL)`)
    dimCheck <- nrow(temp) + dimCheck
    if(i == 1){AAA <- temp} else{
      AAA <- merge(AAA, temp, all = TRUE)
    }
  } 
}

#### check ID
AAA <- factor_2_character(AAA)
ii <- nchar(AAA$sample_id) != 24

## ISSUE REPORTED FOR CLARIFICATION
#Drew data/f/31tab Fall HAB Data EFR20121031,SRR20121107,TAR20121106 alg.xls
AAA <- AAA[!ii, ]

algae <- data.frame(ID = AAA$sample_id,
                     lake = substr(AAA$sample_id, 2,4),
                     station = substr(AAA$sample_id, 5, 9),
                     depth_ft = substr(AAA$sample_id, 22, 24),
                     date = substr(AAA$sample_id, start=10, stop=17),
                     taxa = AAA$Genus,
                     cell_per_l = AAA$`Concentration (cells/mL)` * 1000,
                     BV.um3.L = as.numeric(NA),  ## how can I be certain about these units?
                     class = NA,
                     hab = AAA$hab,
                     sheet_id = AAA$sheet_id)


## Fix date and quotes
ii <- algae$ID == "2TARALG01020121161540000"
algae$date[ii] <- "20121106"
algae$taxa <- gsub(pattern='"', replacement = "", algae$taxa)
algae$taxa <- gsub(pattern="'", replacement = "", algae$taxa)


## Write file
setwd(homeDir)
chunck_check(algae)
if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE) 
  
}