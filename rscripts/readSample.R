################
#### Will Barnett, August 2016
################


################
#### This script is called from masterScript.R, and reads in the data
#### with Phytoplankton in the file name, with some exceptions.
################


## Change working directory
datDir <- "originalData/algae/EFR Phytoplankton Data/"
setwd(datDir)

## Subset the files
id <- grepl("Phytoplankton", OUT$file) & grepl("Sample Results", OUT$sheet)
id1 <- grepl("jade042414", OUT$full_file_name) ## These files have a separate script
id2 <- grepl("2012PhytoDataFiles", OUT$full_file_name) ## These files have a separate script
OUTsub2 <- OUT[(id & !(id1 | id2)) & OUT$processed == FALSE, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readSample.R"


## Read the data
## Checking names
nmsList <- list()
for( i in 1:nrow(OUTsub2)){ # i = 1
  wb <- OUTsub2$full_file_name[i]
  temp <- read_excel(wb, sheet = OUTsub2$sheet[i], col_names = TRUE)
  nmsList[[i]] <- names(temp)
}
## Names alll look the same,
for( i in 1:nrow(OUTsub2)){ # i = 1
  err <-    try( excel_sheets(OUTsub2$full_file_name[i]) )
  if(class(err) == "try-error"){ print("Error")}else {
    wb <- OUTsub2$full_file_name[i]
    temp <- read_excel(wb, sheet = OUTsub2$sheet[i], col_names = TRUE)
    temp <- temp[,-which(names(temp) == 'Date Analyzed')]
    #  print(tail(temp))
    dimCheck <- dimCheck + nrow(temp)
    temp$sheet_id <- OUTsub2$sheet_id[i]
    if(i == 1){
      AAA <- temp
    }else {
      names(temp) <- names(AAA)
      AAA <- rbind(AAA, temp)  
    }
  } # end else
} # end for


## Rearrange some dates
ii <- which(substr(AAA$`Sample Date`, 1,1)!=2)
AAA$`Sample Date`[ii] <- paste(substr(AAA$`Sample Date`[ii] , 5,8) ,
                             substr(AAA$`Sample Date`[ii] , 1,2),
                             substr(AAA$`Sample Date`[ii] , 3,4),
                             sep = "")

ID <- paste(AAA$Location, AAA$`Sample Date`, 
            formatC(AAA$`Sample Time`, width = 4, flag = "0"),
            formatC(AAA$`Sample Depth`, width = 3, flag = "0"), sep = "")

ii <- nchar(ID)== 24
ID <- ID[ii] 
AAA  <- AAA[ii, ]

algae <- data.frame(ID = ID,
                    lake = substr(ID, 2,4),
                    station = substr(ID, 5,9) ,
                    depth_ft = substr(ID,22,24 ),
                    date = substr(ID, start=10, stop=17),
                    taxa = AAA$Taxa,
                    cell_per_l = AAA$`Cells/liter`,
                    BV.um3.L = AAA$`Total biovolume (µm3/L)`,
                    class = NA,
                    hab = FALSE,
                    sheet_id = AAA$sheet_id)


chunck_check(algae)
setwd(homeDir)


if(WRITE){
  write.table(algae, "processed_data/algae.csv", row.names=FALSE, sep = ",", append= TRUE, col.names = FALSE)
}

