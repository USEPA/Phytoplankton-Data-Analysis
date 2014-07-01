### read jade0424.R
library(reshape2)

### check 0307, 0324

setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)
id <- grepl(pattern="jade0424a", OUT$full_file_name)

### subset of jade0424a files
OUTsub2 <- OUT[id  & !OUT$processed, ]

 # bkup <- OUTsub2
  # bkupOUT <- OUT

 OUTsub2 <- bkup
 OUT <- bkup

OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readJade0424a.R"

###
### id for full list


### don't read sheets starting with 121.
iii <- grep("^121", OUTsub2$sheet )
OUTsub2$processed[iii] <- TRUE
OUTsub2$script[iii]    <- "readJade0424a.R"
OUTsub2$skip[iii]      <- "Not processed, data read by merging sample results and details."

####
OUTsub3 <- OUTsub2[iii,]

dimCheck <- 0
xxx <- NULL

files <- "jade0424a/2GRR20120912 No Biovolume Phytoplankton.xlsx"

for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" )
  tempDetails <- readWorksheet(wb, sheet="Sample Details" )
  tempBoth <- merge(tempResults, tempDetails, all.x = TRUE )
  tempBoth$sheet_id <- subset(OUT, sheet == "Sample Results" & full_file_name == files[j] )$sheet_id
  if(j == 1){
    xxx <- tempBoth
  }else { xxx <- merge(xxx, tempBoth, all = TRUE)}
}

i <- OUT$full_file_name %in% files
OUT$processed[i] <- TRUE

groupA <- xxx
####

files <- "jade0424a/2BRR20120911 Phytoplankton.xls" 

for(j in 1:length(files)){
  err <-    try( wb     <- loadWorkbook(files[j]) )
  if(class(err) == "try-error") print( "File Missing") 
  wsheets <- getSheets(wb)  
  tempResults <- readWorksheet(wb, sheet="Sample Results" ) 
wsheets <-   wsheets[grep("^121",wsheets)]
  xxx<- NULL
  for( i in 1:length(wsheets) ){
    tempSheet <- readWorksheet(wb, sheet=wsheets[i], startRow=9 ) 
  tempSheet <- tempSheet[!is.na(tempSheet$Genus.species), ]
  tempSheet$Sample.ID <- wsheets[i]
  
  xxx <- merge(xxx, tempSheet, all = TRUE)
}
  tempBoth <- merge(tempResults, xxx, all.x = TRUE ) 
}
groupAA <- tempBoth


###
temp <- unique(OUTsub2$full_file_name)
i    <- OUT$full_file_name %in% temp & OUT$sheet == "Sample Results" & !(OUT$full_file_name %in% files)# & !OUT$processed
iX   <- OUTsub2$full_file_name %in% temp & OUTsub2$sheet == "Sample Results" & !(OUTsub2$full_file_name %in% files)

OUTsub3 <- OUT[i,]

OUT$processed[i] <- TRUE
OUTsub2$processed[iX] <- TRUE
OUT$script[i] <- "readJade0424a.R"


dimCheck <- 0
xxx <- NULL
for(i in 1:nrow(OUTsub3)){
  
  err <-    try( wb     <- loadWorkbook(OUTsub3$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  #  print(i)
  temp <- readWorksheet(wb, sheet=OUTsub3$sheet[i])
  ### fix names
  names(temp) <- tolower(names(temp))
  names(temp) <- gsub("\\.", names(temp), replacement="") 
  ii <- grep("^col", names(temp))
  if(length(ii) > 0){
  #  print(temp[,ii])
    temp <- temp[, -ii]
  } 

  ii <- grep("^cells", names(temp) )
  names(temp)[ii] <- "densitycellsl"
  
  ii <- grep("totalbiovolumeµm3l", names(temp) )
  names(temp)[ii] <- "totalbiovolumeum3l"
  
  ii <- grep("meanbiovolumeµm3", names(temp) )
  names(temp)[ii] <- "meanbiovolumeum3"
    
  temp <- temp[!is.na(temp$location), ]
  temp$sheet_id <- OUTsub3$sheet_id[i]
  
  if(i == 1){
    xxx <- temp
  }else{
    xxx <- merge(xxx, temp, all = TRUE)
  }
  
}
  
groupB <- xxx

### temp move


iii <- OUT$ncol == 10 & ( nchar(OUT$sheet) == 3 )
iiiX <- OUTsub2$ncol == 10 & ( nchar(OUTsub2$sheet) == 3 ) 

OUT$processed[ iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUTsub2$processed[iiiX] <- TRUE

OUT$script[iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- "readJade0424a.R"
OUT$skip[iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- "Data present in Sample Results Sheet"

#ii <- grepl("^121", OUTsub2$sheet ) & !OUTsub2$processed
#iiX <- paste( OUT$full_file_name, OUT$sheet)  %in% unique(paste(OUTsub2$full_file_name[ii], OUTsub2$sheet[ii] ) )

#OUT$processed[iX] <- TRUE
#OUTsub2$processed[ii] <- TRUE

OUTsub3 <- OUTsub2[!iiiX,]
#X <- subset(OUTsub2, ncol == 8)





for(i in 1:nrow(OUTsub3)){
  
  err <-    try( wb     <- loadWorkbook(OUTsub3$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  #  print(i)
  temp <- readWorksheet(wb, sheet=OUTsub3$sheet[i], startRow=9 )
 # temp <- temp[-1, ]
  temp <- temp[! is.na(temp$Division), ]
  names(temp) <- tolower(names(temp))
  names(temp) <- gsub("\\.", names(temp), replacement="") 
  ### fix names
  
  ii <- grep("^col", names(temp))
  if(length(ii) > 0){
    #  print(temp[,ii])
    temp <- temp[, -ii]
  } 
   temp$sheet_id <- OUTsub3$sheet_id[i]
  tmp2 <- readWorksheet(wb, sheet=OUTsub3$sheet[i], endRow=6, endCol = 4 )
 
  ii <- grep( x=tmp2[,1], pattern= "Sample ID")
  temp$sample_id <- tmp2[ii,2]
  
  ii <- grep("^cells", names(temp) )
  names(temp)[ii] <- "densitycellsl"
  
  ii <- grep("totalbiovolumeµm3l", names(temp) )
  names(temp)[ii] <- "totalbiovolumeum3l"
  
  ii <- grep("meanbiovolumeµm3", names(temp) )
  names(temp)[ii] <- "meanbiovolumeum3"
  
  
  
  temp <- temp[!is.na(temp$location), ]
  if(i == 1){
    xxx <- temp
  }else{
    xxx <- merge(xxx, temp, all = TRUE)
  }
  
}



algae <- data.frame(ID = AAA$sample_id,
                    lake = substr(AAA$sample_id, 2,4),
                    station = substr(AAA$sample_id, 5, 9),
                    depth_ft = substr(AAA$sample_id, 22, 24),
                    date = substr(AAA$sample_id, start=10, stop=17),
                    taxa = AAA$Genus,
                    cell_per_l = AAA$Concentration..cells.mL.,
                    BV.um3.L = NA,  ## how can I be certain about these units?
                    class = NA,
                    hab = TRUE,   ### hard coded HAB, see issue 
                    sheet_id = AAA$sheet_id)







####
OUTsub2 <- subset(OUTsub, sheet == "Sample Result" )

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirK.R"
