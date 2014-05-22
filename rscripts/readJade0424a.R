### read jade0424.R

library(reshape2)
### check 0307, 0324

setwd("originalData/algae/EFR Phytoplankton Data/")

OUT <- factor_2_character(OUT)

id <- grepl(pattern="jade0424a", OUT$full_file_name)

OUTsub2 <- OUT[id  & !OUT$processed, ]
OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readJade0424a.R"

####

iii <- OUT$ncol == 10 & ( nchar(OUT$sheet) == 3 )
OUT$processed[ iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- "readJade0424a.R"
OUT$skip[iii & OUT$full_file_name %in% OUTsub2$full_file_name] <- "Data present in Sample Results Sheet"

temp <- unique(OUT$full_file_name[iii])
i <- OUT$full_file_name %in% temp & OUT$sheet == "Sample Results"

OUTsub3 <- OUT[i,]


dimCheck <- 0
xxx <- NULL
for(i in 1:nrow(OUTsub3)){
  
  err <-    try( wb     <- loadWorkbook(OUTsub3$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  #  print(i)
  temp <- readWorksheet(wb, sheet=OUTsub3$sheet[i])
  temp <- temp[!is.na(temp$Location), ]
  if(i == 1){
    xxx <- temp
  }else{
    xxx <- merge(OUT, temp, all = TRUE)
  }
  
}
  
  if(nrow(temp)==0){ 
    temp<- data.frame( Division = "Cynophycota", Genus = "Not Found", "Concentration..cells.mL." = NA)
  }
  temp$sample_id <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                         startCol=1,endCol=1,startRow=3, endRow=3, header = FALSE) )
  
  temp$sample_date <- unlist(readWorksheet(wb, sheet=OUTsub2$sheet[i], 
                                           startCol=3,endCol=3,startRow=3, endRow=3, header = FALSE) )
  
  
  
  
  



X <- subset(OUTsub2, grepl("^12", sheet) )
X$hab <- FALSE
#X <- subset(OUTsub2, ncol == 8)


#####


rm("AAA")
dimCheck <- 0


for(i in 1:nrow(X)){
  
  err <-    try( wb     <- loadWorkbook(X$full_file_name[i]) )
  if(class(err) == "try-error") print( "File Missing") 
  #  print(i)
  temp <- readWorksheet(wb, sheet=X$sheet[i], startRow=9)
  temp <- temp[-1, ]  
  temp <- temp[!is.na(temp$Division), ]
  if(nrow(temp)==0){ 
    temp<- data.frame( Division = "Cynophycota", Genus = "Not Found", "Concentration..cells.mL." = NA)
  }
  temp$sample_id <- unlist(readWorksheet(wb, sheet=X$sheet[i], 
                                         startCol=2,endCol=2,startRow=5, endRow=5, header = FALSE) )
  
DD<-  unlist(readWorksheet(wb, sheet=X$sheet[i], 
                                           startCol=7,endCol=7,startRow=3, endRow=3, header = FALSE) )
  if(grepl("^Date", DD[1] )){
    DD<-  unlist(readWorksheet(wb, sheet=X$sheet[i], 
                               startCol=8,endCol=8,startRow=3, endRow=3, header = FALSE) ) 
    
  }
  
  temp$sample_date <- DD
  temp$hab <- X$hab[i]
  temp$sheet_id <- X$sheet_id[i]
  dimCheck <- nrow(temp) + dimCheck
  if(i == 1){AAA <- temp} else{
    AAA <- merge(AAA, temp, all = TRUE)
  }
}

ii <- AAA$Division %in% c( "10 ml filtered per slide (3 slides prepared)", "5 ml water sample filtered per slide @ 3 slides total Total",
                           "Grand Total", "Grand Total ", "GRAND TOTAL", "GRAND TOTAL ")

AAA <- AAA[ !ii, ]

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
                    cell_per_l = AAA$Concentration..cells.mL.,
                    BV.um3.L = NA,  ## how can I be certain about these units?
                    class = NA,
                    hab = TRUE,   ### hard coded HAB, see issue 
                    sheet_id = AAA$sheet_id)







####
OUTsub2 <- subset(OUTsub, sheet == "Sample Result" )

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
OUT$script[OUT$full_file_name %in% OUTsub2$full_file_name] <- "readDirK.R"
