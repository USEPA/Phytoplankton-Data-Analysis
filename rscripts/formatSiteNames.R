#### Will Barnett, Dec. 2014
#### Jade provided a list of corrected site names in June
#### 2014, but the format of that file isn't useful.

setwd("/Users/wbarnett/Documents/Neptune/svn/EPA-ORD/trunk/2013/TO 0016/analysis/Phytoplankton-Data-Analysis")
library(XLConnect)
library(plyr)


## Workbook
wb <- loadWorkbook("meta_info/Corrected Site Names.xlsx")


## Read Algae file
nms <- readWorksheet(wb, sheet = "Algae")[,1:2]
names(nms) <- c("Stuff","Corrected")
d1 <- ddply(nms, .(Stuff), .fun = function(x){
 tmp <- gsub(pattern = "\"", replacement = "", x = unlist(strsplit(x$Stuff,split = " "))[1:2],
             fixed=TRUE)
 return(data.frame("sheet_id" = tmp[1], "Original" = tmp[2], "Corrected" = x$Corrected ))
})


## Read Chemical file
nms <- readWorksheet(wb, sheet = "Chemical",
                     startRow=2, startCol=1)[,1:2]
names(nms) <- c("Incorrect","Corrected")


## Write new file.
wbOut <- loadWorkbook("meta_info/Edited Site Names.xlsx", create = TRUE)
createSheet(wbOut, name = "Chemical")
writeWorksheet(wbOut, data = nms, sheet = "Chemical")
createSheet(wbOut, name = "Algae")
writeWorksheet(wbOut, data = d1[c("sheet_id","Original","Corrected"),], 
               sheet = "Algae")
saveWorkbook(wbOut)
