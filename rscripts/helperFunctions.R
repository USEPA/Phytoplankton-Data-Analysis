### helper functions

readSelectSheets <- function(shortList){
 i <- 1
 err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
 if(class(err) == "try-error") next  
  temp      <- readWorksheet(wb, sheet=shortList$sheet[i])


for(i in 2:nrow(shortList) ){
  #for(i in 2:6 ){
  print(i)     
  err <-    try( wb     <- loadWorkbook(shortList$full_file_name[i]) )
  if(class(err) == "try-error") next
  x      <- readWorksheet(wb, sheet=shortList$sheet[i])
  temp <- merge(temp, x, all = TRUE)
  
  xlcFreeMemory()
}

return(temp)
}
  

#####
factor_2_character <- function(X){
     
     for(i in 1:ncol(X)){ 
          if(is.factor(X[,i])){
               X[,i] <- as.character(X[,i])  
          }
     }
     return(X)
}
