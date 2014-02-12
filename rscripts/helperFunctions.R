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
  x$file_id <- shortList$sheet_id[i]
  temp <- merge(temp, x, all = TRUE)
  #temp <- rbind(temp, x)
  print(dim(temp) )
  xlcFreeMemory()
}

return(temp)
}
  

### result convert
result_convert <- function (x){
  if( !is.character(x)) warning("Expecting a character vector")
  ## strip blanks
 x <- gsub( "[[:space:]]", replacement="", x)
  ###
df <-  data.frame(original = x)
df$qual1 <- gsub("[[:digit:]]|\\.", replacement='', x )
df$result_num <-  gsub("[[:alpha:]]", replacement='', x )    
df$result_num <-  gsub("[<>]", replacement='', df$result_num ) 
df$result_num <-as.numeric(df$result_num)
return(df)  
}





#####
factor_2_character <- function(X){
     
     for(i in 1:ncol(X)){ 
          if(is.factor(X[,i])){
               X[,i] <- as.character(X[,i])  
               ## strip spaces
               
          }
     }
     return(X)
}
