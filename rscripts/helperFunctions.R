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
df$qual2 <- NA  
df$result_num2 <- NA  
### identify values with <> in the same result.
i <- grepl("<", df$qual1) & grepl(">", df$qual1 )  
ii <- grepl( "^>", df$qual1)  

df$qual2[i] <- "<"  
df$qual1[i] <- ">"  
### check to see that the first character is >
  x[x=="ND"] <- -777
  
df$result_num <-  gsub("[[:alpha:]]", replacement='', x )    
df$result_num <-  gsub("[<>]", replacement='', df$result_num ) 
df$result_num <-as.numeric(df$result_num)
### split dual censor on the <    
  temp1     <- strsplit( x, "<")
  temp2     <- ldply(temp1, rbind)
  temp2[,1] <-   gsub("[>,]", replacement='', temp2[,1]) 
  temp2[,1] <- as.numeric(temp2[,1])
  temp2[,2] <- as.numeric(temp2[,2])

  df$result_num[i] <- temp2[i,1]
  df$result_num2[i] <- temp2[i,2]
  
  return(df)  
}

#test <- result_convert(WQ_all$result)

  split_sampleID <- function(x){
  ## x is the column with a sample id.  This is expected to be 24 characters long.
  ## returns a dataframe with components split
  id <- nchar(x) == 24
  out <- data.frame(ID =x,
                    lake = NA, 
                    station = NA,
                    date = NA,
                    time = NA,
                    depth_ft = NA
                    )
  out$lake[id] <- substr(x,2,4 )
  out$station[id] <- substr(x, 5,9)
  out$date[id] <- substr(x,10,17 )
  out$time[id]<- substr(x,18,21 )
  out$depth_ft[id]<- substr(x,22,24 )
return(out)
}


# temp <- split_sampleID(AAA$sample_id)



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

### is there ever anything in column  B, not in column A
compColumns <- function(A, B){  #compares columns A and B with an eye towards dropping column B.
  if( sum(! ( ( A == B) | is.na(B) )) == 0 ){
    print(" Drop second column. ")}else{
      id1 <- !is.na(B)  & is.na(A)  ### does column B contain values when column A is NA
      if(sum(id1)== 0){
        id <- A != B
        id2 <- is.na(A) & is.na(B)  ### makes NA == NA -> TRUE
        id[id2] <- FALSE
        if(sum(id)==0){print("Drop second column")}else{print("Investigate - discrepancy between non-empty cells")}
      }else {print("Column B contains values while column A is NA")}
    }
}
