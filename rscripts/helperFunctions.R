### helper functions

#####
factor_2_character <- function(X){
     
     for(i in 1:ncol(X)){ 
          if(is.factor(X[,i])){
               X[,i] <- as.character(X[,i])  
          }
     }
     return(X)
}
