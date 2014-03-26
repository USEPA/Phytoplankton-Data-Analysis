
### checks before each algae chunck is merged.

chunck_check <-function(x){

  id <- is.na(x$ID)
  if(sum(id) > 0){print("! Missing ID")}
  id <- is.na(x$sheet_id)
  if(sum(id) > 0){print("! Missing sheet_id")}
  
   if( max(nchar(x$ID))!=24){ print(paste("! Max length = ", max(nchar(x$ID))))}
   if( min(nchar(x$ID))!=24){ print(paste("! Min length = ", max(nchar(x$ID))))}
   xx <- range(as.numeric(x$depth_ft))
   yy <- as.Date(as.character(x$date), format = "%Y%m%d")
   
   if(max(yy) > "2014-03-24" | min(yy) < "1980-01-01" ) { print("!Check Range of dates") 
                                                          print(range(yy) ) }
  xx <- unique(x$hab)
  
  if(! all( xx%in%c(TRUE, FALSE) ) ) { print("! Check coding on HAB;  values missing")}
   
if(!  all( is.finite(x$cell_per_l) | is.finite(x$BV.um3.L) ) ){ print("! Missing data ")}
  print("Checked")
  
  ### check for ' marks
  
i <- grepl( pattern="'", x$taxa)
  if(sum(i)> 0){ print("Single Quote in taxa.")}

ii <- grepl( pattern='"', x$taxa)
  if(sum(ii)> 0){ print("Double Quote in taxa.")}


}


#i <- is.na(yy)
#i <- !grepl("^2", algae$date)           
           