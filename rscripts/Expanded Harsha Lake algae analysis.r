# J. Beaulieu 6/6/13
# modified to reflect project paths; mjp
# Initial code to look over Harsha Lake Cyano data set
rm(list=ls())
library(ggplot2)
library(mgcv)
library(reshape)
library(scales)
#######################################################################################################################################
#Function to be used to identify rows containing only NA
#http://stackoverflow.com/questions/6471689/remove-rows-in-r-matrix-where-all-data-is-na
all.na <- function(X1) {
  all(is.na(X1))
  }
  
#Need to preserve all digits in 'Sample_Number' for subsequent subscripting.
options(scipen=999)                                  #https://stat.ethz.ch/pipermail/r-devel/2003-May/026485.html
#######################################################################################################################################
#FIRST, PULL IN DATA SETS AND FORMAT
filePath <- "originalData/algae//EFR Phytoplankton Data"
### PRE 1993           
  #NEW DATA: All Lakes Phyto Data pre-1993 Plankton Biomass.txt and All Lakes Phyto Data pre-1993 Total algae.txt
    pre.1993.bio <- read.table(file.path(filePath, 'All Lakes Phyto Data pre-1993 Plankton Biomass.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '')                
    str(pre.1993.bio);head(pre.1993.bio) 
    #Create 'Lake' vector
      pre.1993.bio$Lake <- substr(as.character(pre.1993.bio$Loc_ID), start=2, stop=4)                               
    #Split out station
      pre.1993.bio$Station <- substr(as.character(pre.1993.bio$Loc_ID), start=5, stop=9)
    #Sample Date
      pre.1993.bio$Date <- as.Date(substr(as.character(pre.1993.bio$Sample_Num), start=1, stop=8), format = '%Y%m%d')
    #Depth
      pre.1993.bio$Depth.ft <- as.integer(substr(pre.1993.bio$Sample_Num, start=13, stop=15))
    #Taxa
      pre.1993.bio$Taxa <- NA                   
    #Division
      pre.1993.bio$Division <- NA
    #Biovolume
      pre.1993.bio$BV.um3.L <- pre.1993.bio$Value * 1000000000000                #convert to um3 / L
    #Total Algae data
     pre.1993.tot <- read.table(file.path(filePath,
                      'All Lakes Phyto Data pre-1993 Total algae.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '')                
    str(pre.1993.tot);head(pre.1993.tot)     
    #Create 'Lake' vector
      pre.1993.tot$Lake <- substr(as.character(pre.1993.tot$Loc_ID), start=2, stop=4)                               
    #Split out station
      pre.1993.tot$Station <- substr(as.character(pre.1993.tot$Loc_ID), start=5, stop=9)
    #Sample Date
      pre.1993.tot$Date <- as.Date(substr(as.character(pre.1993.tot$Sample_Num), start=1, stop=8), format = '%Y%m%d')
    #Depth
      pre.1993.tot$Depth.ft <- as.integer(substr(pre.1993.tot$Sample_Num, start=13, stop=15))
    #Taxa
      pre.1993.tot$Taxa <- NA                   
    #Division
      pre.1993.tot$Division <- NA         
    #Cells/L
      pre.1993.tot$Cells.L <- as.numeric(gsub('>', '', as.character(pre.1993.tot$Value))) * 1000          #gsub used to remove greater than (>)
    #Merge biovolume and total algae data
      pre.1993 <- merge(pre.1993.bio[,c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L')], 
                        pre.1993.tot[,c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'Cells.L')], 
                        by = c('Lake', 'Station', 'Date', 'Depth.ft'), all = T)   
      #dotchart(pre.1993$BV.um3.L)                                               #one obvious data entry error
      pre.1993[pre.1993$BV.um3.L > 8e+14 & !is.na(pre.1993$BV.um3.L), 
              'BV.um3.L'] = NA                                                  #exclude outlier
      colnames(pre.1993)[colnames(pre.1993) == c('Taxa.x', 'Division.x')] = c('Taxa', 'Division')        #rename taxa and division 
      pre.1993 <- pre.1993[,-c(which(colnames(pre.1993) == 'Taxa.y'), 
                    which(colnames(pre.1993) == 'Division.y'))]                                          #akward way of exlcuding extra column
    #  pre.1993


### 1988    
  #NEW DATA: EFR 1988 Phyto.txt
    p.1988.efr <- read.table(file.path(filePath,
                      'EFR 1988 Phyto.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '')                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.1988.efr);head(p.1988.efr)        
    #Create 'Lake' vector
      p.1988.efr$Lake <- substr(as.character(p.1988.efr$Station), start=2, stop=4)                               
    #Split out station
      p.1988.efr$Station <- substr(as.character(p.1988.efr$Station), start=5, stop=nchar(as.character(p.1988.efr$Station)))
    #Sample Date
      p.1988.efr$Date <- as.Date(as.character(p.1988.efr$Date..yymmdd.), format = '%y%m%d')
    #Taxa
      p.1988.efr$Taxa <- as.character(p.1988.efr$Species)
      p.1988.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.1988.efr$Taxa)                                     #strip out trailing/leading white space
    #Cells.L
      p.1988.efr$Cells.L <- p.1988.efr$Cells.ml * 1000
    #Biovolume
      p.1988.efr$BV.um3.L <- NA    
    #Division
      p.1988.efr$Division <- NA  
    #Simplify
      p.1988.efr <- p.1988.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.1998.efr <- p.1988.efr[-51,]                                                                  #Remove row with NA

###1992    
  #NEW DATA: 92Rawdat.txt, this file was generated from the '.wb1' file in the 'Files with complications folder'
  #Use skip =1 in 'read.table' to exclude the first row of .txt file which contains header info.
    p.1992 <- read.table(file.path(filePath,
                      'Files with complications/92Rawdat.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '', skip = 1)
    str(p.1992);head(p.1992) 
    #Create 'Lake' vector
      p.1992$Lake <- substr(as.character(p.1992$X), start=2, stop=4)
    #Split out station
      p.1992$Station <- substr(as.character(p.1992$X), start=5, stop=9)  
    #Sample date
      p.1992$Date <- as.Date(as.character(p.1992$Date), format = '%Y%m%d')
    #Depth
      p.1992$Depth.ft <- as.integer(as.character(p.1992$X.ft.))
    #Taxa
      p.1992$Taxa <- as.character(p.1992$X.1)
      p.1992$Taxa <- gsub("^\\s+|\\s+$", "", p.1992$Taxa)                         #Strip leading and trailing white spaces
    #Biovolume
      p.1992$BV.um3.L <- as.numeric(as.character(p.1992$X.um3.l.))
    #Cells per L
      p.1992$Cells.L <- p.1992$X.cells.l.     
    #Simplify
      p.1992 <- p.1992[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
    #Quick inspection for gross outlier
      #dotchart(p.1992$BV.um3.L)                                                 #1e11 is max in Lake Taihu
      p.1992[p.1992$BV.um3.L > 6e+10, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa')]
      #dotchart(p.1992$Cells.L)      
      unique(p.1992$Depth.ft)
      unique(p.1992$Date)
      unique(p.1992$Lake)


### 1993            
  #NEW DATA: EFR 1993 Phyto.txt
    p.1993.efr <- read.table(file.path(filePath,
                      'EFR 1993 Phyto.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '')                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.1993.efr);head(p.1993.efr)    
    #Create 'Lake' vector
      p.1993.efr$Lake <- substr(as.character(p.1993.efr$Location), start=2, stop=4)                               
    #Split out station
      p.1993.efr$Station <- substr(as.character(p.1993.efr$Location), start=5, stop=nchar(as.character(p.1993.efr$Location)))
    #Sample Date
      p.1993.efr$Date <- as.Date(as.character(p.1993.efr$Sample.Date), format = '%m/%d/%Y')
    #Depth
      p.1993.efr$Depth.ft <- p.1993.efr$Sample.Depth
    #Taxa
      p.1993.efr$Taxa <- as.character(p.1993.efr$Taxa)
      p.1993.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.1993.efr$Taxa)                                     #strip out trailing/leading white space
    #Division
      p.1993.efr$Division <- NA  
    #Cells.L
      p.1993.efr$Cells.L <- p.1993.efr$Density....l.
    #Biovolume
      p.1993.efr$BV.um3.L <- p.1993.efr$Biovolume..um3.L.    
    #Simplify
      p.1993.efr <- p.1993.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest


### 1995   
  #NEW DATA: Algae 95.txt, this file was generated from the .dbf file in the 'Files with complications folder'
  #Used 'quote=''' argument to disable quoting.  Needed because Depth was entered as '0, '5,....
    p.1995 <- read.table(file.path(filePath,
                      'Files with complications/Algae 95.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '', quote='')
    str(p.1995);head(p.1995) 
    #Create 'Lake' vector
      p.1995$Lake <- toupper(as.character(p.1995$LAKE))
    #Split out station
      p.1995$Station <- as.character(p.1995$STATION)  
    #Sample date
      p.1995$Date <- as.Date(as.character(p.1995$DATE), format = '%d-%b-%y')
    #Depth
      p.1995$Depth.ft <- as.character(p.1995$DEPTH)
      unique(p.1995$Depth.ft)
      p.1995[p.1995$Depth.ft == "0-5'", 'Depth.ft'] = 0                         #Convert unusual depths to project convention
      p.1995[p.1995$Depth.ft == "0-10'", 'Depth.ft'] = 10                       #Convert unusual depths to project convention
      p.1995[p.1995$Depth.ft == "0-20'", 'Depth.ft'] = 20                       #Convert unusual depths to project convention
      p.1995$Depth.ft <- as.integer(gsub("'", "", p.1995$Depth.ft))             #remove "'", convert to integer  
   #Taxa
      p.1995$Taxa <- as.character(p.1995$TAXA)
      p.1995$Taxa <- gsub("^\\s+|\\s+$", "", p.1995$Taxa)                       #Strip leading and trailing white spaces
      #format troublesome name
      p.1995[p.1995$Taxa == "\"Chrysophyta \"\"A'\"" & !is.na(p.1995$Taxa), 'Taxa'] = 'Chrysophyta'
      p.1995[p.1995$Taxa == "\"Mallomonas \"\"small\"\"\"" & !is.na(p.1995$Taxa), 'Taxa'] = 'Mallomonas'
    
    #Biovolume
      p.1995$BV.um3.L <- p.1995$BV_L
    #Cells per L
      p.1995$Cells.L <- p.1995$CELLS_L     
    #Simplify
      p.1995 <- p.1995[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
    #Quick inspection for gross outlier
      #dotchart(p.1995$BV.um3.L)                                                 #1e11 is max in Lake Taihu
      #dotchart(p.1995$Cells.L)                                                  #5x10^6 L-1 reported in Wetzel (pg.363).  1x10^6 = bloom (WHO)
      unique(p.1995$Depth.ft)
      unique(p.1995$Date)
      unique(p.1995$Lake)      


### 1998        
  #NEW DATA: EFR 1998 Phyto.txt
    p.1998.efr <- read.table(file.path(filePath,
                      'EFR 1998 Phyto.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE, comment.char = '')                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.1998.efr);head(p.1998.efr)
    #Create 'Lake' vector
      p.1998.efr$Lake <- substr(as.character(p.1998.efr$Location), start=2, stop=4)                               
    #Split out station
      p.1998.efr$Station <- substr(as.character(p.1998.efr$Location), start=5, stop=nchar(as.character(p.1998.efr$Location)))
    #Sample Date
      p.1998.efr$Date <- as.Date(as.character(p.1998.efr$Sample.Date), format = '%m/%d/%Y')
    #Depth
      p.1998.efr$Depth.ft <- p.1998.efr$Sample.Depth
    #Taxa
      p.1998.efr$Taxa <- as.character(p.1998.efr$Taxa)
      p.1998.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.1998.efr$Taxa)                                     #strip out trailing/leading white space
    #Division
      p.1998.efr$Division <- NA
    #Cells.L
      p.1998.efr$Cells.L <- p.1998.efr$Density....l.
    #Biovolume
      p.1998.efr$BV.um3.L <- p.1998.efr$Biovolume..um3.L.
    #Simplify
      p.1998.efr <- p.1998.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.1998.efr <- p.1998.efr[!apply(p.1998.efr, 1, all.na), ]                         #Remove rows of NA

### 1999 - 2008
  #NEW DATA:  1999 - 2008.  I combined the 1999 - 2008 Excel files into one Excel spreadsheet.
  #The originals were formatted identically, with the exception of the date.  It was much faster to format
  #the date in Excel, then format everything else simultaneously in R.
    p.efr.99.08 <- read.table(file.path(filePath,
                      'EFR 1999_2008 Phyto.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
    str(p.efr.99.08)            
    #Lake
      p.efr.99.08$Lake <- substr(as.character(p.efr.99.08$Location), start=2, stop=4)
    #Station
      p.efr.99.08$Station <- substr(as.character(p.efr.99.08$Location), start=5, stop=9)  
    #Date
      p.efr.99.08$Date <- as.Date(as.character(p.efr.99.08$Sample.Date), format = '%m/%d/%Y')
    #Depth
      p.efr.99.08$Depth.ft <- p.efr.99.08$Sample.Depth
    #Taxa
      p.efr.99.08$Taxa <- as.character(p.efr.99.08$Taxa)
      p.efr.99.08$Taxa <- gsub("^\\s+|\\s+$", "", p.efr.99.08$Taxa)                                     #strip out trailing/leading white space
      #fine tune problematic names
      p.efr.99.08[p.efr.99.08$Taxa == 'Coccoid Ovate Green (6X4)' & !is.na(p.efr.99.08$Taxa), 'Taxa'] = 'Coccoid Ovate Green'
      p.efr.99.08[p.efr.99.08$Taxa == 'Coccoid Ovate Green (10x6)' & !is.na(p.efr.99.08$Taxa), 'Taxa'] = 'Coccoid Ovate Green'
    #Cells per Liter
      p.efr.99.08$Cells.L <- p.efr.99.08$Density....l.
    #Biovolume
      p.efr.99.08$BV.um3.L <- p.efr.99.08$Biovolume..um3.L.
    #Simplify
      p.efr.99.08 <- p.efr.99.08[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'BV.um3.L', 'Cells.L')]
    #Inspect
      unique(p.efr.99.08$Lake)
      unique(p.efr.99.08$Station)
      unique(p.efr.99.08$Depth.ft)
      #dotchart(p.efr.99.08$Cells.L)
      #dotchart(p.efr.99.08$BV.um3.L)
      
### Cant read, mjp 12/20 
### 2011     
if(FALSE){  ## consider script to read from Excel files.
  #NEW DATA: Phytoplankton 2011 BRR EFR.txt
    p.2011.brr.efr <- read.table(file.path(filePath, 'Phytoplankton 2011 BRR EFR.txt'), header = TRUE, fill = TRUE, na.string='')#, sep='\t', strip.white = TRUE)                                         Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.2011.brr.efr);head(p.2011.brr.efr)
    #Create 'Lake' vector
      p.2011.brr.efr$Lake <- ifelse(grepl('EFR', as.character(p.2011.brr.efr$Location)), 'EFR', 
                              ifelse(grepl('BRR', as.character(p.2011.brr.efr$Location)), 'BRR', NA))                        #Only has EFR and BRR               
    #Split out station
      p.2011.brr.efr$Station <- substr(as.character(p.2011.brr.efr$Location), start=5, stop=nchar(as.character(p.2011.brr.efr$Location)))
    #Rename Sample.ID
      p.2011.brr.efr$ID <- p.2011.brr.efr$Sample.ID
    #Sample Date
      p.2011.brr.efr$Date <- as.Date(as.character(p.2011.brr.efr$Sample.Date), format = '%Y%m%d')
    #Depth
      p.2011.brr.efr$Depth.ft <- p.2011.brr.efr$Sample.Depth
    #Taxa
      p.2011.brr.efr$Taxa <- as.character(p.2011.brr.efr$Taxa)
      p.2011.brr.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.2011.brr.efr$Taxa)                                      #strip white space
    #Division
      p.2011.brr.efr$Division <- as.character(p.2011.brr.efr$Division)
      p.2011.brr.efr$Division <- gsub("^\\s+|\\s+$", "", p.2011.brr.efr$Division)
    #Cells.L
      p.2011.brr.efr$Cells.L <- as.numeric(gsub(',', '', as.character(p.2011.brr.efr$Cells.liter)))            #gsub to remove commas from numbers (http://stackoverflow.com/questions/1523126/how-to-read-a-csv-file-in-r-where-some-numbers-contain-commas)
    #Biovolume
      #Tricky because the greek letter mu was included in column name when imported (Total.Biovolume..um3.L)
      #Rename column
        colnames(p.2011.brr.efr)[grep('Total.biovolume', colnames(p.2011.brr.efr))] = 'BV.um3.L'  
      p.2011.brr.efr$BV.um3.L <- as.numeric(gsub(',', '', as.character(p.2011.brr.efr$BV.um3.L)))      
    #Simplify
      p.2011.brr.efr <- p.2011.brr.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.2011.brr.efr <- p.2011.brr.efr[!apply(p.2011.brr.efr, 1, all.na), ]                      #Remove rows of NA


###  more 2011
  #NEW DATA: EFR phyto 8-23-2011.txt
    p.8.23.11.efr <- read.table(file.path(filePath,
                      'EFR phyto 8-23-2011.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE)                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.8.23.11.efr);head(p.8.23.11.efr)
    #Create 'Lake' vector
      p.8.23.11.efr$Lake <- substr(as.character(p.8.23.11.efr$Sample.ID), start=2, stop=4)
    #Split out station
      p.8.23.11.efr$Station <- substr(as.character(p.8.23.11.efr$Sample.ID), start=5, stop=9)
    #Sample Date
      p.8.23.11.efr$Date <- as.Date(substr(as.character(p.8.23.11.efr$Sample.ID), start=10, stop=17), format = '%Y%m%d')                               
    #Sample Depth
      p.8.23.11.efr$Depth.ft <- as.integer(substr(as.character(p.8.23.11.efr$Sample.ID), start=22, stop=24))
    #Taxa
      p.8.23.11.efr$Taxa <- as.character(p.8.23.11.efr$Genus.species)
      p.8.23.11.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.8.23.11.efr$Taxa)                               #code to strip leading and trailing white space
    #Division
      p.8.23.11.efr$Division <- as.character(p.8.23.11.efr$Division)
      p.8.23.11.efr$Division <- gsub("^\\s+|\\s+$", "", p.8.23.11.efr$Division)
    #Cells/L
      p.8.23.11.efr$Cells.L <- p.8.23.11.efr$Concentration..cell..L.
    #Biovolume
      #Tricky because the greek letter mu was included in column name when imported (Total.Biovolume..um3.L)
      #Rename column
        colnames(p.8.23.11.efr)[grep('Total.biovolume', colnames(p.8.23.11.efr))] = 'BV.um3.L'  
      p.8.23.11.efr$BV.um3.L <- as.numeric(gsub(',', '', as.character(p.8.23.11.efr$BV.um3.L)))    
    #Simplify
      p.8.23.11.efr <- p.8.23.11.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.8.23.11.efr <- p.8.23.11.efr[!apply(p.8.23.11.efr, 1, all.na), ]                         #Remove rows of NA  


### 2012  
  #NEW DATA: Phytoplankton 2012 SRR BHR CFK EFR.txt
    p.2012 <- read.table(file.path(filePath,
                      'Phytoplankton 2012 SRR BHR CFK EFR.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE)                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    head(p.2012); str(p.2012)
    #Create 'Lake' vector
      p.2012$Lake <- ifelse(grepl('EFR', as.character(p.2012$Location)), 'EFR',
                      ifelse(grepl('SRR', as.character(p.2012$Location)), 'SRR',
                      ifelse(grepl('BHR', as.character(p.2012$Location)), 'BHR',
                      ifelse(grepl('CFK', as.character(p.2012$Location)), 'CFK', NA))))
    #Split out station
      p.2012$Station <- substr(as.character(p.2012$Location), start=5, stop=nchar(as.character(p.2012$Location)))
    #Rename Sample.ID
      p.2012$ID <- p.2012$Sample.ID
    #Sample Date
      p.2012$Date <- as.Date(as.character(p.2012$Sample.Date), format = '%Y%m%d')
    #Depth
      p.2012$Depth.ft <- p.2012$Sample.Depth
    #Taxa
      p.2012$Taxa <- as.character(p.2012$Taxa)
      p.2012$Taxa <- gsub("^\\s+|\\s+$", "", p.2012$Taxa)                                      #strip white spaces
    #Division
      p.2012$Division <- as.character(p.2012$Division)
      p.2012$Division <- gsub("^\\s+|\\s+$", "", p.2012$Division)                              #strip white spaces
    #Cells.L
      p.2012$Cells.L <- as.numeric(gsub(',', '', as.character(p.2012$Cells.liter)))            #gsub to remove commas from numbers (http://stackoverflow.com/questions/1523126/how-to-read-a-csv-file-in-r-where-some-numbers-contain-commas)
    #Biovolume
      #Tricky because the greek letter mu was included in column name when imported (Total.Biovolume..um3.L)
      #Rename column
        colnames(p.2012)[grep('Total.biovolume', colnames(p.2012))] = 'BV.um3.L'  
      p.2012$BV.um3.L <- as.numeric(gsub(',', '', as.character(p.2012$BV.um3.L)))
    #Simplify
      p.2012 <- p.2012[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.2012 <- p.2012[!apply(p.2012, 1, all.na),]                                              #Remove rows of NA


#### more 2012
  #NEW DATA: Phytoplankton 2012 EFR.txt
    p.2012.efr <- read.table(file.path(filePath,
                      'Phytoplankton 2012 EFR.txt'),
                      header = TRUE, fill = TRUE, na.string='', sep='\t', strip.white = TRUE)                                 #Use 'strip.white to remove leading and trailing white spaces in character strings
    str(p.2012.efr);head(p.2012.efr)
    #Create 'Lake' vector
      p.2012.efr$Lake <- ifelse(grepl('EFR', as.character(p.2012.efr$Location)), 'EFR', NA)                                       #Only contains EFR and NAs
    #Split out station
      p.2012.efr$Station <- substr(as.character(p.2012.efr$Location), start=5, stop=nchar(as.character(p.2012.efr$Location)))
    #Rename Sample.ID
      p.2012.efr$ID <- p.2012.efr$Sample.ID
    #Sample Date
      p.2012.efr$Date <- as.Date(as.character(p.2012.efr$Sample.Date), format = '%Y%m%d')
    #Depth
      p.2012.efr$Depth.ft <- p.2012.efr$Sample.Depth
    #Taxa
      p.2012.efr$Taxa <- as.character(p.2012.efr$Taxa)
      p.2012.efr$Taxa <- gsub("^\\s+|\\s+$", "", p.2012.efr$Taxa)
    #Division
      p.2012.efr$Division <- as.character(p.2012.efr$Division)
      p.2012.efr$Division <- gsub("^\\s+|\\s+$", "", p.2012.efr$Division)      
    #Cells.L
      p.2012.efr$Cells.L <- as.numeric(gsub(',', '', as.character(p.2012.efr$Cells.liter)))            #gsub to remove commas from numbers (http://stackoverflow.com/questions/1523126/how-to-read-a-csv-file-in-r-where-some-numbers-contain-commas)
    #Biovolume
      #Tricky because the greek letter mu was included in column name when imported (Total.Biovolume..um3.L)
      #Rename column
        colnames(p.2012.efr)[grep('Total.biovolume', colnames(p.2012.efr))] = 'BV.um3.L'  
      p.2012.efr$BV.um3.L <- as.numeric(gsub(',', '', as.character(p.2012.efr$BV.um3.L)))
    #Simplify
      p.2012.efr <- p.2012.efr[, c('Lake', 'Station', 'Date', 'Depth.ft', 'Taxa', 'Division', 'BV.um3.L', 'Cells.L')]                 #Extract columns of interest
      p.2012.efr <- p.2012.efr[!apply(p.2012.efr, 1, all.na), ]                                  #Remove rows of NA
}
      
### EFR HAB data
  #NEW DATA:  HAB: EFR HAB 2012 - 2013.txt
    p.efr.hab.9.20.12 <- read.table(file.path(filePath,
                      'EFR HAB 9-20-2012.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
    p.efr.hab.7.20.12 <- read.table(file.path(filePath,
                      'EFR HAB 7-20-2012.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')               
    p.efr.hab.9.24.12 <- read.table(file.path(filePath,
                      '2EFR20120924-CTDATA alg HAB.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')    
    p.efr.hab.9.20.12 <- read.table(file.path(filePath,
                      '2EFR20120920-CTDATA alg HAB.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')     
    p.efr.hab.8.28.12 <- read.table(file.path(filePath,
                      '2EFR20120828-CTDATA - alg HAB.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
                 
    p.efr.hab.5.22.13 <- read.table(file.path(filePath,
                      'HAB 2EFR 2013 05 22.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')       
      #Eliminate algae bloom sample
        p.efr.hab.5.22.13 <- p.efr.hab.5.22.13[p.efr.hab.5.22.13$Site != 'Algae Bloom Sample', ]
    p.efr.hab.6.5.13 <- read.table(file.path(filePath,
                      'HAB 2EFR 2013 06 05.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
    p.efr.hab.6.20.13 <- read.table(file.path(filePath,
                      'HAB 2EFR 2013 06 20.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
      #Eliminate beach
        p.efr.hab.6.20.13 <- p.efr.hab.6.20.13[-grep('Camp', p.efr.hab.6.20.13$Site), ]                                          
    p.efr.hab.7.02.13 <- read.table(file.path(filePath,
                      'HAB 2EFR 2013 07 02.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '') 
      #Eliminate beach
        p.efr.hab.7.02.13 <- p.efr.hab.7.02.13[-grep('Camp', p.efr.hab.7.02.13$Site), ]                                          
    p.efr.hab.7.17.13 <- read.table(file.path(filePath,
                      'HAB 2EFR20130717.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')
      #Eliminate beach
        p.efr.hab.7.17.13 <- p.efr.hab.7.17.13[-grep('CAMP', p.efr.hab.7.17.13$Site), ]                                                                                       
    p.efr.hab.8.5.13 <- read.table(file.path(filePath,
                      'HAB 2EFR20130805.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '')  
      #Eliminate beach
        p.efr.hab.8.5.13 <- p.efr.hab.8.5.13[-grep('CAMP', p.efr.hab.8.5.13$Site), ]     
    p.efr.hab.8.20.13 <- read.table(file.path(filePath,
                      'HAB 2EFR20130820.txt'), header = TRUE, fill = TRUE,
                      na.string='', sep='\t', strip.white = TRUE, comment.char = '') 
      #Eliminate beach
        p.efr.hab.8.20.13 <- p.efr.hab.8.20.13[-grep('CAMP', p.efr.hab.8.20.13$Site), ]  
                                        
  #merge data, then format new dataframe
    dfs1 <- list(p.efr.hab.9.20.12, p.efr.hab.7.20.12, p.efr.hab.9.24.12, p.efr.hab.9.20.12, p.efr.hab.8.28.12, p.efr.hab.5.22.13,
                    p.efr.hab.6.5.13, p.efr.hab.6.20.13, p.efr.hab.7.02.13, p.efr.hab.7.17.13, p.efr.hab.8.5.13, p.efr.hab.8.20.13)
    efr.hab <- merge_recurse(dfs1)            
    #Lake
      efr.hab$Lake <- substr(as.character(efr.hab$Site), start=2, stop=4)
    #Station
      efr.hab$Station <- substr(as.character(efr.hab$Site), start=5, stop=9)  
    #Date
      efr.hab$Date <- as.Date(substr(as.character(efr.hab$Site), start=10, stop=17), format = '%Y%m%d')
    #Depth.ft
      efr.hab$Depth.ft <- as.integer(substr(as.character(efr.hab$Site), start=22, stop=24))
    #Taxa
      efr.hab$Taxa <- as.character(efr.hab$Dominant.Genus)
      efr.hab$Taxa <- gsub("^\\s+|\\s+$", "", efr.hab$Taxa)                                     #strip out trailing/leading white space
    #Division
      efr.hab$Division <- 'Cyanophycota'
    #Cells.L
      efr.hab$Cells.L <- as.numeric(gsub(',', '', as.character(efr.hab$Total.Cyanobacteria.Cell.Count)))        #gsub to remove commas
    #HAB; index indicating that these are HAB response data
      efr.hab$HAB <- 1
    #Simplify
      efr.hab <- efr.hab[, -which(colnames(efr.hab) == 'CT..' | colnames(efr.hab) == 'Site')]
        str(efr.hab)

###                                
  ##MERGE ALL DATA FRAMES
    #list all data frames
      dfs <- list(p.2012, p.2012.efr, p.2011.brr.efr, p.8.23.11.efr, p.1998.efr, pre.1993, p.efr.99.08, p.1995, p.1992, efr.hab)
      
p.hab.73.12 <- merge_recurse(dfs)
      str(p.hab.73.12)
    #Add a '0' for non-HAB data
      p.hab.73.12[is.na(p.hab.73.12$HAB), 'HAB'] = 0
    #Strip leading and trailing white spaces in Taxa
      p.hab.73.12$Taxa <- gsub("^\\s+|\\s+$", "", p.hab.73.12$Taxa)  
    #Order    
      p.hab.73.12 <- p.hab.73.12[with(p.hab.73.12, order(Date, Station, Depth.ft)), ]
      
    #Write table for inspection
      write.table(p.hab.73.12, file = file.path(filePath,
                      'research/EPA/East Fork Reservoir/algae/Harsha Phyto HAB_1973_2012.PHYTO.txt'), row.names=F)

  ##Lisa's algal classification data
    clas <- read.delim(file.path(filePath,
                      'research/EPA/East Fork Reservoir/algae/Classification_EFR Phyto leu.txt'), header = TRUE, fill = TRUE,
                      na.strings=c('', 'NA'), sep='\t')                         #read.delim works while read.table doesn't.  Related to quote specification (http://stackoverflow.com/questions/3016333/r-why-does-read-table-stop-reading-a-file)
    clas <- clas[,c('Taxa', 'Class')]                                           #Remove notes column
    clas$Taxa <- as.character(clas$Taxa)
    clas$Taxa <- gsub("^\\s+|\\s+$", "", clas$Taxa)                             #Remove white spaces
    clas$Class <- as.character(clas$Class)
    clas$Class <- gsub("^\\s+|\\s+$", "", clas$Class)                           #Remove white spaces
  ##Merge Lisa's and Jade's  
    ld <- merge(p.hab.73.12, clas, all=T)
    ld <- ld[with(ld, order(Date, Station, Depth.ft)), ]                 
  #Write table for inspection
    write.table(ld, file = file.path(filePath,
                      'research/EPA/East Fork Reservoir/algae/Harsha Phyto HAB_1973_2012_Lisa.PHYTO.txt'), row.names=F)
  #Pull out algal taxa w/out a corresponding Class ID from Lisa
    no.class <- unique(ld[is.na(ld$Class) & !is.na(ld$Taxa), 'Taxa' ])          #Where Class is NA, but taxa is known.  Unique to reduce redundancies.  Send to Lisa for updating.
    no.class[order(no.class)]
    #write.table(no.class, file = file.path(filePath,
                      'research/EPA/East Fork Reservoir/algae/no.class.txt'), row.names=F)
    
      
#####################################################################################################################################
#####################################################################################################################################
###A BIT OF UNORGANIZED DATA EXPLORATION

############ld for 'Louisville District' house the final data.  Upgrade code accordingly.
  #Reset scipen (option for dictating display of scientific figures).  This was messing with display of site code above.  
    options(scipen=3)                      
  #Extract EFR data
    p.efr.hab.78.12 <- ld[ld$Lake == 'EFR', ]
  #Calculate total cells.L per lake, station, depth, and date
    t.cells <- with(p.efr.hab.78.12, aggregate(Cells.L ~ Lake + Station + Depth.ft + Date + HAB, FUN = sum))  
    colnames(t.cells)[which(colnames(t.cells) == 'Cells.L')] = 'T.Cells.L'      #rename
  #Calculate Blue-Green Cells.L per lake, station, depth, and date           
    bg.cells <- with(p.efr.hab.78.12[p.efr.hab.78.12$Class == 'Blue-green', ],
             aggregate(Cells.L ~ Lake + Station + Depth.ft + Date + HAB, FUN = sum))
    colnames(bg.cells)[which(colnames(bg.cells) == 'Cells.L')] = 'BG.Cells.L'   #rename
  #Calculate total biovolume per lake, station, depth, and date
    t.biov <- with(p.efr.hab.78.12, aggregate(BV.um3.L ~ Lake + Station + Depth.ft + Date + HAB, FUN = sum))
    colnames(t.biov)[which(colnames(t.biov) == 'BV.um3.L')] = 'T.BV.um3.L'      #rename
  #Calculate Blue-Green biovolume per lake, station, depth, and date           
    bg.biov <- with(p.efr.hab.78.12[p.efr.hab.78.12$Class == 'Blue-green', ],
             aggregate(BV.um3.L ~ Lake + Station + Depth.ft + Date + HAB, FUN = sum))
    colnames(bg.biov)[which(colnames(bg.biov) == 'BV.um3.L')] = 'BG.BV.um3.L'      #rename             
  #Merge aggregated data
    my.agg.data <- list(t.cells, bg.cells, t.biov, bg.biov)
    efr.agg <- merge_recurse(my.agg.data)
    str(efr.agg)
  #Prop BG by biovolume  
    efr.agg$prop.BG.BV <- efr.agg$BG.BV.um3.L / efr.agg$T.BV.um3.L             
    efr.agg[efr.agg$HAB == 1, 'prop.BG.BV'] = NA                                #This code eliminates any data from HAB monitoring where only BG where quantified.  Is NA anyway, since HAB response data have no biovolume, just in case
  #Prop BG by Cells counts
    efr.agg$prop.BG.Cells <- efr.agg$BG.Cells.L / efr.agg$T.Cells.L                 
    efr.agg[efr.agg$HAB == 1, 'prop.BG.Cells'] = NA                             #This code eliminates any data from HAB monitoring where only BG where quantified. 
  
  #Reinforce order
    efr.agg <- efr.agg[with(efr.agg, order(Date, Station, Depth.ft)), ]

#QUICK GGPLOT FIGURES
  #Formatting data for x-axis
    x_breaks <- seq(as.Date('1975/1/1'), as.Date('2014/1/1'), by = '2 year')
    x_labels <- as.character(x_breaks, format='%Y')
  #Biovolume plots
    #Total Biovolume
      ggplot(efr.agg, aes(Date, T.BV.um3.L)) + geom_point() + ylab('Total Biovolume') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
    #BG Biovolume      
      ggplot(efr.agg, aes(Date, BG.BV.um3.L)) + geom_point() + ylab(expression(paste('BG Biovolume ( ', mu, m^3, '/L)'))) + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
    #Proportion BG biovolume
      ggplot(efr.agg, aes(Date, prop.BG.BV)) + geom_point() + ylab('Proportion BG Biovolume') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
  #Cells per L plots                  
    #Total cells
      ggplot(efr.agg, aes(Date, T.Cells.L)) + geom_point() + ylab('Total Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) +
      scale_y_continuous(labels=comma)
    #BG cells  
      ggplot(efr.agg, aes(Date, BG.Cells.L)) + geom_point() + ylab('Blue Green Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) +
      scale_y_continuous(labels=comma)
    #Proportion BG cells  
      ggplot(efr.agg, aes(Date, prop.BG.Cells)) + geom_point() + ylab('Proportion Blue Green by Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
      
            
   







    


    
                        