# PREVIEW PHYTOPLANKTON DATA THAT MATT IMPORTED

# LIBRARIES-----------------------------------------------------
  library(ggplot2)
  library(reshape)

# READ IN AND FORMAT .csv FROM OUTPUT FOLDER-------------------------
# Reading from output/processed_data folder
  algae <- read.table("processed_data/algae.csv", sep = ",", header = TRUE, as.is = TRUE)
  head(algae)
  str(algae)
  algae$rdate <- as.Date(as.character(algae$date), format = '%Y%m%d')

# Review number of samples, lakes, dates, etc
  table(algae$lake, substr(algae$date, 1,4))  # data from all lakes in 92, only EFR >92
  table(algae$rdate, algae$hab)  # Only two dates of HAB sampling entered

# POPULATE 'CLASS' FIELD----------------------------------------
  unique(algae$class)  # Not filled out yet
  algae <- subset(algae, select = -c(class))  # Remove class field.  Merge it in below.
  # Lisa's algal classification data
    clas <- read.delim('originalData/algae/Classification_EFR Phyto leu.txt', header = TRUE, fill = TRUE,
                   na.strings=c('', 'NA'), sep='\t', as.is = TRUE)  # read.delim works while read.table doesn't.  Related to quote specification (http://stackoverflow.com/questions/3016333/r-why-does-read-table-stop-reading-a-file)
    str(clas)
    clas <- clas[,c('Taxa', 'Class')]  # Remove notes columns
    names(clas) <- c('taxa', 'class')  # Change names per phytoplankton data precedent
    clas$taxa <- gsub("^\\s+|\\s+$", "", clas$taxa)  # Remove white spaces
    clas$class <- gsub("^\\s+|\\s+$", "", clas$class)  # Remove white spaces
  # Merge clas with phytoplankton data  
    ld.algae <- merge(algae, clas, by = 'taxa', all=T)  # 'ld' for Louisville District
    str(ld.algae)  
  # Pull out algal taxa w/out a corresponding slass ID from Lisa
    no.class <- unique(ld[is.na(ld$class) & !is.na(ld$taxa), 'taxa' ])  # Where Class is NA, but taxa is known.  Unique to reduce redundancies.  Send to Lisa for updating.
    no.class[order(no.class)]  # Only a few, very god.
    #write.table(no.class, file = file.path(filePath,
    #'research/EPA/East Fork Reservoir/algae/no.class.txt'), row.names=F)

# A FEW VERY BASIC FIGURES-----------------------------------------
  # EFR
    ggplot(algae[algae$lake == 'EFR',], aes(rdate, cell_per_l)) + 
      geom_point(aes(color=hab))
  # All data
    ggplot(algae, aes(rdate, cell_per_l)) + geom_point(aes(color=lake))

# AGGREGATE AND CALCULATE DERIVED QUANTITIES-----------------------
  # Algal counts first
    # Calculate total cells.L per lake, station, depth, and date
      t.cells <- with(ld.algae, aggregate(cell_per_l ~ lake + station + depth_ft + rdate + hab, FUN = sum))  
      colnames(t.cells)[which(colnames(t.cells) == 'cell_per_l')] = 't.cell_per_l'  # Rename
    # Calculate Blue-Green cells_per_l per lake, station, depth, and date           
      bg.cells <- with(ld.algae[ld.algae$class == 'Blue-green', ],
                       aggregate(cell_per_l ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(bg.cells)[which(colnames(bg.cells) == 'cell_per_l')] = 'bg.cell_per_l'   # Rename

  # Algal biovolumes
    # Calculate total biovolume per lake, station, depth, and date
      t.biov <- with(ld.algae, aggregate(BV.um3.L ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(t.biov)[which(colnames(t.biov) == 'BV.um3.L')] = 't.BV.um3.L'  # Rename
    # Calculate Blue-Green biovolume per lake, station, depth, and date           
      bg.biov <- with(ld.algae[ld.algae$class == 'Blue-green', ],
                    aggregate(BV.um3.L ~ lake + station + depth_ft + rdate + hab, FUN = sum))
      colnames(bg.biov)[which(colnames(bg.biov) == 'BV.um3.L')] = 'bg.BV.um3.L'  # Rename  

  # Merge aggregated data
    ld.algae.list <- list(t.cells, bg.cells, t.biov, bg.biov)
    ld.algae.agg <- merge_recurse(ld.algae.list)
    str(ld.algae.agg)

  # Calculate Prop BG by biovolume  
    ld.algae.agg$prop.bg.BV <- with(ld.algae.agg, bg.BV.um3.L / t.BV.um3.L)             
    ld.algae.agg[ld.algae.agg$hab == TRUE, 'prop.bg.BV'] = NA  # This code eliminates any data from HAB monitoring where only BG where quantified.  Is NA anyway, since HAB response data have no biovolume, just in case
  
  # Calculate Prop BG by Cells counts  
    ld.algae.agg$prop.bg.cell <- with(ld.algae.agg, bg.cell_per_l / t.cell_per_l)                 
    ld.algae.agg[ld.algae.agg$hab == TRUE, 'prop.bg.cell'] = NA                             #This code eliminates any data from HAB monitoring where only BG where quantified. 

  # Reinforce order
    ld.algae.agg <- ld.algae.agg[with(ld.algae.agg, order(rdate, station, depth_ft)), ]

# QUICK GGPLOT FIGURES-------------------------------------
  # Formatting data for x-axis
    x_breaks <- seq(as.Date('1975/1/1'), as.Date('2014/1/1'), by = '2 year')
    x_labels <- as.character(x_breaks, format='%Y')

  # Biovolume plots
  # Total Biovolume
    ggplot(ld.algae.agg, aes(rdate, t.BV.um3.L)) + geom_point() + ylab('Total Biovolume') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)
    # BG Biovolume      
      ggplot(ld.algae.agg, aes(rdate, bg.BV.um3.L)) + geom_point() + ylab(expression(paste('BG Biovolume ( ', mu, m^3, '/L)'))) + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    # Proportion BG biovolume
      ggplot(ld.algae.agg, aes(rdate, prop.bg.BV)) + geom_point() + ylab('Proportion BG Biovolume') + 
        scale_x_date(breaks=x_breaks, labels = x_labels)
    
  # Cells per L plots                  
  # Total cells
    ggplot(ld.algae.agg, aes(rdate, t.cell_per_l)) + geom_point() + ylab('Total Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) 
  # BG cells  
    ggplot(ld.algae.agg, aes(rdate, bg.cell_per_l)) + geom_point() + ylab('Blue Green Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels) 
  # Proportion BG cells  
    ggplot(ld.algae.agg, aes(rdate, prop.bg.cell)) + geom_point() + ylab('Proportion Blue Green by Cell count') + 
      scale_x_date(breaks=x_breaks, labels = x_labels)

# PREVIEW WATER CHEM DATA THAT MATT IMPORTED
# Reading from output/processed_data folder
  chem <- read.table("processed_data/water_quality.csv", sep = ",", header = TRUE, as.is = TRUE,na.strings=c("", "NA"))
  head(chem)
  str(chem)
  chem$rdate <- as.Date(as.character(chem$date), format = '%Y%m%d')

# Review number of samples, lakes, dates, etc
table(algae$lake, substr(algae$date, 1,4))  # data from all lakes in 92, only EFR >92
table(algae$rdate, algae$hab)  # Only two dates of HAB sampling entered
