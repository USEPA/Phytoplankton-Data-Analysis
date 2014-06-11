###########################################################################################
#  TIME SERIES ANALYSIS OF LD PHYTOPLANKTON DATA
###########################################################################################

# LIBRARY
  library(mgcv)

# DATA----------
# The data prep workflow is GitHub -> Rproject -> PreviewProcessedData ->
# The objects to be inspected are the 'algae' and "ld.algae.agg" data frames.  They might
# be saved as a .R object and read in here, or produced in real time from 
# PreviewProcessedData and used here.  I will use the latter approach now.

# INSPECT AND FORMAT DATA
  str(algae)
  str(ld.algae.agg)
  #Add Day of Year (DOY) index.  Can use to examine periodicity.  See Taranu et al. 2012
  ld.algae.agg$DOY <- strptime(ld.algae.agg$rdate, format = '%Y-%m-%d')$yday+1
  #Add Year index.  Can use to asses interannual variability
  ld.algae.agg$year <- strptime(ld.algae.agg$rdate, format = '%Y-%m-%d')$year + 1900

# TIME SERIES MODEL
  ld.algae.agg.no.na.bg.bv <- ld.algae.agg[with(ld.algae.agg, !is.na(bg.BV.um3.L) &
                                                  !is.na(DOY) & !is.na(station) &
                                                  !is.na(year) & !is.na(depth_ft)), ]
  m1 <- gamm(bg.BV.um3.L ~ s(DOY) + s(year),
             random = list(lake =~ 1),
            data = ld.algae.agg.no.na.bg.bv)

  op <- par(mfrow = c(1,2))
  plot(m1$gam, select = c(1))
  plot(m1$gam, select = c(2))
  par(op)
  