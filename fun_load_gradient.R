# ==============================================  
# This function load the gas concentration from 
# the lifting tower from the sampling level at 30cm and 150cm, respectively.
# The exel file is complied at daily time scale every 60 seconds.
# ==============================================

fun_load_gradient <- function(filename="/work/ychen/CSSAGRI/GD_Raw/daily/20190714.xlsx",
                              ld_plot=FALSE,ld_spike=FALSE) {
#ld_plot=FALSE;ld_spike=FALSE
#filename="/work/ychen/CSSAGRI/GD_Raw/20190809.xls"
print(paste("start loading tildas file,", filename,sep=""))

#load the library to read the exel file
library(readxl)
#load the white space seperated ascii data
gd.raw <- read_excel(path=filename, col_names=TRUE, trim_ws=TRUE, 
                     col_types=c("text","date","numeric","numeric","numeric","numeric","numeric",
                     "date","numeric","numeric","numeric","numeric","numeric","numeric"))
# generate the timestamp by every 15 mins
n15 <- c(96)   
ini.date <- format(gd.raw$Datetime[1],format="%Y-%m-%d")
end.date <- gd.raw$Datetime[length(gd.raw$Datetime)]
#nn <- floor((end.date-ini.date)/15)
#print(paste("n15:",nn))
date.id   <- format(seq(as.POSIXct(ini.date, tz="UTC"), length.out=n15, by='15 min'),'%Y-%m-%d %H:%M:%S')
#
date.id <- as.POSIXct(date.id,tz="UTC")
#create tables for gradient gas conxentration
table.up <- data.frame()
table.dn <- data.frame()
table.gd <- data.frame()
#
for (i in 1:(n15-1)) {
#for (i in 1:10) {
#  use date.id to find the observations from raw data

   if (date.id[i] <= as.POSIXct("2019-08-08 14:45:00",tz="UTC") ) {
   tmp.n2o.dn <- median( gd.raw$'N2O(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.n2o.up <- median( gd.raw$'N2O(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
# 
   tmp.co2.dn <- median( gd.raw$'CO2(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE ) 
   tmp.co2.up <- median( gd.raw$'CO2(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
# 
   tmp.ch4.dn <- median( gd.raw$'CH4(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.ch4.up <- median( gd.raw$'CH4(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
#   
   tmp.h2o.dn <- median( gd.raw$'H2O(%)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.h2o.up <- median( gd.raw$'H2O(%)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
#   
   tmp.nh3.dn <- median( gd.raw$'NH3(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.nh3.up <- median( gd.raw$'NH3(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
# 
   tmp.no.dn <- median( gd.raw$'NO(ppb)_88PY'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.no.up <- median( gd.raw$'NO(ppb)_88PY'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
#   
   tmp.nox.dn <- median( gd.raw$'NOx(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="30cm")], na.rm=TRUE) 
   tmp.nox.up <- median( gd.raw$'NOx(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="150cm")], na.rm=TRUE) 
#   
   }else{
   tmp.n2o.dn <- median( gd.raw$'N2O(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.n2o.up <- median( gd.raw$'N2O(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
# 
   tmp.co2.dn <- median( gd.raw$'CO2(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE ) 
   tmp.co2.up <- median( gd.raw$'CO2(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
# 
   tmp.ch4.dn <- median( gd.raw$'CH4(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.ch4.up <- median( gd.raw$'CH4(ppm)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
#   
   tmp.h2o.dn <- median( gd.raw$'H2O(%)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.h2o.up <- median( gd.raw$'H2O(%)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
#   
   tmp.nh3.dn <- median( gd.raw$'NH3(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.nh3.up <- median( gd.raw$'NH3(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
# 
   tmp.no.dn <- median( gd.raw$'NO(ppb)_88PY'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.no.up <- median( gd.raw$'NO(ppb)_88PY'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
#   
   tmp.nox.dn <- median( gd.raw$'NOx(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="57cm")], na.rm=TRUE) 
   tmp.nox.up <- median( gd.raw$'NOx(ppb)'[(gd.raw$Datetime>date.id[i])&(gd.raw$Datetime<date.id[i+1])&(gd.raw$Elevation=="177cm")], na.rm=TRUE) 
#
   }
   dn.tmp <- data.frame(date=date.id[i], n2o=tmp.n2o.dn, co2=tmp.co2.dn, ch4=tmp.ch4.dn, h2o=tmp.h2o.dn, nh3=tmp.nh3.dn, no=tmp.no.dn, nox=tmp.nox.dn )
   up.tmp <- data.frame(date=date.id[i], n2o=tmp.n2o.up, co2=tmp.co2.up, ch4=tmp.ch4.up, h2o=tmp.h2o.up, nh3=tmp.nh3.up, no=tmp.no.up, nox=tmp.nox.up )
#
   #combine the table
   table.up <- rbind(table.up, up.tmp)
   table.dn <- rbind(table.dn, dn.tmp) 
#
}

#== sub-function for liner gap-fill ==
    fun_gap <- function(x) 
    {
    x <- as.numeric(x)
    #
    gap.id <-   which ( is.na(x)  )
    #   replace the first and the final elenments and subset the id 
    if  ( is.na(x[1]) )         x[1] <- x[1+1]
    if  ( is.na(x[length(x)]) ) x[length(x)] <- x[length(x)-1]
    gap.id <- subset(gap.id, (gap.id!=1 & gap.id!=length(x)))
    #
    for (i in gap.id) { 
         x[i] <- (x[i-1]+x[i+1])/2. 
    } 
    #   return gap-filled x
    return(x)
    }
#== end sub-function ==

# gap-fill thei gaps for upper level 
  table.up$n2o <- fun_gap(x=table.up$n2o)
  table.up$co2 <- fun_gap(x=table.up$co2)
  table.up$ch4 <- fun_gap(x=table.up$ch4)
  table.up$h2o <- fun_gap(x=table.up$h2o)
  table.up$nh3 <- fun_gap(x=table.up$nh3)
  table.up$no  <- fun_gap(x=table.up$no )
  table.up$nox <- fun_gap(x=table.up$nox)

# gap-fill thei gaps for lower level 
  table.dn$n2o <- fun_gap(x=table.dn$n2o)
  table.dn$co2 <- fun_gap(x=table.dn$co2)
  table.dn$ch4 <- fun_gap(x=table.dn$ch4)
  table.dn$h2o <- fun_gap(x=table.dn$h2o)
  table.dn$nh3 <- fun_gap(x=table.dn$nh3)
  table.dn$no  <- fun_gap(x=table.dn$no )
  table.dn$nox <- fun_gap(x=table.dn$nox)

# calculate the gradient by using (lower - upper)  
  table.gd <- table.dn-table.up

# assign the date.id to the table
  table.gd$date <- table.up$date


#load fun_desike() to remove the skipe
if(ld_spike) {
  source("fun_despike.R")
  table.gd$n2o <- fun_despike(x=table.gd$n2o,nstd=2.0)
  table.gd$co2 <- fun_despike(x=table.gd$co2,nstd=2.0)
  table.gd$ch4 <- fun_despike(x=table.gd$ch4,nstd=2.0)
  table.gd$h2o <- fun_despike(x=table.gd$h2o,nstd=2.0)
  table.gd$nh3 <- fun_despike(x=table.gd$nh3,nstd=2.0)
  table.gd$no  <- fun_despike(x=table.gd$no ,nstd=2.0)
  table.gd$nox <- fun_despike(x=table.gd$nox,nstd=2.0)
}
  

# plot the graident for checking
  if (ld_plot) {
  par(mfrow=c(2,3))
   plot(x=table.gd$date, y=table.gd$co2,type="b",main="CO2")
   plot(x=table.gd$date, y=table.gd$ch4,type="b",main="CH4")
   plot(x=table.gd$date, y=table.gd$h2o,type="b",main="H2O")
   plot(x=table.gd$date, y=table.gd$n2o,type="b",main="N2O")
   plot(x=table.gd$date, y=table.gd$nh3,type="b",main="NH3")
   plot(x=table.gd$date, y=table.gd$no,type="l",main="NO&NOX")
  par(new=TRUE)
   plot(x=table.gd$date,y=table.gd$nox, type="l",col="brown",
        xaxt="n",yaxt="n",xlab="",ylab="")
        axis(4)
        mtext("NOX",side=4,line=3)
   }
# return the gradient table
  return(gd.raw=table.gd)

} #end function
 
 
