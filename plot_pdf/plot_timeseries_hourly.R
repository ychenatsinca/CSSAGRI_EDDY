
# Revised date: 2020-03-31
# the horly flux data will be presented in the final time-series plot
# import flux.table from txt file

#flux.table <- read.table(file="/work/ychen/scripts/R/Rscripts/CSSAGRI/plot_pdf/flux.table.20190806_20191114.dat", heade=TRUE, row.names=NULL,sep=",")
#flux.table <- read.table(file="/work/ychen/scripts/R/Rscripts/CSSAGRI/plot_pdf/flux.table.20190707-20190810.dat", heade=TRUE, row.names=NULL,sep=",")
flux.table <- read.table(file="/work/ychen/scripts/R/Rscripts/CSSAGRI/plot_pdf/flux.table.20190707-20190812_v1.1.dat", heade=TRUE, row.names=NULL,sep=",")
#

flux.table$date <- format(as.Date(flux.table$date.time),format="%m-%d")
flux.table$hour <- format(as.POSIXlt(flux.table$date.time),format="%H")
flux.table$date.hour <- paste(format(as.Date(flux.table$date.time),format="%Y-%m-%d")," ",format(as.POSIXlt(flux.table$date.time),format="%H"),":00:00", sep="") 
#subset to high quility fluxes 
flux.table <- subset(flux.table, ((flux.table$flag.stat==TRUE)|(flux.table$flag.itc==TRUE)) )
#flux.table <- subset(flux.table, (flux.table$flag.fpt==TRUE))
#
# limited the maximun footprint <= 100
flux.table$lev1.xmax[flux.table$lev1.xmax>=100] <- 100
#load despike function
source(file="../fun_despike.R")
flux.table$lev1.n2o <- fun_despike(x=flux.table$lev1.n2o, nstd=2.0)

# aggregate 15min flux to hourly flux 

aggdata  <- aggregate(flux.table , by =list(flux.table$date.hour), FUN = mean, na.rm=TRUE)  

# calculate days 
nx <- length (flux.table$date.time) 
date_diff <- as.Date(as.character(flux.table$date.time[nx]),format="%Y-%m-%d") -
             as.Date(as.character(flux.table$date.time[1]),format="%Y-%m-%d") 
date_diff <- as.integer(date_diff)

# generate an hourly table
time.stamp.hour <- data.frame(Group.1 = as.character( seq(as.POSIXct(flux.table$date.time[4]), as.POSIXct(flux.table$date.time[nx]), "hour")) )

# merge the table 
flux.table.hr <- merge(aggdata, time.stamp.hour, by = "Group.1", all=TRUE)  


flux.table <-  flux.table.hr

# output the file

library("WriteXLS")

#WriteXLS( flux.table.hr, ExcelFileName="flux.table.20190707-20190810_hourly.xls",col.names=TRUE)
WriteXLS( flux.table.hr, ExcelFileName="flux.table.20190707-20190812_hourly.xls",col.names=TRUE)


# calculate the accumulative N-flux
# replace na to zero
#flux.table[is.na(flux.table)]<-0
#flux.table$acc.n.fluxes <-  flux.table$lev1.hno3*14+flux.table$lev1.hono*14+ flux.table$lev1.n2o*28+
#                            +flux.table$lev1.no.gd*14*2+flux.table$lev1.nox.gd*14*2




#despike 
#flux.table$acc.n.fluxes <- fun_despike(flux.table$acc.n.fluxes,nstd=3.5)
# do the loop for the accumulative sume
#flux.table$acc.n.fluxes <- cumsum(flux.table$acc.n.fluxes)


#pdf("flux.time.series.20190806-20191114.QCQA.pdf",width=18,height=12,pointsize=18)
#pdf("flux.time.series.20190707-20190810.QCQA.pdf",width=18,height=12,pointsize=18)

par( mai=c(0.6,0.6,0.3,0.3),cex.main=1.5)
layout( matrix(c(1,2,3,4),4,1,byrow=FALSE) )
#soil ferterlization 
dd<-levels(as.factor(flux.table$date))
x <- c(which(dd=="07-07")[1],
       which(dd=="07-09")[1],
       which(dd=="07-19")[1],
       which(dd=="07-22")[1]  )
#soil tillage
xx <- c(which(dd=="07-11")[1],
        which(dd=="07-14")[1],
        which(dd=="08-06")[1],
        which(dd=="08-07")[1]  )

y <- c(-1,1)

plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.hno3*14,lwd="3",type="l",
                  main=expression( paste( "(a) HN",O[3],", (ng/(",m^{2}*s,")",sep="" )),
	          outline=FALSE, col="lightblue", ylim=c(-0.005,0.015*20),xlab="",ylab="")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)



plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.hono*14,lwd="3",type="l",
                  main=expression( paste( "(b) HONO, (ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-1E-3,0.06*20), outline=FALSE,col="brown",xlab="",ylab="")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)


y <- c(-50,650)

plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.n2o*28,lwd="3",type="l",
                  main=expression( paste("(c)", N[2],"O, (ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-5E0,20*30), outline=FALSE,col="gray",xlab="",ylab="" )
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)


plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.le.1,lwd="3",type="l",
                 main=expression( paste("(d)", H[2],"O","flux W/(",m^{2},")",sep="" )),
	         ylim=c(-1E1,5E2),outline=FALSE,col="lightgray",xlab="",ylab="")

rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)

#dev.off()

dev.new()
par( mai=c(0.6,0.6,0.3,0.3),cex.main=1.5)
layout( matrix(c(1,2,3,4),4,1,byrow=FALSE) )

ld.go<-TRUE
if(ld.go){
#dev.new()
#par(mfrow=c(3,2),mai=c(0.75,0.75,0.2,0.2) )

y<-c(-2,5)
plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.no.gd*14*2,lwd="3",type="l",
	         main=expression( paste( "(e) NO, ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="lightblue",ylim=c(-1,4),xlab="",ylab="")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)



plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.nox.gd*14*2,lwd="3",type="l",
	         main=expression( paste( "(f) NOx, ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="blue",ylim=c(-1,4),xlab="",ylab="")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)


#replace by accunulative N-fluxes
#plot(x=(flux.table$date), y=flux.table$lev1.nh3*14,
#                 main=expression( paste( "N",H[3],", N-flux ng/(",m^{2}*s,")",sep="" )),
#	         outline=FALSE,col="forestgreen",)
y<-c(-1,4)
#plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$acc.n.fluxes*1800*(10^-12)*10000,
#                  main="(g) Acc Total N-efflux , (Kg/ha)", outline=FALSE,col="forestgreen",xlab="",ylab="" )
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)

y<-c(-2,2)
plot(x=as.POSIXct(flux.table$Group.1), y=flux.table$lev1.co2.1,lwd="3",type="l",
                 main=expression( paste( "(h) C",O[2]," flux mg/(",m^{2}*s,")",sep="" )),
	         ylim=c(-2E0,1E0),outline=FALSE,col="green",xlab="",ylab="")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)


}

#dev.off()

#pdf("flux.diurnal.mean.20190806-20191114.QCQA.pdf",width=18,height=12)
#pdf("flux.diurnal.mean.20190707-20190810.QCQA.pdf",width=18,height=12,pointsize=18)




#----- subset the time series to the epidemic period ------  
# flux.table <- subset(flux.table, ( flux.table$date < "07-09") & (flux.table$date > "07-09") )


ld_go <- FALSE

if(ld_go) {
#plot diurnal 
par( mai=c(0.5,0.5,0.2,0.2))
layout( matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=FALSE) )

flux.table$hh <- as.factor(substr(flux.table$date.time,start=12,stop=13))

plot(x=flux.table$hh, y=flux.table$lev1.hno3*14.,
                  main=expression( paste( "HN",O[3]," ng/(",m^{2}*s,")",sep="" )),
	          outline=FALSE, col="lightblue", ylim=c(-0.1,0.5))

plot(x=flux.table$hh, y=flux.table$lev1.hono*14.,
                  main=expression( paste( "HONO ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-1E-3,2), outline=FALSE,col="brown")

plot(x=flux.table$hh, y=flux.table$lev1.n2o*28.,
                  main=expression( paste( N[2],"O ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-5E0,500), outline=FALSE,col="gray" )


plot(x=flux.table$hh, y=flux.table$lev1.le.1,
                 main=expression( paste( H[2],"O"," flux W/(",m^{2},")",sep="" )),
	         ylim=c(-1E1,5E2),outline=FALSE,col="lightgray")

plot(x=flux.table$hh, y=flux.table$lev1.no.gd*14.,
	         main=expression( paste( "NO ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="lightblue")

plot(x=flux.table$hh, y=flux.table$lev1.nox.gd*14.,
	         main=expression( paste( "NOx ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="blue")


plot(x=flux.table$hh, y=flux.table$lev1.nh3*14.,
                 main=expression( paste( "N",H[3]," ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="forestgreen",)

plot(x=flux.table$hh, y=flux.table$lev1.co2.1,
                 main=expression( paste( "C",O[2]," mg/(",m^{2}*s,")",sep="" )),
	         ylim=c(-1E0,1E0),outline=FALSE,col="green")


#dev.off()
}

