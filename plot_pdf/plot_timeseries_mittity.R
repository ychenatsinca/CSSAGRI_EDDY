
# import flux.table from txt file
flux.table <- read.table(file="/work/ychen/scripts/R/Rscripts/CSSAGRI/plot_pdf/flux.table.20190707-20190810.dat", header=TRUE, row.names=NULL,sep=",")
#subset to high quility fluxes 
flux.table <- subset(flux.table, ((flux.table$flag.fpt==TRUE)|(flux.table$flag.stat==TRUE)))
# limited the maximun footprint <= 100
flux.table$lev1.xmax[flux.table$lev1.xmax>=200] <- 200
#load despike function
source(file="../fun_despike.R")
flux.table$lev1.n2o <- fun_despike(x=flux.table$lev1.n2o, nstd=1.0)
flux.table$lev1.nox.gd <- fun_despike(x=flux.table$lev1.nox.gd, nstd=1.0)


# calculate the accumulative N-flux
# replace na to zero
flux.table[is.na(flux.table)]<-0
flux.table$acc.n.fluxes <-  flux.table$lev1.hno3*14+flux.table$lev1.hono*14+ flux.table$lev1.n2o*28+
                            +flux.table$lev1.no.gd*14*2+flux.table$lev1.nox.gd*14*2
#despike 
flux.table$acc.n.fluxes <- fun_despike(flux.table$acc.n.fluxes,nstd=3.5)
# do the loop for the accumulative sume
flux.table$acc.n.fluxes <- cumsum(flux.table$acc.n.fluxes)



flux.table$date <- format(as.Date(flux.table$date.time),format="%y-%m-%d")

x.date <- levels(as.factor(flux.table$date))
n2o.daily.mean <- boxplot(lev1.n2o ~ date, data=flux.table, plot=FALSE)$stats[3,] # using boxplot funtion to get medium values from each day  
nox.daily.mean <- boxplot(lev1.nox.gd ~ date, data=flux.table, plot=FALSE)$stats[3,] # using boxplot funtion to get medium values from each day  
#
#pdf("flux.time.series.20190707-20190810.QCQA.pdf",width=18,height=12,pointsize=18)
par( mai=c(0.6,0.6,0.3,0.3))
#layout( matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=FALSE) )
#soil ferterlization 
#dd<-levels(as.factor(flux.table$date))
#x <- c(which(dd=="07-07")[1],
#       which(dd=="07-09")[1],
#       which(dd=="07-19")[1],
#       which(dd=="07-22")[1]  )
#soil tillage
#xx <- c(which(dd=="07-11")[1],
#        which(dd=="07-14")[1],
#        which(dd=="08-06")[1],
#        which(dd=="08-07")[1]  )

#y <- c(-1,1)


#plot(x=as.Date(flux.table$date),
#     y=flux.table$lev1.n2o,type="p",pch=1,col="gray",cex=0.5, ylim=c(-5,35))

#points(x=as.Date(x.date), 
#     y=n2o.daily.mean, type="p", col="black", pch=19,cex=1.0, add=TRUE) 
#     main=expression( paste("(c)", N[2],"O, N-efflux ng/(",m^{2}*s,")",sep="" )),
#     ylim=c(-50,800), col="gray" )

plot(x=as.Date(flux.table$date),
     y=flux.table$lev1.nox.gd,type="p",pch=1,col="gray",cex=0.5,ylim=c(-0.1,0.3))

points(x=as.Date(x.date), 
     y=nox.daily.mean, type="p", col="black", pch=19,cex=1.0, add=TRUE) 
#
#rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
#rect(x[3],y[1],x[4],y[2],col= 'brown',density=10)
#rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
#rect(xx[3],y[1],xx[4],y[2],col= 'gray',density=10)

#dev.off()


