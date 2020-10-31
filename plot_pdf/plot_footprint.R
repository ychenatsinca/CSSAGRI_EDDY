
#This script is the example of using openair package to plot the wind-rose chart 
#install the openair library
#install.packages("openair")


# load windrose library
library(openair)

#windRose(kc1, type=c("season","daylight"), angle = 10, width=1.5 )

# import flux.table from txt file
flux.table <- read.table(file="flux.table.test.dat", header=TRUE, row.names=NULL)

# limited the maximun footprint <= 100
flux.table$lev1.xmax[flux.table$lev1.xmax>=100] <- 100


#subset to conditions
flux.table.n1 <- subset(flux.table, flux.table$flag.n1n2=="N1")
flux.table.n2 <- subset(flux.table, flux.table$flag.n1n2=="N2")


#
#pdf(file="flux.footprint.pdf",width=6,height=6)
#
data.n1 <- data.frame(xmax=flux.table.n1$lev1.xmax,wd=flux.table.n1$lev1.wd,
		      le=flux.table.n1$lev1.le.1,n2o=flux.table.n1$lev1.n2o,
		      hno3=flux.table.n1$lev1.hno3,hono=flux.table.n1$lev1.hono)
#
data.n2 <- data.frame(xmax=flux.table.n2$lev1.xmax,wd=flux.table.n2$lev1.wd,
		      le=flux.table.n2$lev1.le.1,n2o=flux.table.n2$lev1.n2o,
		      hno3=flux.table.n2$lev1.hno3,hono=flux.table.n2$lev1.hono)
#
p1 <- polarPlot(mydata=data.n1,
	 wd="wd",x="xmax",k=10,
	 pollutant="le",upper=c(50.),main="1N condition")

p2 <- polarPlot(mydata=data.n2,
	 wd="wd",x="xmax", k=10,
	 pollutant="le",upper=c(50.),main="2N condition")
ld.go <-FALSE
if(ld.go) {	
# use polarPlot to show the distribution of the maximum footprint distance to the observation tower.
p3 <- polarPlot(mydata=data.n1,
	 wd="wd",x="xmax",
	 pollutant="n2o",upper=c(50.),main="1N condition")
p4 <- polarPlot(mydata=data.n2,
	 wd="wd",x="xmax",
	 pollutant="n2o",upper=c(50.),main="2N condition")

# use polarPlot to show the distribution of the maximum footprint distance to the observation tower.
p5 <- polarPlot(mydata=data.n1,
	 wd="wd",x="xmax",
	 pollutant="hno3",upper=c(50.),main="1N condition")
p6 <- polarPlot(mydata=data.n2,
	 wd="wd",x="xmax",
	 pollutant="hno3",upper=c(50.),main="2N condition")

# use polarPlot to show the distribution of the maximum footprint distance to the observation tower.
p7 <- polarPlot(mydata=data.n1,
	 wd="wd",x="xmax",#
	 pollutant="hono",upper=c(50.),main="1N condition")
p8 <- polarPlot(mydata=data.n2,
	 wd="wd",x="xmax",
	 pollutant="hono",upper=c(50.),main="2N condition")
#arrange plot obj to the layout by print/spilt funtion

#print(p1, split = c(1, 1, 2, 1))
#print(p2, split = c(2, 1, 2, 1), newpage = FALSE)
#print(p3, split = c(1, 2, 2, 4), newpage = FALSE)
#print(p4, split = c(2, 2, 2, 4), newpage = FALSE)
#print(p5, split = c(1, 3, 2, 4), newpage = FALSE)
#print(p6, split = c(2, 3, 2, 4), newpage = FALSE)
#print(p7, split = c(1, 4, 2, 4), newpage = FALSE)
#print(p8, split = c(2, 4, 2, 4), newpage = FALSE)


#dev.off()
}
