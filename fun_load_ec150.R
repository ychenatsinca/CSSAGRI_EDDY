# Purpose load the EC 10Hz data
# First Date: 2019-03-06; Revise: 2019-07-12
# Author: Yi-Ying Chen


fun_load_ec150 <- function(dir.name="/work/ychen/CSSAGRI/EC_Dat/",
			     prefix="TOA5_Repair_6934.t_s_",yyyy="2019",mm="07",dd="11",hhmm="0915",
		             ld.plot=FALSE,ld.spec=FALSE){

#	 dir.name="/work/ychen/CSSAGRI/EC_Dat/";yyyy="2019";mm="07";dd="11";hhmm="0915";ld.plot="FALSE";ld.spec="FALSE"

#	file.name <- paste(dir.name,"/","CSSAGRI_",yyyy,"_",mm,"_",dd,"_",hhmm,".dat",sep="")
       	file.name <- paste(dir.name,"/",prefix,yyyy,"_",mm,"_",dd,"_",hhmm,".dat",sep="")
        
       	line.offset <- 2 
        line.skip   <- 1
        print(paste("working on file:", file.name))
        #try to load the file
	
	load.data <- read.csv(file=file.name, sep=",", stringsAsFactors=F,
		       skip=line.skip)[-(1:line.offset),]
        # check file length
	  if ( length(load.data$TIMESTAMP)!=9000) {
	     print(paste("please check file length!","Initial lines:",length(load.data$TIMESTAMP),sep=""))
	     load.data<-unique(load.data)
	     print(paste("unique the datastream !", "Final lines:", length(load.data$TIMESTAMP),sep=""))
	   }
	
	#list.out <- list(=load.data)i
	if (ld.plot) {
        plot(load.data$Ux,typ="l",ylim=c(-3,3))
        lines(load.data$Uy,add=T,col="red")
        lines(load.data$Uz,add=T,col="blue")
        legend("top", legend=c("black(U)","red(V)","blue(W)")) 
	}
        #check spectrum 
	if (ld.spec) {
	par(mfrow=c(2,2))
	plot(as.numeric(load.data$Uz),xlab="samples at 10Hz", ylab="wind speed (m/s)",typ="l")
	asp<-spectrum(as.numeric(load.data$Uz),plot=FALSE)
	plot(asp$spec~asp$freq, log="xy", main="W Spectrum")
	mtext(paste("Julian day:",dd,"- ",hhmm,sep=""),side=1,line=4,col="blue")
	plot(as.numeric(load.data$Ux),xlab="samples at 10Hz", ylab="wind speed (m/s)",ty="l")	
	asp<-spectrum(as.numeric(load.data$Ux),plot=FALSE)
        plot(asp$spec~asp$freq, log="xy", main="U Spectrum")
        x<-asp$freq; y<- x^(-5/3)*0.1
	lines(y~x,col="red")
	mtext(paste("Date:",mm,"-",dd," ",hhmm,sep=""),side=1,line=4,col="blue")
	}
	# return the datatable
	return(load.data)

}	



