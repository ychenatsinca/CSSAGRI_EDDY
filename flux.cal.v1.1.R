# ------------------------------------------------------------
#
#     The program calculates the eddy flux from two EC systems
#     open path EC-150 & Young 81000 (H2O, CO2, Ta, u, v, w),
#     closed path: TILDAS(HNO3, N2O, HONO, H2O).
#     Surface fluxes from gradient approach
#     all gd.fluxes reference to the sensible heat flux or N2O flux  
#     Author: Yi-Ying Chen (RCEC, Taiwan) 
#     Contact: yiyingchen@gate.sinica.edu.tw
#     First Date: 2019-07-18
#     Revised: 2020-04-07
#     Version: 1.1
#
#     Data Processing Tasks: 
#     1.detrend, 2.despike, 3.lag time fixed, 4.co-spectra check
#     5.coordinate rotation, 6.turbulent test, 7.stationary test
#     8.footprint test, 9.wpl correction(not yet) 
#     10.gradient flux    
# ------------------------------------------------------------

# initial  tables
  flux.table <- data.frame()
  spec.table <- data.frame()
#load source file for the functions used in the program
  source("fun_load_tildas.R")
  source("fun_load_ec150.R")
  source("fun_load_gradient.R")
  source("fun_despike.R")
  source("fun_ecflux_for.R")
  source("fun_turbulent_test.R")
#load library for the signal processing
  library("signal")
  library("itsmr")
  library("pracma")

#--- set the date for analysis 
# search tildas files 
  tildas.dir   <- c("/work/ychen/CSSAGRI/EC_Raw/TILDAS/")
  ec150.dir    <- c("/work/ychen/CSSAGRI/EC_Raw/EC150_15min/")
  gradient.dir <- c("/work/ychen/CSSAGRI/GD_Raw/")

  tildas.fnames <- list.files(pattern="*.str",path=tildas.dir)

  print(tildas.fnames)
#for (fid in 1:length(tildas.fnames)) { 
for (fid in 1:51) { 
# 1-51 cabbage 
#-- get date time information from tildas files  
  yyyy   <- formatC(substr(tildas.fnames[fid],start=1,stop=4),flag="0",format="d",width=4)
  mm     <- formatC(substr(tildas.fnames[fid],start=5,stop=6),flag="0",format="s",width=2)
  dd     <- formatC(substr(tildas.fnames[fid],start=7,stop=8),flag="0",format="s",width=2)
  target <- substr(tildas.fnames[fid], start=10,stop=11) 

#--- load the data from tildas 
  obs.date <- paste(yyyy,mm,dd,sep="")
  try( raw.tildas <- fun_load_tildas(filename=paste(tildas.dir,tildas.fnames[fid], sep="")) )
# get initial date from tildas
  ini.hh <- as.integer(substr(raw.tildas$timestamp[1],start=1,stop=2))  
  ini.mm <- as.integer(substr(raw.tildas$timestamp[1],start=4,stop=5))
  ini.ss <- as.numeric(substr(raw.tildas$timestamp[1],start=7,stop=10))
# skip the non zero start and shift to next time 15 min 
# ceiling the initial minumtes to every 15min
  ini.mm <- ceiling(ini.mm/15)*15
  if(ini.mm==60){ini.mm=0;ini.hh=ini.hh+1}
# get final date from tilda
  fin.hh <- as.integer(substr(tail(raw.tildas$timestamp,n=1),start=1,stop=2))
  fin.mm <- as.integer(substr(tail(raw.tildas$timestamp,n=1),start=4,stop=5))
# switch 00:00 to 24:00 
  if(fin.hh==0)fin.hh=24
  n15  <- (fin.hh-ini.hh)*4 + floor(fin.mm/15) - 2    
# create a sequency of date id
  ini.stamp <- c(paste(yyyy,"-",mm,"-",dd," ",
		     formatC(ini.hh,flag="0",format="s",width=2),":",
		     formatC(ini.mm,flag="0",format="s",width=2),":00",sep="")) 
# forward one step for none zero seconds start
  if(ini.ss!= 0) {ini.stamp <- as.POSIXct(ini.stamp, tz="") + 60*15}
  ini.stamp <- as.POSIXct(ini.stamp, tz="") + 60*15

  print( paste("EC150 starting date:",ini.hh,":",ini.mm,":",ini.ss, 
  	     "Total 15min steps:",n15, "For",target,"Filed!", sep=" ") )  
  date.id   <- format(seq(as.POSIXct(ini.stamp, tz=""), length.out=n15, by='15 min'),'%Y-%m-%d %H:%M:%S')
# date.id <- format(seq(as.POSIXct("2019-03-14 12:00:00", tz=""), length.out=10, by='15 min'),'%Y_%j %H:%M')
  date.yyyy <- substr(date.id,start=1,stop=4)
  date.mm   <- substr(date.id,start=6,stop=7)
  date.dd   <- substr(date.id,start=9,stop=10)
  date.hhmmss <- substr(date.id,start=12,stop=19)
  time.stamp <- format(paste(substr(date.hhmmss,start=1,stop=2),":",
			   substr(date.hhmmss,start=4,stop=5),":00.0",sep=""))


  #------ load gas concentration difference from exel file (compiled from gradient observation system)
  # set gd_filename
  gd_filename <- c(paste(yyyy,mm,dd,".xls",sep=""))
  # load the fun_load_gradient(), this funtion will return a table contains the gradient of different gases
  try( gd.raw <- fun_load_gradient(filename=paste(gradient.dir,"/",gd_filename,sep=""), ld_plot=TRUE) )

  #  
  #par(mfrow=c(2,2),mai=c(1,1,1,1) )
  #
  ld.go<-TRUE
  #
  # load library signal for filtering and despike with a crtiria of 3.5-std 
     # library("signal")
     # source("fun_despike.R")
     # --- using band pass filter to correct frequency response of closed-path EC measurenment --- 
     #create the band-pass filter
     bf.h2o <- butter(1, c(0.0,0.9), type="pass")
     #apply the filter to the h2o noisy signal 
     raw.tildas$H2O.2 <- filtfilt(bf.h2o, as.numeric(raw.tildas$H2O.2)) 
     #create the band-pass filter
     bf.n2o <- butter(1, c(0.0,0.9), type="pass")
     #apply the filter to the h2o noisy signal 
     raw.tildas$N2O <- filtfilt(bf.n2o, as.numeric(raw.tildas$N2O)) 
     #create the band-pass filter
     bf.hno3 <- butter(1, c(0.0,0.9), type="pass")
     #apply the filter to the h2o noisy signal 
     raw.tildas$HNO3 <- filtfilt(bf.hno3, as.numeric(raw.tildas$HNO3))
     #create the band-pass filter
     bf.hono <- butter(1, c(0.0,0.9), type="pass")
     #apply the filter to the h2o noisy signal 
     raw.tildas$HONO <- filtfilt(bf.hono, as.numeric(raw.tildas$HONO)) 

  # load library pracma for detrending
  # library("pracma")
  # load fun_tubulent_test() function(x,ustar,SH,LE,z,d0,Ta,roha,L,flag.itc=TRUE,flag.stat=TRUE)
  # to get nstationary flag and integral turbulent flag 
  #
  # source("fun_turbulent_test.R") 
  # open pdf device 
  # pdf("cssargi_detail.pdf")
  w.id1 <- c(1) 
  w.id2 <- c(9000)

  if(ld.go) {
     for (i in 1:length(date.yyyy)) {
     # for (i in 10) {
     # start flux.cal
     #check file exist 
	     file.name <- paste(ec150.dir,"/","CSSAGRI_",date.yyyy[i],"_",date.mm[i],"_",date.dd[i],"_",
			       substr(date.hhmmss[i],start=1,stop=2),substr(date.hhmmss[i],start=4,stop=5),".dat",sep="")
	     print(file.name)
	     if(file.exists(file.name)==TRUE) {
	     raw.data <- fun_load_ec150(dir.name=ec150.dir, prefix="CSSAGRI_",
			        yyyy=date.yyyy[i],mm=date.mm[i],
			        dd=date.dd[i],
				hhmm=paste(substr(date.hhmmss[i],start=1,stop=2),substr(date.hhmmss[i],start=4,stop=5),sep=""),
		                ld.plot=FALSE,ld.spec=FALSE)
             #if loading file gets error skip to next 15 mins 
	     }else{ print("NO EC-150 file"); next}
     
     #---- find the time slot for the 15min interval from gd.table
     tmp.datetime <-  as.POSIXct(as.character(date.id[i]),tz="UTC")
     gd.sub <-  subset(gd.raw, gd.raw$date==tmp.datetime)
     print( paste("gradient table:",gd.sub))     
 

     #remove na column in raw.data
     raw.data$Uzc <- (as.numeric(raw.data$Uzc))
     raw.data<-na.omit(raw.data)

     w.length <- length(raw.data$TIMESTAMP)
     
     # use time.stamp to find the data from tildas timeseries
     if ( length(which(raw.tildas$timestamp==time.stamp[i]))==0)
     {
	    w.id1 <- w.id2
            w.id2 <- w.id2 + w.length -1
     } 
     else
     {    
     w.id1 <- which(raw.tildas$timestamp==time.stamp[i])[1]
     w.id2 <- w.id1 + w.length - 1 
     }
     #
     if (w.id1 > w.id2) { print("break loop! !check data!"); break}
     #
     print( paste("w.id1: ",w.id1,"w.id2: ",w.id2,sep=" "))
     
     # exit if the final elenment is greater tildas data length       
     if (w.id2 > length(raw.tildas$H2O.2)){print("skip the final 15min!"); break}
     
     # create inital data for the evaluation  
     # --- detrend of the time series data
     raw.tildas$H2O.2[w.id1:w.id2]  <- detrend(raw.tildas$H2O.2[w.id1:w.id2], tt="linear")
     raw.tildas$N2O[w.id1:w.id2]    <- detrend(raw.tildas$N2O[w.id1:w.id2],   tt="linear")
     raw.tildas$HNO3[w.id1:w.id2]   <- detrend(raw.tildas$HNO3[w.id1:w.id2],  tt="linear")
     raw.tildas$HONO[w.id1:w.id2]   <- detrend(raw.tildas$HONO[w.id1:w.id2],  tt="linear")
     print("Find Lag time ...")
     # --- using acf to find the maximum corelation of two covariance terms ---
     # --- the unit conversion was also applied to all gas specis ---
     # --- H2O ppb(1E-9) to mg/m^3 (1E-6)
     # --- HNO3 ppb(1E-9) ~ g/m3 (1E-9) to nmol(1/63)
     # --- HONO ppb(1E-9)  to nmol (1/47)
     # --- N2O  ppb(1E-9)  to nmol (1/44)
     # find H2O lag time
     aa <- ccf(x= (as.numeric(raw.tildas$H2O.2[w.id1:w.id2])-mean(as.numeric(raw.tildas$H2O.2[w.id1:w.id2]),na.rm=T)), 
	       y= (as.numeric(raw.data$Uzc)), lag.max = 600*1.0, plot=FALSE)
     lag.id = (aa$lag[which.max((aa$acf))])
     print(paste("H2O lag time:",lag.id," was found! ",sep=""))
     # copy tildas dat to raw data
     raw.data$tildas.H2O  <- as.numeric(raw.tildas$H2O.2[(w.id1+lag.id):(w.id2+lag.id)])#*1E-6
     # find HNO3 lag time
     # find HNO3 lag time
     aa <- ccf(x= (as.numeric(raw.tildas$HNO3[w.id1:w.id2])-mean(as.numeric(raw.tildas$HNO3[w.id1:w.id2]),na.rm=T)), 
	       y= (as.numeric(raw.data$Uzc)), lag.max = 600*1.0, plot=FALSE)
     lag.id = (aa$lag[which.max((aa$acf))])
     print(paste("HNO3 lag time:",lag.id," was found! ",sep=""))
     # copy tildas data to raw.data 
     raw.data$tildas.HNO3 <- as.numeric(raw.tildas$HNO3[(w.id1+lag.id):(w.id2+lag.id)])#*(1/63)
     # find HONO lag time
     aa <- ccf(x= (as.numeric(raw.tildas$HONO[w.id1:w.id2])-mean(as.numeric(raw.tildas$HONO[w.id1:w.id2]),na.rm=T)), 
	       y= (as.numeric(raw.data$Uzc)), lag.max = 600*1.0, plot=FALSE)
     lag.id = (aa$lag[which.max((aa$acf))])
     print(paste("HONO lag time:",lag.id," was found! ",sep=""))
     # copy tildas data to raw data
     raw.data$tildas.HONO <- as.numeric(raw.tildas$HONO[(w.id1+lag.id):(w.id2+lag.id)])#*(1/47)
     # find N2O lag time
     aa <- ccf(x= (as.numeric(raw.tildas$N2O[w.id1:w.id2])-mean(as.numeric(raw.tildas$N2O[w.id1:w.id2]),na.rm=T)), 
	       y= (as.numeric(raw.data$Uzc)), lag.max = 600*1.0, plot=FALSE)
     lag.id = (aa$lag[which.max((aa$acf))])
     print(paste("N2O lag time:",lag.id," was found! ",sep=""))
     # copy tildas data to raw data
     raw.data$tildas.N2O  <- as.numeric(raw.tildas$N2O[(w.id1+lag.id):(w.id2+lag.id)])#*(1/44)
     # --- despike of time series data with a 3.5-standard deviation 
     raw.data$tildas.H2O  <- fun_despike(x=raw.data$tildas.H2O,  nstd=c(3.5))
     raw.data$tildas.HNO3 <- fun_despike(x=raw.data$tildas.HNO3, nstd=c(3.5))
     raw.data$tildas.HONO <- fun_despike(x=raw.data$tildas.HONO, nstd=c(3.5))
     raw.data$tildas.N2O  <- fun_despike(x=raw.data$tildas.N2O,  nstd=c(3.5))
     raw.data$Uzc  <- fun_despike(x=as.numeric(raw.data$Uzc),  nstd=c(3.5))
     raw.data$Uxc  <- fun_despike(x=as.numeric(raw.data$Uxc),  nstd=c(3.5))
     raw.data$Uyc  <- fun_despike(x=as.numeric(raw.data$Uyc),  nstd=c(3.5))
     raw.data$H2O  <- fun_despike(x=as.numeric(raw.data$H2O),  nstd=c(3.5))
     raw.data$CO2  <- fun_despike(x=as.numeric(raw.data$CO2),  nstd=c(3.5))
     #remove na column in raw.data
     raw.data <- na.omit(raw.data)
     # --- coordinate rotation and covariance calculation via fortran subroutine --- 
     try(
   	obj  <- fun_ecflux_for(raw.data=raw.data)
	)
     # --- tubulent, stationary, and footprint  test  flux2 is the lowwer level at 30-60cm
     flux.test <- fun_turbulent_test(x=raw.data$Uzc, ustar=obj$flux.2$ustar,uavg=obj$flux.2$uavg,
				     SH=obj$air.den * 1005. * obj$flux.2$f1, 
				     LE=obj$air.den * 2.504 * 1E3 * obj$flux.2$f2,
				     Ta=obj$air.temp,roha=obj$air.den, zm=0.7,
				     wd=obj$flux.2$wd)
     ld.go <-FALSE
     if (ld.go) {
     # --- frequency analysis --- 
     cospec<- spectrum(x=c(as.numeric(raw.data$Uzc),
			   (as.numeric(raw.data$tildas.HNO3)-mean(as.numeric(raw.data$tildas.HNO3),na.rm=T))),
			   method="pgram",plot=FALSE)
     hno3.spec <- cospec$spec
     #plot(cospec$spec~cospec$freq, log="xy", main="W' HNO3' Co-Spectrum")
     cospec<- spectrum(x=c(as.numeric(raw.data$Uzc),
			   (as.numeric(raw.data$tildas.HONO)-mean(as.numeric(raw.data$tildas.HONO),na.rm=T))),
		           method="pgram",plot=FALSE)
     hono.spec <- cospec$spec
     #plot(cospec$spec~cospec$freq, log="xy", main="W' HONO' Co-Spectrum")
     cospec<- spectrum(x=c(as.numeric(raw.data$Uzc),
			   (as.numeric(raw.data$tildas.N2O)-mean(as.numeric(raw.data$tildas.N2O),na.rm=T))),
		           method="pgram",plot=FALSE)
     n2o.spec <- cospec$spec
     #plot(cospec$spec~cospec$freq, log="xy", main="W' N2O' Co-Spectrum")
     cospec<- spectrum(x=c(as.numeric(raw.data$Uzc),
			   (as.numeric(raw.data$tildas.H2O)-mean(as.numeric(raw.data$tildas.H2O),na.rm=T))),
		           method="pgram",plot=FALSE)
     h2o.spec <- cospec$spec
     #plot(cospec$spec~cospec$freq, log="xy", main="W' H2O' Co-Spectrum")
     new.sheet <- data.frame(freq=cospec$freq, h2o.spec= h2o.spec, n2o.spec=n2o.spec,hono.spec=hono.spec,hno3.spec=hno3.spec)
     spec.table <- rbind(spec.table, new.sheet)    
     }
     # --- summary the 15-min flux calculate and create new row to the flux.table --- 
     f.cor <- 0.5 
     if ( length(gd.sub$date)==1 ) { 
        # gd.le <- f.cor*obj$air.den*2.504*1E4*(18./24.5)*gd.sub$h2o/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2. 
        # gd.co2 <- f.cor*(24.5/44.)*gd.sub$co2/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        # gd.ch4 <- f.cor*(24.5/16.)*gd.sub$ch4/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        # gd.n2o <- f.cor*(24.5/44.)*gd.sub$n2o/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        # gd.no  <- f.cor*(24.5/30.)*gd.sub$no /(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        # gd.nox <- f.cor*(24.5/46.)*gd.sub$nox/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        # gd.nh3 <- f.cor*(24.5/17.)*gd.sub$nh3/(abs(obj$flux.1$uavg-obj$flux.2$uavg))*(obj$flux.2$ustar)**2.
        
        # revised to new version please see the reference paper (Nelson et al 2019, AFM 264 104-113 Ammonia flux measurements)    
        # gd.le  <- f.cor*(obj$air.den*2.504*1E4*(18./24.5)*gd.sub$h2o/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1) 
        # gd.co2 <- f.cor*((24.5/44.)*gd.sub$co2/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)
        # gd.ch4 <- f.cor*((24.5/16.)*gd.sub$ch4/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)
        # gd.n2o <- f.cor*((24.5/44.)*gd.sub$n2o/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)
        # gd.no  <- f.cor*((24.5/30.)*gd.sub$no /(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)
        # gd.nox <- f.cor*((24.5/46.)*gd.sub$nox/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)
        # gd.nh3 <- f.cor*((24.5/17.)*gd.sub$nh3/(obj$air.temp.1 - obj$air.temp))*(obj$flux.1$f1)

         gd.le  <- f.cor*(obj$air.den*2.504*1E4*(18./24.5)*gd.sub$h2o/(gd.sub$co2))*(obj$flux.1$f3) 
         gd.co2 <- f.cor*((24.5/44.)*gd.sub$co2/(gd.sub$co2))*(obj$flux.1$f3)
         gd.ch4 <- f.cor*((24.5/16.)*gd.sub$ch4/(gd.sub$co2))*(obj$flux.1$f3)
         gd.n2o <- f.cor*((24.5/44.)*gd.sub$n2o/(gd.sub$co2))*(obj$flux.1$f3)
         gd.no  <- f.cor*((24.5/30.)*gd.sub$no /(gd.sub$co2))*(obj$flux.1$f3)
         gd.nox <- f.cor*((24.5/46.)*gd.sub$nox/(gd.sub$co2))*(obj$flux.1$f3)
         gd.nh3 <- f.cor*((24.5/17.)*gd.sub$nh3/(gd.sub$co2))*(obj$flux.1$f3)
 
     }else{
         gd.le  <- NA
         gd.co2 <- NA
         gd.ch4 <- NA
         gd.n2o <- NA
         gd.no  <- NA
         gd.nox <- NA
         gd.nh3 <- NA
     }
     new.row <- data.frame(date.time=as.POSIXct(date.id[i],format='%Y-%m-%d %H:%M:%S'), 
			   lev1.uavg=obj$flux.1$uavg,
			   lev2.uavg=obj$flux.2$uavg,
                           lev1.ustar=obj$flux.1$ustar,
                           lev2.ustar=obj$flux.2$ustar,
			   lev1.sh=obj$air.den * 1005. * obj$flux.1$f1,
			   lev1.le.1=obj$air.den * 2.504 * 1E3 * obj$flux.1$f2,
			   lev1.le.2=obj$air.den * 2.504 * 1E3 * obj$flux.2$f4*(18./24.5)*1E-6, #to mg
			   lev1.co2.1=obj$flux.1$f3 ,# to mol 
			   lev1.hno3=obj$flux.2$f1*(24.5/63.),# to nmol
			   lev1.hono=obj$flux.2$f2*(24.5/47.),# to nmol
			   lev1.n2o=obj$flux.2$f3*(24.5/44.), # to nmol
                           lev1.le.gd=gd.le,
			   lev1.co2.gd=gd.co2,
                           lev1.nh3.gd=gd.nh3,  
	                   lev1.no.gd=gd.no,
                           lev1.nox.gd=gd.nox,
                           lev1.n2o.gd=gd.n2o, 
 			   lev1.wd=obj$flux.1$wd,
                           lev2.wd=obj$flux.2$wd, 
                           lev1.xmax=flux.test$xmax,
                           flag.stat=flux.test$flag.stat,
			   flag.itc=flux.test$flag.itc,
			   flag.fpt=flux.test$flag.fpt,
			   flag.n1n2=target,
                           lev1.ta=obj$air.temp.1,
                           lev2.ta=obj$air.temp  )
     # append the new row to the data.frame
       flux.table <- rbind(flux.table, new.row)   
     }#end for 
     #dev.off()
   }#end if
  
  # order the flux.table by date
  flux.table <- flux.table[order(flux.table$date.time),]

  # calculate mean of the co-spectra 
  #spec.mean <- aggregate( spec.table[,2:5], by=list(spec.table$freq), FUN=mean, na.action = na.omit) 
  #plot(x=as.numeric(spec.mean$Group.1)*10,y=spec.mean$nh2o/max(spec.mean$h2o), log="xy",ylim=c(1e-4,1e0),
  #     main="w'H2O' co-spectra",cex=0.5,ylab="normailzed co-spectra",xlab="frequency",col="blue")
  #plot(x=as.numeric(spec.mean$Group.1)*10,y=spec.mean$n2o/max(spec.mean$n2o), log="xy",ylim=c(1e-4,1e0),
  #     main="w'N2O' co-spectra",cex=0.5,ylab="normailzed co-spectra",xlab="frequency",col="brown")
  #plot(x=as.numeric(spec.mean$Group.1)*10,y=spec.mean$hono/max(spec.mean$hono), log="xy",ylim=c(1e-4,1e0),
  #     main="w'HONO' co-spectra",cex=0.5,ylab="normailzed co-spectra",xlab="frequency",col="black")
  #plot(x=as.numeric(spec.mean$Group.1)*10,y=spec.mean$hno3/max(spec.mean$hno3), log="xy",ylim=c(1e-4,1e0),
  #     main="w'HNO3' co-spectra",cex=0.5,ylab="normailzed co-spectra",xlab="frequency",col="red")
}# end for --fid-- loop  

#write out the flux.table

write.table(file=paste("./plot_pdf/flux.table.20190707-20190812.dat",sep=""),flux.table, row.names=FALSE, sep=",")
#
#

plot.ld <- TRUE
if(plot.ld)
{
# pdf(file=paste("./plot_pdf/flux.table.timeseries.pdf",sep=""))
 par(mfrow=c(4,2),mai=c(0.75,0.75,0.2,0.2) )
 plot(x=flux.table$date.time, y=flux.table$lev1.hno3,type="p",xlab="HNO3 flux (nmol m-2s-1)",
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1) , ylim=c(-1E-3,0.01))

 plot(x=flux.table$date.time, y=flux.table$lev1.hono,type="p",xlab="HONO flux (nmol m-2s-1)",
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1), ylim=c(-1E-3,0.05))
 
 plot(x=flux.table$date.time, y=flux.table$lev1.n2o,type="p",xlab="N2O flux (nmol m-2s-1)", 
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1), ylim=c(-5E0,40) )
 
 plot(x=flux.table$date.time, y=flux.table$lev1.le.2,type="p",xlab="Latent heat flux (W m-2; J m-2 s-1)",
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1),ylim=c(-5E1,600))

 plot(x=flux.table$date.time, y=flux.table$lev1.no.gd,type="p",xlab="NO flux (nmol m-2s-1)", 
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1),ylim=c(-1E-3,0.1))
 
 plot(x=flux.table$date.time, y=flux.table$lev1.nox.gd,type="p",xlab="(NOx flux (nmolm-2s-1)",
      col=ifelse(flux.table$flag.n1n2=="N1","gray","black"),
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1),ylim=c(-1E-3,0.3))
 
 plot(x=flux.table$date.time, y=flux.table$lev1.ustar,type="p",
      col="forestgreen",xlab="Friction velocity (m s-1)",
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1),ylim=c(-1E-3,0.4))
 
 plot(x=flux.table$date.time, y=flux.table$lev1.le.1,type="p",xlab="Latent/Sensible heat flux (W m-2)",
      pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1), ylim=c(-5E1,600))
 
 lines(x=flux.table$date.time, y=flux.table$lev1.sh,col="red", 
	pch=ifelse( (flux.table$flag.itc==TRUE)&(flux.table$flag.stat==TRUE),19,1) )

#dev.off()
} 
