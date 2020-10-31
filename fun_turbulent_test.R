
# this function return flags and maximum footprint location to justify the flux quality 
# Ref: Foken and Wichura (1996) AFM, 78(1-2), 83-105.
# Ref: Hsieh et al. (2000)      AWR, 23, 765-772.  
fun_turbulent_test <- function(x,ustar,uavg,SH,LE,zm,Ta,roha,wd) {
  library(pracma)  
  flag.itc=TRUE
  flag.stat=TRUE
  x <- as.numeric(x)
  #ustar=0.3 #m/s
  #SH=250    #W/m2
  #LE=500    #W/m2  (J/s/m2)
  #z=1.0     # 1.0 m
  #Ta=25.0   #oC in Celsius 
  #roha=1.20 #kg/m3 
  k=0.41    #constant
  g=9.81    #m/s2
  cp=1005.  #J/kg/K 
  lv=2.501*1E6  #KJ/kg
  sub.n=6

  # Variables for footprint analysis 
  hc=0.1    # canopy height(m)
  #zm=0.7    # measurnemnt height(m) 0.6 to 0.8  
  z0=0.1*hc # initial roughness length as 0.1hc 
  d0=0.6*hc # zero plane of displancenment height as 0.6*hc
  xmax<- NA  # maximum footprint distance 
  #
  BigD <- c(0.28,0.97,2.44) # parameter D for Unstable, Neutral, Stable  
  BigP <- c(0.59,1.00,1.33) # parameter P for Unstable, Neutral, Stable

  # calculate the Obukhov Length 
  L = (-1*ustar**3.) /  (k*(g/(Ta+273.15))*( (SH/(cp*roha) + 0.61*(Ta+273.15)*LE/(lv*roha) )))
  print(paste("Obukhov_Length:",L,"(m)",sep=""))
 
  # do --- the stationary test  50%
  totl <- length(x) 
  sub.totl <- as.integer( floor(totl/6)) 
  sigma <- std(x)
  # print(paste("sigma of w:",sigma,"(m/s)",sep=""))
  sigma.sub <- array(NA, dim=c(sub.n)) 
  #
  flag.stat <- TRUE
  for (i in 1:sub.n ) 
  {
	  id1 <- 1 + (i-1)*sub.totl
	  id2 <- sub.totl*i
          sigma.sub[i] <- std(x[id1:id2])
	  if  ( (sigma.sub[i]  < 0.8*sigma) | (sigma.sub[i] > 1.2*sigma)) {
	     flag.stat <- FALSE
	     print(paste("instationary point found sigma.sub:",sigma.sub[i],sep=""))
	     print(paste("original sigma w: ", sigma, sep=""))
	     break # break the loop
	  }	  
   }

   # --- do the integral turbulance test use 30%
   r1 <-  zm/L
   print(paste("r1:",r1,sep=""))
   r2 <- -1*zm/L

   rr <- sigma/ustar
   #print(paste("rr:",rr,"sigma_w:",sigma,"ustar:",ustar,sep=" "))
   #print(paste("rr:",rr, "formula:", 2.0*(r2)**(1./6.), sep=""))


   flag.itc <- TRUE
   # unstable condition
   if (r1 < -1) {
       if ( (rr > 1.3*(2.0*(r2)**(1./6.))) | (rr < 0.7*(2.0*(r2)**(1./6.))) )
       {
       flag.itc <- FALSE
       print(paste("rr:",rr, "formula:", 2.0*(r2)**(1./6.), sep=""))
       print(paste("non-turblent statistic was found, under unstbale condiction",sep=""))
       } 	     
   }	 
   # near neutral 
   if  ( (r1 >= -1) & (r1 <= -0.00625) ) {
       if ( (rr >  1.3*(2.0*(r2)**(1./8.))) | (rr < 0.7*(2.0*(r2)**(1./8.)) ) )
       {
       flag.itc <- FALSE
       print(paste("rr:",rr, "formula:", 2.0*(r2)**(1./8.), sep=""))
       print(paste("non-turblent statistic was found, under neutral condiction",sep=""))
       }	     
   }	 
 
   # stable condition
   if  ( r1 > -0.00625) {
       if ( (rr >  1.3*1.4) | (rr < 0.7*1.4) )
       {
       flag.itc <- FALSE
       print(paste("rr:",rr,"formula: 1.4",sep="")) 
       print(paste("non-turblent statistic was found, under stable condiction",sep=""))
       }	     
    }	 
  
   # --- do flux footprint analysis 
   flag.fpt <- TRUE

   # calculate z0: roughness length from integration 
     z0 <- (zm-d0)/exp(k*uavg/ustar) 

   # calculate zu: a length scale for the footprint analysis
     zu <- zm*(log(zm/z0)-1.+(z0/zm))
     print(paste("zu:",zu,sep=""))
   # unsatable condition
   if (r1 < -1) {
     xmax <- (BigD[1]*(zu**BigP[1])*(abs(L)**(1.-BigP[1])))/(k**2)
   } 
   # near neutral condition
   if ((r1 >= -1) & (r1 <= -0.00625) ){
     xmax <- (BigD[2]*(zu**BigP[2])*(abs(L)**(1.-BigP[2])))/(k**2)
   }	   
   # stable condition
   if (r1 > -0.00625 ){
     xmax <- (BigD[3]*(zu**BigP[3])*(abs(L)**(1.-BigP[3])))/(k**2)
   }	   

   if ( (xmax > 50) | ((wd >45)&(wd<135)) | (wd>225)&(wd<315)){
	   flag.fpt <- FALSE 
           print(paste("zm/L",r1,
	               "z0:",format(z0,  digits=2,width=6, format = "f", flag = "0"),
	               "Footprint, xmax(m):",xmax,sep=" "))  
   } 
   print(paste("stationary:",flag.stat, " ITC:",flag.itc, "footprint:",flag.fpt,sep=""))
   
   # return a list of flags
   output <- list(flag.itc=flag.itc,flag.stat=flag.stat, flag.fpt=flag.fpt, xmax=xmax)
   return(output) 
}	
