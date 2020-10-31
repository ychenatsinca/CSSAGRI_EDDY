
# R funtiom flux.cal() 
# 
fun_ecflux_for <- function(raw.data=raw.data ) {
# load the loading funtion load.data()
# Load/make fortran subroutine rot()
# print(system("R CMD SHLIB rot.f90",intern=TRUE))
#
dyn.load("ecflux.so",type="Fortran")
print(is.loaded("ecflux")) 

# load the sharing fortran library for the data processing/ calcualtion 
# the double rotation, the detail can be found in fortran code ROT.f90 
#!-----SITA: Rotation about Z axis (force average V=0)
#!-----PHI:  Rotation about Y axis (force average W=0)
#!-----SPL:  WINDOW SIZE
#!-----U, V, W: Ture velocity (AFTER CORRECTION)
#!-----UAVG, VAVG, WAVG: WINDOW AVERAGE WIND SPEED
#--------------------------------------------------------------------
# Create share library ---> ecflux.so
# by following command 
#> system(R CMD SHLIB ecflux.f90)
# check funtion names in shared library
#> system("nm -D ecflux.so") show function "names" in ecflux.so
#--------------------------------------------------------------------

#        Rhoa(j)=(Pa*1E5)/(287.05*(273.15+T(j)))           ! unit in [g/m3]      dry air     density     Assume T <- sonic=T <- dry <- air
#           T(j)=T(j)*(1-0.514*(Rhov(j)/Rhoa(j)))          !-----Change Sonic Temp to Air Temp

#convert sonic temperature to air temperature 
raw.data$Rhoa <- (as.numeric(raw.data$cell_press)*1E6)/(287.05*(273.15+as.numeric(raw.data$Tsc)))
raw.data$Tac <- as.numeric(raw.data$Tsc) * (1- 0.514*(as.numeric(raw.data$H2O)/raw.data$Rhoa))
raw.data$Ta  <- as.numeric(raw.data$Ts) * (1- 0.514*(as.numeric(raw.data$H2O)/raw.data$Rhoa))

air.temp <- mean(raw.data$Ta,na.rm=TRUE)
air.den <- mean(raw.data$Rhoa, na.rm=TRUE)
air.press <- mean(as.numeric(raw.data$cell_press), na.rm=TRUE)

air.temp.1 <-  mean(raw.data$Tac,na.rm=TRUE)

#level 1 YOUNG 81000
wind.rot.1 <- .Fortran("ecflux",
		     spl = as.integer(length(raw.data$TIMESTAMP[!is.na(raw.data$Uxc)])),
	             u = -1*as.double(raw.data$Ux[!is.na(raw.data$Uxc)]),
	             v = -1*as.double(raw.data$Uy[!is.na(raw.data$Uxc)]),
	             w = as.double(raw.data$Uz[!is.na(raw.data$Uxc)]),
		     v1 = as.double(raw.data$Ta[!is.na(raw.data$Uxc)]),
		     v2 = as.double(raw.data$H2O[!is.na(raw.data$Uxc)]),
		     v3 = as.double(raw.data$CO2[!is.na(raw.data$Uxc)]), 
	             v4 = as.double(raw.data$CO2[!is.na(raw.data$Uxc)]), 
		     sita = double(1), phi = double(1),
		     uavg = double(1), wd  = double(1), 
		     ustar= double(1), f1 = double(1), f2 = double(1), f3 = double(1), f4 = double(1) )
#level 2 IRGASON(EC150+CSAT3) 
wind.rot.2 <- .Fortran("ecflux",
		     spl = as.integer(length(raw.data$TIMESTAMP[!is.na(raw.data$Uxc)])),
	             u = as.double(raw.data$Uxc[!is.na(raw.data$Uxc)]),
	             v = as.double(raw.data$Uyc[!is.na(raw.data$Uxc)]),
	             w = as.double(raw.data$Uzc[!is.na(raw.data$Uxc)]),
		     v1 = as.double(raw.data$tildas.HNO3[!is.na(raw.data$Uxc)]), 
	             v2 = as.double(raw.data$tildas.HONO[!is.na(raw.data$Uxc)]),
		     v3 = as.double(raw.data$tildas.N2O[!is.na(raw.data$Uxc)]), 
                     v4 = as.double(raw.data$tildas.H2O[!is.na(raw.data$Uxc)]), 
	             sita = double(1), phi = double(1),
		     uavg = double(1), wd  = double(1), 
		     ustar= double(1), f1 = double(1), f2 = double(1), f3 = double(1), f4 = double(1) )

return(list(flux.1=wind.rot.1, flux.2=wind.rot.2, air.den=air.den/1E3, air.temp=air.temp, air.press=air.press, air.temp.1 = air.temp.1))
}


