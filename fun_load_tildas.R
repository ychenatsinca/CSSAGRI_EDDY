

fun_load_tildas <- function( filename="20190707_N1.str") {
#filename="/work/ychen/CSSAGRI/EC_Raw/TILDAS/20190713_N2.str"
print(paste("start loading tildas file,", filename,sep=""))
#load the white space seperated ascii data
tildas.raw <- read.table(file = filename, 
			 fill = TRUE, header = TRUE,
		 stringsAsFactors = FALSE)

#get initial time
init.time <- paste(tildas.raw$Date[1]," ",tildas.raw$HNO3[1],sep="")
#offset one line 
line.offset = 1
tildas.raw <- tildas.raw[-(1:line.offset),]


#remove na columns
tildas.raw <- na.omit(tildas.raw)

tildas.raw$Date <- as.double(tildas.raw$Date)-as.double(tildas.raw$Date[1])

print( paste("inital time:", init.time,sep=" "))
print( paste("total line:", length(tildas.raw$Date), sep=" "))

#create timestamp

tildas.raw$timestamp <- format(as.POSIXct(as.double(tildas.raw$Date),  origin=paste("1990-01-01", substr(init.time,start=12,stop=20),sep=" "), tz="GMT")
					     , format="%H:%M:%OS1")

#   tildas.raw$timestamp<-format(as.POSIXct(format(as.numeric(substr(tildas.raw$Date,start=5,stop=13),digits=1)), origin=ini.time, tz="GMT", format="%H:%M:OS1")) 
#   format(as.POSIXct(as.numeric(substr(tildas.raw$Date[i],start=5,stop=13)), origin=init.time, tz="GMT"), format="%H:%M:%OS1")
#   tildas.raw$timestamp[i] <- format(as.POSIXct(as.numeric(substr(tildas.raw$Date[i],start=5,stop=13)), origin=init.time, tz="GMT"), format="%H:%M:%OS1")
#   print(paste("i",i,tildas.raw$timestamp[i],sep=" "))

return(tildas.raw=tildas.raw)

} #end function

