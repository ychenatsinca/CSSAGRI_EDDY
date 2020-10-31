

fun_despike <- function(x, nstd=3.5, gap.fill="linear")
{      

      #find the index of spikes 
      x <- as.numeric(x)
      spike.id <- which ( abs(x-mean(x,na.rm=T)) >= (sd(x,na.rm=T)*nstd) )
      x.mean<-mean(x,na.rm=T)
      print(paste("mean:",x.mean))
      #skipe first and the final elenments
      spike.id <- subset(spike.id, (spike.id!=1 & spike.id!=length(x))) 

      #filled the spike value

      if (is.na(length(spike.id)))  
	  {
           print(paste("No spike found!"))
	   x <- x
	  }
          else
	  {	  
           for (i in spike.id ) 
           {
              x[i] <- (x[i-1] + x.mean)/2.
      	   }
          print(paste("percentage of spikes:", format(100.0*(length(spike.id)/length(x)), digits=2,width=5),"%",sep=""))
      #
          }	  
      #return the vector
      return(x)
}



