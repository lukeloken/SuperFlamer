

# ##############################################################
# Visualize spatial, timeseries, and distribution of each flame variable
# Input is a spatial points data frame
# Output are several .png images 
# Each figure includes a map, timeseries, and histogram
# ##############################################################


PlotSuperFlame<-function(geodata, dir, Date, Site){
  
  B<-100 #Number of color breaks
  colors<-bpy.colors(n = B, cutoff.tails=0.1, alpha=1)
  
  i=2
  for (i in 2:length(geodata@data)){
    if (is.numeric(geodata@data[,i])==TRUE){
      if (diff(range(geodata@data[,i], na.rm=T))>0){
        name<-names(geodata@data)[i]
        a <- geodata[is.finite(geodata@data[,i]),]
        a$Col <- as.numeric(cut(a@data[,i], breaks = B))
        a$Color<-colors[a$Col]
        
        png(paste(dir, "/Maps/", name, Date, Site, ".png", sep=""), res=200, width=5,height=12, units="in")
        
        par(mar=c(2,1.5,2,0), bg="white")
        par(mfrow=c(3,1)) 
        par( oma = c( 1.5,2,1,0 ) )
        par(tck=-.02)
        par(mgp=c(0,.6,0))
        par(las=0)
        
        plot(a, col=a$Color, pch=16)
        #plot(base, add=TRUE)
        
        axis(1)
        axis(2)
        mtext("Latitude", 2, 2)
        mtext("Longitude", 1, 2)
        box(which='plot')
        
        plot(a@data[,i]~a$date_time, col=a$Color, pch=16, yaxt="n", ylab="", xlab="")
        #axis(1)
        axis(2)
        mtext(paste(name), 2, 2)
        mtext("Time", 1, 2)   
        mtext(paste(name, Site, Date, sep=" "), 3, -1, outer=TRUE, cex=1.5)
        
        hist(a@data[,i], breaks=25, col='lightgrey', main='', xlab='', ylab='')
        mtext(name, 1, 2)
        mtext('frequency', 2, 2)
        abline(v=mean(a@data[,i]), col="red", lty=2)
        abline(v=median(a@data[,i]), col="blue", lty=2)
        
        dev.off()
      }
    }
  }
}

