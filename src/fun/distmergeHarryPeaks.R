# Furthermore we already implement the created harry.bergliste into our spatial DEM peaklist(final.peaklist)

HarrymergePeak<- function(dem.peaklist,ext.peaklist){
  
  # calculate the distance bewtween harry and ext.peak 
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  
  # filter data frame for minimum distance, "1" liest jede Zeile ein
  newdp<-dem.peaklist[apply(dist,1,which.min),]
  
  # apply the corresponding names
  newdp$name<-ext.peaklist$df.Name
  
  # return the dataframe
  return(newdp)
}  