# run all functions

# remove all superfluous/temporary files 
file.remove( (pattern =('run_pro_')),(pattern =('flood_')),(pattern =('tmp.sdat.aux.xml')),(pattern =('.aux.xml')),(pattern=('mp_')),
             full.names = TRUE, ignore.case = TRUE)         

#file names
peaklist<- "peaklist.txt"
ext.peaklist<-read.tabel("HarryBergliste.shp")
dem.peaklist<-read.table("final.peaklist.shp")
dem<- raster("test.asc")

#kernel.size<-3 

#########--------main------######
# calculate peaklist
if (run.makePeak) {
  final.peak.list<-makePeak(dem.in, peak.list,make.peak.mode,epsg.code, kernel.size)
} else {
  if (file.exists(peak.list)){
    final.peak.list<-read.table(peak.list, header = TRUE, sep = " ",dec='.')  
  } else{
    stop('There is no valid peaklist')
  }
}

# calculate dominance, prominence, E-value
for (i in 1: nrow(final.peak.list)){
  final.peak.list[i,4]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3])
  final.peak.list[i,5]<-calculateProminence(final.peak.list,final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],initial.flooding.step)
  final.peak.list[i,7]<-calculateIndependence(final.peak.list[i,])
}

# make peaklist spatial
coordinates(final.peak.list) <- ~xcoord+ycoord #set xy-coords
proj4string(final.peak.list) <- target.proj4   # set projection
writePointsShape(final.peak.list,"finalpeaklist.shp") # set it as shp
plot(final.peak.list) #visualisize

#extract harry
Harry(dem.example,target.projection)

#integrate harry into peaklist
new.peaklist<-distMergePeaks(dem.peaklist,ext.peaklist)
