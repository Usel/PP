# After we create a peaklist out of our DEM we want to submit true summits by "Harry"
# Therefore we download a whole real peaklist from austria including names, coordinates, altitude
# since this is only a non-spatial list, we call a spade a spade by giving it a projection
# the output will be a point shapefile, we will implement in our peaklist
Harry <- function(dem.latlon,latlon.proj4,target.proj4){
  
  download.file("http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz",dest="bergliste.zip", mode = "wb") 
  
  # use r unzip to convert it to a kml file
  unzip ("bergliste.zip")
  
  # convert to csv file with babel
  system("gpsbabel -i kml -f bergliste-komplett.kml -o unicsv -F bergliste-komplett.csv")
  
  # read into data.frame
  df=read.csv("bergliste-komplett.csv",  header = TRUE, sep = ",", dec='.')
  
  # clean the list
  # extract altitude out of Description column that is full of  html garbage
  df$Altitude<-as.numeric(substring(df$Description, regexpr('H&ouml;he:</td><td>', df$Description)+19,regexpr("</td></tr>", df$Description)-1))
  df$Description <- NULL
  df$No <- NULL
  read.table ("bergliste-komplett.csv", header = TRUE, sep = ",", dec='.')
  
  #Now it's getting spatial
  df.sub = subset(df, Longitude >=dem.latlon@extent@xmin & Longitude<=dem.latlon@extent@xmax & Latitude>=dem.latlon@extent@ymin & Latitude  <=dem.latlon@extent@ymax)
  # first we have to assign lat lon geographic coordinates
  harry.bergliste<-SpatialPointsDataFrame(data.frame(df.sub$Longitude,df.sub$Latitude),data.frame(df.sub$Name,df.sub$Altitude), proj4string = CRS(latlon.proj4))
  
  # then we project it to MGI
  spTransform(harry.bergliste,CRS(target.proj4))
  
  # save to shapefile
  writePointsShape(harry.bergliste,"HarryBergliste.shp")
  
  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(harry.bergliste) 
  
}
