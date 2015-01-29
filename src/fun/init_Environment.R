# Set an environment function for settings, libraries and R Packages and RSAGA modules

initEnvironment <- function(fname.control,fname.DEM){
  # load libraries/ activate if not already
  libs<-c("sp","raster","maptools","RSAGA","rgdal", "gdalUtils", "RSAGA")
  lapply(libs, require, character.only=T)
  
  # set environment
  myenv=rsaga.env(check.libpath=FALSE, check.SAGA=FALSE,
                  workspace="D:/UNI/WS_14-15/Advanced_GIS/ibk_10m",
                  os.default.path="C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga",
                  modules="C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga/modules")
  rsaga.env()
  
  
  # define working directroy and parameters
  setwd("D:/WS_14-15/Advanced_GIS/ibk_10m")    
  
  projection(dem)<-target.proj4
  # we reproject it to get the geographical coordinates
  dem.latlon<-projectRaster(dem, crs=latlon.proj4, method="ngb")
  
  # set the requiered projection
  epsg.code<-ini$Projection$targetepsg
  
  # target projection (actually the projection of the DEM)
  target.proj4<-ini$Projection$targetproj4
  # we will also need the  basic latlon wgs84 proj4 string
  latlon.proj4<-ini$Projection$latlonproj4         
  
  
  # provide myenv and parameterlist for common use
  result=list(ini,myenv,Harry)
  names(result)=c('ini','myenv','Harry')
  return (result)  
}

