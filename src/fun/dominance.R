# calculate the dominance between peaks and eventually write it in the dominance coloumn
# this value is defined by the nearest higher peak (or better altitude) to one observed peak
# The output will return the dominance value in the peaklist

calculateDominance <- function(x.coord, y.coord, altitude, int=TRUE){
  # write peak-tupel to csv format
  write.table(list(x.coord, y.coord, altitude), 'tmp_.xyz', row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # (SAGA) create a point shapefile from the extracted line 
  system("saga_cmd io_shapes 3 -SHAPES=tmp_peak.shp -X_FIELD=1 -Y_FIELD=2 -FILENAME=tmp_.xyz")
  
  # (SAGA) create a nodata raster 
  system("saga_cmd grid_calculus 1 -GRIDS dem_fil.sgrd -RESULT tmp_peak.sgrd -FORMULA (a/a*(-99999))")
  # (GDAL) rasterize point Shape (current peak position) into nodata raster file, read the sdat instead of sgrd
  system('gdal_rasterize -burn 1  tmp_peak.shp tmp_peak.sdat')
  # (GDAL) calculates the proximity
  system('saga_cmd grid_tools "Proximity Grid" -FEATURES tmp_peak.sgrd -DISTANCE tmp_dist.sgrd')
  ## mask all higher altitudes
  # (SAGA) mask level=peak set, all other nodata
  system(paste0("saga_cmd grid_calculus 1 -GRIDS dem_fil.sgrd -RESULT tmp_level.sgrd -FORMULA ifelse(gt(a,", ceiling(altitude) ,"),1,-99999)"))
  
  ## dominance calculations
  # (SAGA) calculate masked proximity raster 
  system('saga_cmd grid_calculus 1 -GRIDS "tmp_level.sgrd;tmp_dist.sgrd" -RESULT tmp.sgrd -FORMULA "a*b"') 
  # (R) get the dominance (min)value frome file.info
  file.info<-system('gdalinfo -mm -approx_stats tmp.sdat', intern = TRUE)
  dominance<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  dominance
  return (dominance) 
}