# calculate prominence based on the derived DEM (see makepeak)
# by using a flood model, which lowers the level to the next lower altitude
# connecting to the same isohypse from one observed peak
# the output is the prominence value in our peaklist as derived in makepeak

calculateProminence <- function(peaks,x.coord, y.coord, altitude,exact.enough=5,int=TRUE){
  ## create shapefiles for one desired "current peak" and all other peaks
  #  write current peak tupel to xyz ASCII file
  write.table(list(x.coord, y.coord, altitude), file = "run.xyz",  row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # write run_peaks(all other peaks) from filtered df peaks point xyz ASCII file
  write.table(peaks[-c(4:8)],file = "run_peaks.xyz", row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # transform the current peak tupel to current_peak shape file for my desired peak
  rsaga.geoprocessor('io_shapes',3,env=myenv,
                     list(SHAPES='run_pro_current_peak.shp',
                          X_FIELD=1,
                          Y_FIELD=2,
                          FILENAME='run.xyz'))
  #transform the all other xyz to "run_pro_all_peaks"shape file
  rsaga.geoprocessor('io_shapes',3,env=myenv,
                     list(SHAPES='run_pro_all_peaks.shp',
                          X_FIELD=1,
                          Y_FIELD=2,
                          FILENAME='run_peaks.xyz'))
  
  ## create current_peak polygon shape file for intersecting 
  #  create empty raster with value=0
  rsaga.grid.calculus('mp_fil_dem.sgrd', 'run_pro_current_peak_marker',
                      ~(a*0), env=myenv)
  
  #  (gdalUtils)rasterize current_peak (value=255) position into raw file
  gdal_rasterize('run_pro_current_peak.shp', 'run_pro_current_peak_marker.sdat', burn=255)
  #  vectorize the 'current_peak' to a polygon shape
  rsaga.geoprocessor('shapes_grid', 'Vectorising Grid Classes', env=myenv,
                     list(POLYGONS='run_pro_current_peak_poly.shp',
                          GRID='run_pro_current_peak_marker.sgrd',
                          CLASS_ALL=0,
                          CLASS_ID=255.000000,
                          SPLIT=1))
  
  # Flood loop: flooding level is the middle of the minimum altitude of the DEM
  # and the current_peak altitude. if connected==TRUE we have to decide if the 
  # result is exact enough i.e. if the difference between min and max is smaler than
  # a defined value. if connected==FALSE we have to lower the level
  
  # (gdalUtils) derive infos from filtered dem
  file.info<-gdalinfo('mp_fil_dem.sdat', mm=T, approx_stats=T)
  # (R) obtain minimum flood altitude from 'fil_dem'
  min.flood.altitude<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  # (R) set current altitude to max flood altitude
  max.flood.altitude<-floor(altitude)
  # (R) while starting current_peak is not connected
  connected<-FALSE
  
  # (R) start flooding repeat until connecetd
  while (connected == FALSE) { 
    # set flooding level
    new.flood.altitude<-(max.flood.altitude+min.flood.altitude)/2
    
    # (R) create formula string for mask command
    formula<-paste0('ifelse(gt(a,', new.flood.altitude ,'),1,0)')
    # (RSAGA) mask level>peak set rest to nodata
    rsaga.grid.calculus('mp_fil_dem.sgrd', 'flood_run_level.sgrd',formula,env=myenv)
    
    # (RSAGA) make polygon shape from current floodlevel note altitude was set
    rsaga.geoprocessor('shapes_grid', 'Vectorising Grid Classes', env=myenv,
                       list(POLYGONS='flood_run_level.shp',
                            GRID='flood_run_level.sgrd',
                            CLASS_ALL=1,
                            CLASS_ID=1.000000,
                            SPLIT=1))
    
    # (RSAGA) write "marker value 255" to the flood_run_level shapefile to mark
    # the single polygon that contains the position of current_peak
    rsaga.geoprocessor("shapes_grid","Grid Statistics for Polygons", env=myenv,
                       list(GRIDS="run_pro_current_peak_marker.sgrd" ,
                            POLYGONS="flood_run_level.shp",
                            MAX=T,
                            QUANTILE=0,
                            RESULT="flood_run_result.shp"))
    
    # (gdalUtils) select the current_peak polygon
    ogr2ogr('flood_run_result.shp', 'flood_run_select.shp',
            f="ESRI Shapefile",
            select='run_pro_cur', where="run_pro_cur = 255",
            overwrite=TRUE)
    # (RSAGA) count how much peaks are inside the selceted polygon item
    rsaga.geoprocessor('shapes_points',1, env=myenv,
                       list(POINTS="run_pro_all_peaks.shp",
                            POLYGONS="flood_run_result.shp"))
    # (RSAGA) convert this to a ASCII csv file
    rsaga.geoprocessor('io_shapes', 2 ,env=myenv,
                       list(SHAPES='flood_run_result.shp',
                            ALL=TRUE,
                            HEADER=TRUE,
                            SEPARATE=0,
                            FILENAME='flood_run_result.txt'))
    
    # (R) read it into data frame
    result=read.csv(file = 'flood_run_result.txt', header=T, sep="\t",dec='.')
    
    # (R) check if the table has correct dimensions
    if (ncol(result)!=7) {stop('no results during selection of the peak polygon -> have to stop')}
    
    # (R) name the cols
    colnames(result)=c("c1","c2","c3","c4","c5","c6","c7")
    
    # (R) filter if c6=255 and c7 > 1 (= peak_polygon contains more than one peak => landbridge is closed)
    if (nrow(subset(result,result$c6 == 255 & result$c7 > 1)) > 0){
      # landbrige is closed but maybe in avery coarse way so check if the difference is small enough default=5
      if((max.flood.altitude-min.flood.altitude) < exact.enough){
        # closed landbrige is found
        connected<- TRUE
      }else{
        # if are connected but we flooded to deep (i.e. > exact enough) we rise flooding level half the way up
        min.flood.altitude<- new.flood.altitude}
    }else{
      # if we are not conneced we will lower the flooding level half the way down
      max.flood.altitude<- new.flood.altitude
    }
  }
  
  ## get the prominence value
  # create raster with value=0
  rsaga.grid.calculus(c('mp_fil_dem.sgrd;flood_run_level.sgrd'), 'run_level_raw.sgrd', ~(a*b), env=myenv)
  # (RSAGA) forces nodata reclass to derive true minimum
  rsaga.grid.calculus('run_level_raw.sgrd', 'run_level.sgrd','ifelse(eq(a,0),-99999,a)',env=myenv)
  
  # (R) get the prominence (min)value from file.info
  file.info<-gdalinfo('run_level.sdat', mm=T, approx_stats=T)
  notch<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  
  # (R) calculate the prominence
  prominence<- ceiling(altitude)-notch
  return (prominence)
}