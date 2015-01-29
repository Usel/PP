#Make peak function, using sgrd and extract minima and maxima
# by creating new columns dominance,prominence,name and E we fill in the information later
# but they are only empty by now

makePeak <- function(dem.in, peak.list,make.peak.mode,epsg.code,kernel.size=3,int=TRUE){
  if (make.peak.mode==1){
    of='-of SAGA'
    fname='dem.sdat'
    # first generate the cli gdalwarp command string and run
    gdal.cmd <- paste('gdalwarp -overwrite -s_srs' ,epsg.code, of , dem.in, fname)
    system(gdal.cmd)
    # create saga filter cmd
    filter.cmd <- paste0('saga_cmd grid_filter 0 -INPUT "dem.sgrd" -MODE 0 -RADIUS ', kernel.size,' -RESULT "dem_fil.sgrd"')
    system(filter.cmd)
    # (SAGA) extract maximum values
    system('saga_cmd shapes_grid 9 -GRID  "dem_fil.sgrd" -MINIMA "min" -MAXIMA "max"')
    # (SAGA) generate convert cmd to extract max values to ASCII xyz listing
    convert.cmd <- paste0('saga_cmd io_shapes 2 -SHAPES "max.shp" -ALL -FILENAME "raw_peaklist.txt"')
    system(convert.cmd)
    # read the created peaklist
    df=read.csv("raw_peaklist.txt", header = FALSE, sep = "\t",dec='.')
    #delete unused cols 1-4 and row 1
    df<-df[-c(1:4)]
    df<-df[-c(1), ]
    # name the cols
    colnames(df)=c("xcoord","ycoord","altitude") 
    # sort by altitude
    df[order(df$altitude),]
    # add cols
    df['dominance'] <-NA
    df['prominence'] <-NA
    df['name'] <-NA
    df['E'] <-NA
    
    write.table(df,peak.list,row.names=F)
  } else {
    stop("not implemented yet")
  }
  return(df)
}