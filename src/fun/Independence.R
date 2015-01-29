# calculate the independency value using the derived calculated values of prominence and dominence
# as well as the altitude from the peaklist. The used function is given by Rauch:
# if d<100000: E = -((log2  (h / 8848) + log2 (d / 100000) + log2(p / h)) / 3)
# if d>100000: E = -((log2 (h / 8848) + log2(p / h)) / 3)
# note: using 8848 (Mt.Everest) is the highest mountain peak discoverd, so its used as the reference
#       d= dominance  p=prominence  h=altitude
#       All values has been maesured in meter
# The Independence will return the E-value in the peaklist

calculateIndependence <- function(peaklist){
  altitude<-peaklist[3]
  dominance<-peaklist[4]
  prominence<-peaklist[5]
  
  term1 = log2(altitude/8848)   
  term2 = log2(dominance/100000) # 100 km buffer radius of curent peak
  term3 = log2(prominence/alt)
  
  if (dominance > 100000){
    return((term1+term2+term3)/3*(-1))
  } else {
    return((term1+term3)/3*(-1))
  }
  return (E)
} 