# #############################
# Converts lat/long data from:
# dddmm.mmmmm to dd.dddddddd
# Input and output are vectors
# #############################

ConvertToDD<-function(vector){
  
  deg<-floor(vector/100)
  min<-vector-deg*100
  dd<-deg+min/60
  
  return(dd)
}