#####################
#functions for data correction
######################
#correct angles
corAng<-function(angle){
  return<-ifelse(angle>180,360-angle,ifelse(angle< -180,-360-angle,angle))
}