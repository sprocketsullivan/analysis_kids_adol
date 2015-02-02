####################
#read in data
####################
rm(list=ls())
setwd("D:\\UlfT\\Experimente\\Human_experiments\\social_info\\children_8_10\\")
raw.dir<-("D:\\UlfT\\Experimente\\Human_experiments\\social_info\\children_8_10\\raw_data\\")
source("functions_social_comparison.R")
source("printbold.R")
#first read in data
#then calculate accuracy
#then modelling 2
participants<-read.table("participants.csv",sep="\t",header=T)
# make data set my data
my.data<-NULL
for (i in 1:nrow(participants))
{
  if (i==1)
  {
    my.data<-read.csv(paste(raw.dir,participants$file[i],".csv",sep=""),sep=";",dec=".",header=F)
    my.data$id<-participants$id[i]
    my.data$trial<-seq(1,120)
    my.data$player<-i
  }
  else
  {
    part.data<-read.csv(paste(raw.dir,participants$file[i],".csv",sep=""),sep=";",dec=".",header=F)
    part.data$id<-participants$id[i]
    part.data$trial<-seq(1,120)
    part.data$player<-i
    my.data<-rbind(my.data,part.data)  
  }
}
names(participants)[1]<-"id"
names(my.data)<-c("startTime","endTime","stimulusOnset","decisionDeg","decisionTime","infoType1","infoDeg1","infoType2","infoDeg2","startPos","dummy","id","trial")
#some corrections for the decision degs
my.data$decisionDeg<-ifelse(my.data$decisionDeg<0,360+my.data$decisionDeg,my.data$decisionDeg)
my.data$decisionDeg<-ifelse(my.data$decisionDeg>360,my.data$decisionDeg-360,my.data$decisionDeg)
#calculate onsets for each participant
my.data$onsets<-0
for (i in 1:nrow(participants)){
    sub.data<-subset(my.data,id==participants$id[i])
    firstITI<-(sub.data$stimulusOnset[1]-sub.data$startTime[1])*24*60*60
    onsets<-cumsum(c(0,diff(sub.data$stimulusOnset))*24*60*60)+firstITI
    my.data$onsets[my.data$id==participants$id[i]]<-onsets    
}
#calculate time until decision
my.data$duration<-0
for (i in 1:nrow(participants)){
    sub.data<-subset(my.data,id==participants$id[i])
    duration<-(sub.data$decisionTime-sub.data$stimulusOnset)*24*60*60
    my.data$duration[my.data$id==participants$id[i]]<-duration
}  
sum(my.data$duration>0)
#change S1 S2 so that adults are always player 1
select_adult<-participants$id[participants$adult==2]
#S1_S2
carry.over.1<-my.data$infoDeg1[my.data$id%in%select_adult&my.data$infoType1==1&my.data$infoType2==2]
carry.over.2<-my.data$infoDeg2[my.data$id%in%select_adult&my.data$infoType1==1&my.data$infoType2==2]
my.data$infoDeg1[my.data$id%in%select_adult&my.data$infoType1==1&my.data$infoType2==2]<-carry.over.2
my.data$infoDeg2[my.data$id%in%select_adult&my.data$infoType1==1&my.data$infoType2==2]<-carry.over.1
#I S1
select.1<-which(my.data$infoType1==0&my.data$infoType2==1&my.data$id%in%select_adult)
select.2<-which(my.data$infoType1==0&my.data$infoType2==2&my.data$id%in%select_adult)
my.data$infoType2[select.1]<-2
my.data$infoType2[select.2]<-1

#treatment variable (from info types and Adult Variable)
treat<-0
my.data$treat<-ifelse(my.data$infoType1==0,ifelse(my.data$infoType2==1,1,2),0)
my.data$treatVerb<-ifelse(my.data$treat==0,"Adult Kid",ifelse(my.data$treat==1,"Ind Adult","Ind Kid"))
#difference in degrees between two infos
my.data$initialDiff<-0
my.data$initialDiff<-abs(my.data$infoDeg1-my.data$infoDeg2)
my.data$initialDiff<-ifelse(my.data$initialDiff>180,360-my.data$initialDiff,my.data$initialDiff)
#corrections
my.data$decisionDeg[which(my.data$decisionDeg>360)]<-my.data$decisionDeg[which(my.data$decisionDeg>360)]-360
my.data$decisionDeg[which(my.data$decisionDeg<0)]<- 360+my.data$decisionDeg[which(my.data$decisionDeg<0)]

##export onsets etc for each player
#sub.data<-subset(my.data,id==participants$ID[1])
#sub.data$dummy<-0
#write.table(sub.data,paste("DG_",participants$ID[1],".csv",sep=""),sep=";",dec=".",col.names=F,row.names=F)

#read in behavioural data for accuracy calculation. 

participants[10]
for (i in 1:nrow(participants))
{
  if (i==1)
  {
    p2.data<-read.table(paste(raw.dir,participants[1,5],".csv",sep=""),sep=";",dec=".",skip=5,header=F)
    p2.data$id<-participants$id[i]
  }
  else
  {
    part.data<-read.table(paste(raw.dir,participants[i,5],".csv",sep=""),sep=";",dec=".",header=F,skip=5)
    part.data$id<-participants$id[i]
    p2.data<-rbind(p2.data,part.data)  
  }
}


load("child8_10.Rdata")


#rm(list=ls())
