#read in stimulus file
p2.stim.kids<-read.table("stimulus_kids_same.csv",header=T,sep=";",dec=".")
p2.skel<-read.table("skeleton_modified_3.csv",header=F,sep=";",dec=".")
p2.stim.adol<-read.table("stimulus_acc_12_same.csv",header=T,sep=";",dec=".")
#####
#accuracy for social players
#####
acc.kids<-sum((abs(corAng(p2.stim.kids$c_p-p2.stim.kids$s1)))^2)/59
acc.adol<-sum((abs(corAng(p2.stim.adol$c_p-p2.stim.adol$s1)))^2)/59
mean(abs(corAng(p2.stim.kids$c_p-p2.stim.kids$s1)))
mean(abs(corAng(p2.stim.adol$c_p-p2.stim.adol$s1)))
######
#correct choice in each trial in fmri P4
######
correct_pos<-p2.stim.kids$c_p[p2.skel$V5][1:120]
my.data$correct_pos<-rep(correct_pos,nrow(participants))
participants$acc.ind<-0
for (i in 1:nrow(participants)){
  sub.data<-subset(p2.data,id==unique(p2.data$id)[i])
  helper<<-(corAng(sub.data$V10-sub.data$V13))
  #if (length(which(abs(helper)>60))>0)
  #helper<-helper[-which(abs(helper)>60)]           
  #participants$acc.ind[i]<-sum(helper^2)/length(helper)
  participants$acc.ind[i]<-mean(abs(helper))
}
###################
#second choice accuracy
##################
participants$acc.ind2<-0
for (i in 1:nrow(participants)){
  sub.data<-subset(my.data,id==unique(my.data$id)[i]&decisionDeg>0&decisionDeg<360)
  helper<<-corAng(sub.data$correct_pos-sub.data$decisionDeg)
  #helper<-ifelse(abs(helper)>360,20,helper)
  #participants$acc.ind2[i]<-sum(helper^2)/119
  participants$acc.ind2[i]<-mean(abs(helper))
}
names(my.data)
###################
#simulated accuracy
##################
participants$acc.sim<-0
for (i in 1:nrow(participants)){
  sub.data<-subset(my.data,id==unique(my.data$id)[i])
  participants$acc.sim[i]<-sum(corAng(sub.data$correct_pos-sub.data$sim.choice)^2)/119
}
###################
#simulated accuracy2 without IND IND
##################
participants$acc.sim2<-0
for (i in 1:nrow(participants)){
  sub.data<-subset(my.data,id==unique(my.data$id)[i]&treatVerb!="IND IND")
  participants$acc.sim2[i]<-sum(corAng(sub.data$correct_pos-sub.data$sim.choice)^2)/239
}

aggregate(abs(corAng(p2.data$V10-p2.data$V13)),list(p2.data$id),mean)
################
#calculate weights
################
participants$w.INDIND<-0.5
participants$w.IS1<-ifelse(participants$age=="k",participants$acc.ind^2/(participants$acc.ind^2+acc.kids),participants$acc.ind^2/(participants$acc.ind^2+acc.adol))
participants$w.IS2<-participants$w.IS2
participants$w.SS<-0.5

boxplot(participants$w.IS1~participants$age)



boxplot(participants$acc.ind~participants$age)


