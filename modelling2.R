##########
#models
##########

all.models<-list()
my.data$movement<-corAng(abs(my.data$startPos-my.data$decisionDeg))
my.data$correction<-abs(corAng(my.data$infoDeg1-my.data$decisionDeg))
d.1<-abs(corAng(my.data$infoDeg1-my.data$decisionDeg))
d.2<-abs(corAng(my.data$infoDeg2-my.data$decisionDeg))
d.3<-abs(corAng(my.data$infoDeg2-my.data$infoDeg1))
#my.data$correction<-my.data$correction*ifelse(d.1+d.2>d.3,ifelse(d.1<d.2,-1,1),1)
my.data$distance<-abs(corAng((my.data$infoDeg1-my.data$infoDeg2)))
hist(my.data$distance)


##################
#calculate regression line in SS condition
####################
soc.regression<-list()
for(i in 1:nrow(participants))
{   
  soc.regression[[i]]<-lm(correction~distance-1,data=subset(my.data,id==participants$id[i]&distance<180&correction<180&correction>0&distance>0&treat==0))
}
soc.coef<-rep(0,nrow(participants))
for(i in 1:nrow(participants))
  soc.coef[i]<-soc.regression[[i]]$coefficients
wilcox.test(soc.coef,mu=0.5)


boxplot(soc.coef)
######################
#select a player
######################
for(i in 1:nrow(participants)){
  current.id<-unique(my.data$id)[i]
  s.c<-soc.coef[i]
  models<-list()
  #model 1
  #bayes optimal decision
  help.data<-subset(my.data,id==current.id&distance<180&correction<180&distance>0&correction>0)
  help.data$phase2<-ifelse(help.data$treat==1,1,0)
  help.data$phase3<-ifelse(help.data$treat==2,1,0)
  help.data$phase4<-ifelse(help.data$treat==0,1,0)
  acc.ind<<-participants$acc.ind[participants$id==current.id]
  coef.p2<-(acc.ind/(acc.ind+acc.s1))
  coef.p3<-(acc.ind/(acc.s2+acc.ind))
  coef.p4<-(acc.s1/(acc.s2+acc.s1))
  normNLL <<- function(par) {
    y.pred= help.data$phase2*help.data$distance*coef.p2+help.data$phase3*help.data$distance*coef.p3+help.data$phase4*help.data$distance*coef.p4
    -sum(dnorm(help.data$correction,mean=y.pred,sd=sqrt(par),log=T)) }
  models[[1]]<-optimize(normNLL,interval=c(0,100000))
  #model 2
  # correction for social info
  coef.p2<<-acc.ind/(acc.ind+acc.s1)
  coef.p3<<-acc.ind/(acc.ind+(acc.s1/s.c-acc.s1))
  coef.p4<<-acc.s2/(acc.s1+acc.s2)
  z<-try(models[[2]]<-nls(correction~phase2*acc.ind/(acc.ind+k.s*acc.s1)*distance+phase3*acc.ind/(acc.ind+acc.s2)*distance+phase4*k.s*acc.s1/(k.s*acc.s1+acc.s2)*distance,data=help.data,start=list(k.s=1),algorithm="port",lower=c(0.001),upper=c(100)))
  if(class(z) == "try-error")
  {
    #z<-try(models[[4]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*distance+phase4*acc.s1/(acc.s1+acc.s2)*distance,data=help.data,start=list(k=0.5),algorithm="port",lower=c(0),upper=c(100)))
    if(class(z) == "try-error") models[[2]]<-9999
  }
  #model 3
  # correction for social info
  coef.p2<<-acc.ind/(acc.ind+(acc.s2*s.c/(1-s.c)))
  coef.p3<<-acc.ind/(acc.ind+acc.s2)
  coef.p4<<-s.c
  z<-try(models[[3]]<-nls(correction~phase2*acc.ind/(acc.ind+acc.s1)*distance+phase3*acc.ind/(acc.ind+k.s*acc.s2)*distance+phase4*acc.s1/(acc.s1+k.s*acc.s2)*distance,data=help.data,start=list(k.s=1),algorithm="port",lower=c(0.001),upper=c(100)))
  if(class(z) == "try-error")
  {
    #z<-try(models[[4]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*distance+phase4*acc.s1/(acc.s1+acc.s2)*distance,data=help.data,start=list(k=0.5),algorithm="port",lower=c(0),upper=c(100)))
    if(class(z) == "try-error") models[[3]]<-9999
  }
  ###################################
  #model 4 Bayes optimal with individual discount
  ###################################
  help.data$acc.ind<-acc.ind
  help.data$acc.s1<-acc.s1
  help.data$acc.s2<-acc.s2
  help.data$sc<-s.c
  z<-try(models[[4]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*distance+phase4*acc.s1/(acc.s1+acc.s2)*distance,data=help.data,start=list(k=1),algorithm="port",lower=c(0.001),upper=c(100)))
  #z<-try(models[[4]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*correction+phase4*sc*correction,data=help.data,start=list(k=0.5),algorithm="port",lower=c(0),upper=c(100)))
  if(class(z) == "try-error")
  {
    z<-try(models[[4]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*distance+phase4*acc.s1/(acc.s1+acc.s2)*distance,data=help.data,start=list(k=0.5),algorithm="port",lower=c(0),upper=c(100)))
    if(class(z) == "try-error") models[[4]]<-9999
  }
  ###################################
  #model 5 Bayes optimal with individual discount and social 1
  ###################################
  help.data$acc.ind<-acc.ind
  help.data$acc.s1<-acc.s1
  help.data$acc.s2<-(acc.s1/s.c-acc.s1)
  help.data$sc<-s.c
  z<-try(models[[5]]<-nls(correction~phase2*exp(k)*acc.ind/(exp(k)*acc.ind+exp(k.s)*acc.s1)*distance+phase3*exp(k)*acc.ind/(exp(k)*acc.ind+acc.s2)*distance+phase4*exp(k.s)*acc.s1/(exp(k.s)*acc.s1+acc.s2)*distance,data=help.data,start=list(k=1,k.s=1)),algorithm="port")
  #if(class(z) == "try-error")
  #{
  #  z<-try(models[[5]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+k.s*acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+acc.s2)*distance+phase4*k.s*acc.s1/(k.s*acc.s1+acc.s2)*distance,data=help.data,start=list(k=1,k.s=1),algorithm="port",lower=c(0),upper=c(100)))
  #  if(class(z) == "try-error") models[[5]]<-9999
  #}
  ###################################
  #model 6 Bayes optimal with individual discount and social 2
  ###################################
  help.data$acc.ind<-acc.ind
  help.data$acc.s1<-(acc.s2*s.c/(1-s.c))
  help.data$acc.s2<-acc.s2
  help.data$sc<-s.c
  z<-try(models[[6]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+k.s*acc.s2)*distance+phase4*acc.s1/(acc.s1+k.s*acc.s2)*distance,data=help.data,start=list(k=0.5,k.s=1),algorithm="port",lower=c(0),upper=c(100)))
  if(class(z) == "try-error")
  {
    z<-try(models[[6]]<-nls(correction~phase2*k*acc.ind/(k*acc.ind+acc.s1)*distance+phase3*k*acc.ind/(k*acc.ind+k.s*acc.s2)*distance+phase4*acc.s1/(acc.s1+k.s*acc.s2)*distance,data=help.data,start=list(k=1,k.s=1),algorithm="port",lower=c(0),upper=c(100)))
    if(class(z) == "try-error") models[[5]]<-9999
  }
  
  ############################
  #model 7
  #same slopes
  ############################
  coef.p2<<-(acc.ind/(acc.ind+acc.s1))
  coef.p3<<-(acc.ind/(acc.s2+acc.ind))
  coef.p4<<-(acc.s1/(acc.s2+acc.s1))
  models[[7]]<-lm(correction~distance-1,data=help.data)
  #################################
  #model 8
  #three different slopes
  #################################
  models[[8]]<-lm(correction~I(phase2*distance*coef.p2)+I(phase3*distance*coef.p3)+I(phase4*distance*coef.p4)-1,data=help.data)
  all.models[[i]]<-models
}
################################
#calculate BICs
################################
i<-1
list.BIC<-list()

for(i in 1:nrow(participants))
{
  res.BIC<-rep(0,7)
  lenny<-length(all.models[[i]][[8]]$residuals)
  res.BIC[1]<-all.models[[i]][[1]]$objective*2+1*log(lenny)
  try(res.BIC[2]<-BIC(all.models[[i]][[2]]))
  try(res.BIC[3]<-BIC(all.models[[i]][[3]]))
  try(res.BIC[4]<-BIC(all.models[[i]][[4]]))
  try(res.BIC[5]<-BIC(all.models[[i]][[5]]))
  try(res.BIC[6]<-BIC(all.models[[i]][[6]]))
  try(res.BIC[7]<-BIC(all.models[[i]][[7]]))
  try(res.BIC[8]<-BIC(all.models[[i]][[8]]))
  #res.BIC[6]<-BIC(all.models[[i]][[6]])
  #res.BIC[7]<-BIC(all.models[[i]][[7]])
  res.BIC[res.BIC==0]<-NA
  list.BIC[[i]]<-res.BIC
}

results <- data.frame(matrix(unlist(list.BIC), nrow=nrow(participants), byrow=T))
results$id<-unique(my.data$id)
#names(results)<-c("BO","BO_soc1", "BO_soc2","BO_ind","one_slope","three_slopes","id")
names(results)<-c("BO","BO_soc1", "BO_soc2","BO_ind","BO_ind_soc_1","BO_ind_soc_2","AID","FM","id")
#results<-results[order(results$id),]
require(xtable)
printbold(xtable(results),each="row",max=F,type="latex")
print(xtable(results))
results
?nls
##install.packages("Matrix")
#load("results_SC.Rdata")

ks<-rep(0,nrow(participants))
for (i in 1:nrow(participants))
{
  z<-try(as.numeric(coef(all.models[[i]][[6]]))[1])
  if(class(z) == "try-error"|length(z)==0) ks[i]<-NA else ks[i]<-z
}

kss<-rep(0,nrow(participants))
for (i in 1:nrow(participants))
{
  z<-try(as.numeric(coef(all.models[[i]][[6]]))[2])
  if(class(z) == "try-error"|length(z)==0) kss[i]<-NA else kss[i]<-z
}
ks
boxplot(kss)
plot(log(ks)~log(participants$acc.ind))
abline(lm(log(ks)~log(participants$acc.ind)))
       summary(lm(log(ks)~log(participants$acc.ind)+factor(participants$gender)))
ks<-data.frame(value=ks,id=participants$id,value_s=kss)
my.data$ks<-""
for (i in 1:nrow(participants))
  my.data$ks[my.data$treat==0&my.data$id==participants$id[i]]<-ks$value[ks$id==participants$id[i]]
ks$w.IS1<-ks$value*participants$acc.ind/(ks$value*participants$acc.ind+acc.s1)
ks$w.IS2<-ks$value*participants$acc.ind/(ks$value*participants$acc.ind+acc.s2)
ks$t.IS1<-participants$acc.ind/(participants$acc.ind+acc.s1)
ks$t.IS2<-participants$acc.ind/(participants$acc.ind+acc.s2)
ks$diff<-(1-ks$w.IS2)-(1-ks$w.IS1)
ks$acc<-participants$acc.ind
ks$age<-participants$age
ks$age.fac<-ifelse(ks$age>59,1,0)



#which ks to use; sort by directory
ks_transfer<-ks
ks_transfer$id<-paste("DG_",ks$id,sep="")
part.id<-paste("DG_",participants$ID,sep="")
ks_transfer<-ks_transfer[order(ks_transfer$id),]
part.acc<-participants$acc.ind[order(part.id)]
#ks_transfer<-ks_transfer[which(ks_transfer$id%in%directories),]
require(reshape2)
ks_plot<-melt(ks_transfer[,1:3],id.vars=c("id"))
ks_transfer<- ((as.numeric(((unlist(scale(1/ks_transfer[1])))))))
#ks_transfer<-(as.numeric(((unlist(ks_transfer[1])))))

#write.table(ks_transfer,'clipboard',col.names=F,row.names=F)
names(participants)
plot((1-participants$w.IS1)~participants$acc.ind,ylim=c(0,1))
points(I(1-participants$w.IS2)~I(participants$acc.ind*ks_transfer))
points(I(1-participants$w.SS)~participants$acc.s2)
curve()
#########################
#plot parameters from models
#########################
require(ggplot2)
require(reshape2)
levels(ks_plot$variable)<-c("k_i","k_s")
ks_plot.2<-data.frame(with(ks_plot,aggregate(value,list(variable),mean)),with(ks_plot,aggregate(value,list(variable),sd)$x))
names(ks_plot.2)<-c("Variable","Mean","SD")
pdf("Dist_param_model.pdf")
p.modparam<-ggplot(aes(y=value,x=variable),data=ks_plot)+geom_boxplot()+theme_bw()+xlab("Parameter")+ylab("Model Estimate")+scale_x_discrete(labels=(c(expression(k[i]), expression(k[s]))))+geom_abline(intercept=1,slope=0,linetype=2)+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+ylim(0,20)
print(p.modparam)
dev.off()
require(Cairo)

my.data$BOslope<-0.5
#write Bayes Optimal slope
for(i in 1:nrow(participants)){    
  my.data$BOslope[my.data$id==participants$id[i]&my.data$treat==1]<-participants$w.IS1[i]
  my.data$BOslope[my.data$id==participants$id[i]&my.data$treat==2]<-participants$w.IS2[i]
  my.data$BOslope[my.data$id==participants$id[i]&my.data$treat==3]<-participants$w.SS[i]
}


require(grid)
##########################################
#plot player decisions
require(ggplot2)
#names(my.data)
############
#add slopes to my.data
###########
my.data$player<-0
for (i in 1:nrow(participants)){
  my.data$player[my.data$id==factor(unique(my.data$id),ordered=F)[i]]<-i    
}
my.data$slope<-0
my.data$intercept<-0
my.data$slope[which(my.data$treat==0)]<-0.5
for (i in 1:nrow(participants)){
  my.data$slope[which(my.data$treat==1&my.data$player==i)]<-participants$w.IS1[i]
  my.data$slope[which(my.data$treat==2&my.data$player==i)]<-participants$w.IS2[i]
  my.data$slope[which(my.data$treat==3&my.data$player==i)]<-participants$w.SS[i]
}
####################
#get best model for each player
###################
for (i in 1:nrow(participants)){
  mo<-which(results[i,]==min(results[i,1:7],na.rm=T))
  mo<-mo[1]
  s.c<-soc.coef[i]
  current.id<-unique(my.data$id)[i]
  acc.ind<<-participants$acc.ind[participants$id==current.id]
  if (mo==1){
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-participants$w.IS1[i]
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-participants$w.IS2[i]
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-participants$w.SS[i]
  }
  if (mo==2){
    k.s<-as.numeric(coef(all.models[[i]][[2]]))
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-acc.ind/(acc.ind+k.s*acc.s1)
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-acc.ind/(acc.ind+acc.s2)
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-k.s*acc.s1/(k.s*acc.s1+acc.s2)
  }
  if (mo==3){
    k.s<-as.numeric(coef(all.models[[i]][[3]]))
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-acc.ind/(acc.ind+acc.s1)
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-acc.ind/(acc.ind+k.s*acc.s2)
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-acc.s1/(acc.s1+k.s*acc.s2)
  }
  if (mo==4){
    k<-as.numeric(coef(all.models[[i]][[4]]))
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-k*acc.ind/(k*acc.ind+acc.s1)
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-k*acc.ind/(k*acc.ind+acc.s2)
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-acc.s1/(acc.s1+acc.s2)
  }
  if (mo==5){
    k<-as.numeric(coef(all.models[[i]][[5]])[1])
    k.s<-as.numeric(coef(all.models[[i]][[5]])[2])
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-k*acc.ind/(k*acc.ind+k.s*acc.s1)
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-k*acc.ind/(k*acc.ind+acc.s2)
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-k.s*acc.s1/(k.s*acc.s1+acc.s2)
  }
  if (mo==6){
    k<-as.numeric(coef(all.models[[i]][[6]])[1])
    k.s<-as.numeric(coef(all.models[[i]][[6]])[2])
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-k*acc.ind/(k*acc.ind+(acc.s2*s.c/(1-s.c)))
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-k*acc.ind/(k*acc.ind+acc.s2)
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-acc.s1/(acc.s1+k.s*acc.s2)
  }
  if (mo==7){
    k<-as.numeric(coef(all.models[[i]][[7]]))
    my.data$slope[which(my.data$treat==1&my.data$player==i)]<-k
    my.data$slope[which(my.data$treat==2&my.data$player==i)]<-k
    my.data$slope[which(my.data$treat==3&my.data$player==i)]<-k
  }
  #if (mo==8){
  # k<-as.numeric(coef(all.models[[i]][[7]]))
  #my.data$slope[which(my.data$treat==1&my.data$player==i)]<-k[1]*(acc.ind/(acc.ind+acc.s1))
  #my.data$slope[which(my.data$treat==2&my.data$player==i)]<-k[2]*(acc.ind/(acc.ind+acc.s2))
  #my.data$slope[which(my.data$treat==3&my.data$player==i)]<-k[3]*(acc.s1/(acc.s2+acc.s1))
  #}  
}

require(ggplot2)
##################
#calculate differences
#######################
#sub.data<-subset(my.data,id==my.data$id[[1]])
pdf("decisions_all_participants.pdf",onefile=T)
for(i in 1:20){
  play1<-i*5+1
  play2<-i*5+5
  p.1<-ggplot(aes(y=correction,x=distance),data=subset(my.data,distance<30&player%in%unique(my.data$player)[play1:play2]))+geom_point(aes(colour=factor(treat)),size=1)+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue","red"))+theme(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))+geom_abline(aes(slope=BOslope,intercept=intercept,colour="red"))+geom_text(aes(x=5,y=25,label=as.character(round(value,2)),size=4),data=subset(ks,id%in%unique(my.data$id)[play1:play2]))+theme(text=element_text(size=6))
print(p.1)
}
p.1<-ggplot(aes(y=correction,x=distance),data=subset(my.data,distance<30))+geom_point(aes(colour=factor(treat)),size=4)+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue","red"))+theme(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))+geom_abline(aes(slope=BOslope,intercept=intercept,colour="red"))+geom_text(aes(x=5,y=25,label=as.character(round(value,2)),size=4),data=subset(ks,id%in%unique(my.data$id)))+theme(text=element_text(size=6))
dev.off
my.data$player
play1<-51
play2<-51
p.1<-ggplot(aes(y=correction,x=distance),data=subset(my.data,distance<30&id%in%unique(my.data$player)[play1:play2]))+geom_point(aes(colour=factor(treat)),size=1)+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue","red"))+theme(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))+geom_abline(aes(slope=BOslope,intercept=intercept,colour="red"))+geom_text(aes(x=5,y=25,label=as.character(round(value,2)),size=4),data=subset(ks,id%in%unique(my.data$id)[play1:play2]))+theme(text=element_text(size=6))
print(p.1)
dev.off()

require(ggplot2)
ggplot(aes(y=correction,x=distance),data=subset(my.data,distance>-180&correction>-180&correction<180&distance<180&treat==0&id>500&id<600))+geom_point()+facet_grid(~id)+geom_smooth(method="lm")+theme_classic()+geom_abline(aes(intercept=0,slope=0.5))




my.data2<-my.data[order(my.data$id),]
players.per.page<-5
for (i in 1:(nrow(participants)%/%players.per.page)){
  selector<-unique(my.data2$id)[((i-1)*players.per.page+1):(i*players.per.page)]
  sub.data<-subset(my.data2,id%in%selector)
  p.1<-ggplot(aes(y=correction,x=distance),data=subset(sub.data,distance<30))+geom_point(aes(colour=factor(treat)))+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue"))+opts(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))#+stat_smooth(method="lm",formula = y ~ x-1,colour="red")
  print(p.1)
}
s.1<-nrow(participants)-nrow(participants)%%players.per.page+1
s.2<-nrow(participants)
selector<-unique(my.data2$id)[s.1:s.2]
rm(s.1,s.2)
sub.data<-subset(my.data2,id%in%selector)
p.1<-ggplot(aes(y=correction,x=distance),data=subset(sub.data,distance<30))+geom_point(aes(colour=factor(treat)))+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue"))+opts(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))#+stat_smooth(method="lm",formula = y ~ x-1,colour="red")
print(p.1)
rm(my.data2)
dev.off()
require(ggplot2)
unique(my.data$id)


p.1<-ggplot(aes(y=(correction),x=(distance)),data=subset(my.data,correction<180&distance<180&distance>0&correction>0&id%in%unique(my.data$id)[5]))+geom_point(aes(colour=factor(treat)),size=5)+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue","red"))+theme(legend.position="none")+stat_smooth(method="lm")+theme(text=element_text(size=20))
p.test<-ggplot(aes(y=correction/distance,x=trial),data=subset(my.data,distance<30&id%in%unique(my.data$id)[21:29]))+geom_point(aes(colour=factor(treat)),size=1)+facet_grid(id~treatVerb)+theme_bw()+scale_colour_manual(values=c("red","greenyellow","green","blue","red"))+theme(legend.position="none")+ylim(0,1)
p.1
names(my.data)
plot(log(ks$value)~log(participants$acc.ind))
summary(lm(log(ks$value)~log(participants$acc.ind)))


####################
#BIC weights
###################

require(xtable)
results.BIC<-results[,-c(2,5)]
results.BIC[,2]<-apply(results[,c(2,3)],1,min,na.rm=T)
results.BIC[,4]<-apply(results[,c(5,6)],1,min,na.rm=T)
names(results.BIC)<-c("Bayes Optimal","BO social modifier","BO individual modifier","BO inidvidual and social modifier","AID","id")
results.BIC[,c(1:5)]<-results.BIC[,c(1:5)]-apply(results.BIC[,c(1:5)],1,min)
results.BIC[,c(1:5)]<-exp(-0.5*exp(results.BIC[,c(1:5)]))
results.BIC[,c(1:5)]<-(results.BIC[,c(1:5)]/rowSums(results.BIC[,c(1:5)]))
colSums(results.BIC[,c(1:5)])/nrow(results.BIC)
BICweight.table<-data.frame(cbind(apply(results.BIC[,c(1:5)],2,mean),apply(results.BIC[,c(1:5)],2,sd)))
names(BICweight.table)<-c("Mean BIC weights","SD BIC weights")
xtable(BICweight.table)
apply(results.BIC[,c(1:5)],2,mean)
apply(results.BIC[,c(1:5)],2,sd)

participants
###########
#Distance difference between treatments
############
require(ggplot2)
require(reshape)
p.dist<-ggplot(aes(y=distance,x=treatVerb),data=my.data)+geom_boxplot()

with(subset(my.data,distance<30),aggregate(distance,list(player,treat),mean))

################
#plots presentation Bielefeld
################
#Deviation Phase 1 vs Phase 2
helper.data<-cbind(sqrt(participants$acc.ind),sqrt(participants$acc.ind2),participants$id)
helper.data<-melt(helper.data,"id")
plot.col<-c("#D90404","#F2A663","#03258C","#400601")
names(helper.data)<-c("id","phase","value")

ggplot(aes(y=value,x=factor(phase,labels=c("Phase 1","Phase 2")),fill=factor(phase)),data=helper.data)+geom_boxplot()+theme_bw()+ylim(0,20)+xlab(label="")+scale_fill_manual(values=plot.col[c(1,3)])+opts(legend.position="none")+theme(axis.text=element_text(size=14))+theme(axis.title=element_text(size=14))+ylab("Mean Deviation\nfrom correct location")
#######sample player
require(Cairo)
sub.data<-(my.data)
sub.data$treatVerb<-factor(sub.data$treatVerb)
levels(sub.data$treatVerb)<-c(expression(I:I),expression(I:S[HIGH]),expression(I:S[LOW]),expression(S[LOW]:S[HIGH]))
ggplot(aes(y=correction,x=distance),data=subset(sub.data,distance<30&player==15))+geom_point(aes(colour=factor(treat)))+facet_grid(.~treatVerb,labeller=label_parsed)+theme_bw()+scale_colour_manual(values=plot.col)+opts(legend.position="none")+geom_abline(aes(slope=slope,intercept=intercept))+geom_abline(aes(slope=BOslope,intercept=intercept,colour=factor(0)))#stat_smooth(method="lm",formula = y ~ x-1,colour="red")
names(sub.data)
sub.data$player
###deviation from Bayes optimal choice
ggplot(aes(y=value,x=variable),data=coef.p)+geom_boxplot(col=plot.col[c(2,3,4)])+ylim(-0.5,0.5)+theme_bw()+ylab("Deviation from\noptimal choice")+xlab("")+scale_x_discrete(labels=(c(expression(I:S[HIGH]), expression(I:S[LOW]),expression(S[LOW]:S[HIGH]))))+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+geom_abline(intercept=0,slope=0,linetype=2)
CairoSVG("F2a.svg")
ggplot(aes(y=value,x=variable),data=coef.p)+geom_boxplot()+ylim(-0.5,0.5)+theme_bw()+ylab("Deviation from\noptimal choice")+xlab("")+scale_x_discrete(labels=(c(expression(I:S[HIGH]), expression(I:S[LOW]),expression(S[LOW]:S[HIGH]))))+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+geom_abline(intercept=0,slope=0,linetype=2)
dev.off()
CairoSVG("F2b.svg")
print(ggplot(aes(y=value,x=log(accuracy)),data=coef.p.sub)+facet_grid(.~variable,labeller=label_parsed)+geom_point(size=2)+theme_bw()+ylim(-0.5,0.5)+geom_smooth(method="lm",colour="black",se=FALSE)+ylab("Deviation from\noptimal choice")+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+geom_vline(aes(xintercept=vline),data=coef.p.sub, linetype="dotted"))
dev.off()
#stats for correlation plot
levels(coef.p.sub$variable)
coef.p.sub$accuracy2<-log(coef.p.sub$accuracy)
m.1<-with(subset(coef.p.sub,variable=="I:S[HIGH]")[-7,],lm(value~accuracy2))
summary(m.1)

####Bayes Models vs Heuristics
plot.BIC<-rep(0,51*3)
dim(plot.BIC)<-c(3,51)
for (i in 1:51)
plot.BIC[,i]<-c(min(results[i,1:6],na.rm=T)[1],(results[i,7]),(results[i,8]))
plot.BIC<-as.data.frame(t(plot.BIC))
for (i in 1:51)
  plot.BIC[i,]<-plot.BIC[i,]-min(plot.BIC[i,])
#calculate weights
names(plot.BIC)<-c("Bayes","Mean Dev","Full Model")
weights<-plot.BIC
require(reshape2)
plot.BIC<-melt(plot.BIC)
plot.BIC$id<-seq(1,51,1)
names(plot.BIC)<-c("strategy","dBIC","id")
ggplot(aes(x=(dBIC),y=id,col=strategy),data=plot.BIC)+geom_point()+theme_bw()+scale_colour_manual(values=plot.col)+geom_vline(xintercept=2,linetype=3)+xlim(0,20)
require(reshape)
require(ggplot2)
for (i in 1:51)
  weights[i,]<-as.numeric(exp(-plot.BIC[i,]/2)/sum(exp(-plot.BIC[i,]/2)))
names(weights)<-c("Bayes","Mean Dev","Full Model")
weights<-melt(weights)
weights$id<-seq(1,51,1)
names(weights)<-c("strategy","BIC_weight","id")
ggplot(aes(y=BIC_weight,x=strategy,col=strategy),data=weights)+geom_boxplot()+theme_bw()+scale_colour_manual(values=plot.col[c(1,2,3)])+ylab("BIC weights")+opts(legend.position="none")

theme_get()


#############
#calc BIC weights
############
results.new<-cbind(apply(results[,1:6],1,min,na.rm=T),results[,7],results[,8])
results.new<-results.new-apply(results.new,1,min,na.rm=T)
results.new<-exp(-0.5*results.new)
results.new<-(results.new/rowSums(results.new))
mean(results.new[,3])
sd(results.new[,3])


######
#comparison with low age group
####
require(ggplot2)
#remove players with non Bayes strategy
test<-rep(0,51)
for (i in 1:51)
  test[i]<-(min(results[i,1:6],na.rm=T)[1]<min(c(results[i,7],results[i,8])))
sum(participants$age[which(test==0)]<60)
sum(participants$age<60)



ks$age.fac2<-0
ks$age.fac2<-ifelse(ks$age<40,0,ifelse(ks$age>60,2,1))
ks.plot<-ks[which(test==T),]
ks.plot2$age.fac2<-factor(ks.plot$age.fac2,labels=c("25-39","40-59","60+"))            
ks.plot2$age.fac3<-0
ks.plot2$age.fac3<-ifelse(ks.plot2$age<40,0,ifelse(ks.plot2$age>60,2,1))
ks.plot2$age.fac3<-factor(ks.plot2$age.fac3,labels=c("25-39","40-59","60+"))            
pdf("main_result.pdf")
print(ggplot(aes(y=log(value),x=log(acc),col=factor(age.fac2)),data=ks.plot)+geom_point(size=5)+theme_bw()+geom_smooth(method='lm',se=F,size=1.2)+ylab("log(k)")+xlab("log(accuracy)"))+theme(grid.major=element_blank())
print(ggplot(aes(y=log(value),x=log(acc),col=age),data=ks.plot)+geom_point(size=5)+theme_bw()+geom_smooth(method='lm',se=F,size=1.2)+ylab("log(k)")+xlab("log(accuracy)"))+theme(grid.major=element_blank())

dev.off()
####
#addmri
####
ks.import<-read.table("clipboard",sep=";")
ks.import$age<-0
ks.import$age.fac<- -1
ks.import$age.fac2<-"MRI"

ks.plot2<-rbind(ks.plot,ks.import)


m.1<-lm(log(value)~log(acc)+age+log(acc):age,data=ks.plot)
summary(m.1)
require(xtable)
xtable(summary(m.1))
require(car)
qqPlot(residuals(m.1))
plot(residuals(m.1)~fitted(m.1))
plot(m.1)



ks.import$age<-ages$V1[which(ids$V1%in%ks.import$id)]
require(ggplot2)
p.1<-ggplot(aes(y=correction,x=distance),data=subset(my.data,distance<30&id==101))+geom_point()+facet_wrap(~treat)
ggplot(aes(y=correction,x=distance),data=subset(my.data,id==210&correction<250&correction>0))+geom_point()+facet_wrap(~treat)
f.res.2<-list()
for (i in 1:nrow(participants)){
  subby<-subset(my.data,id==participants$id[i]&treat==2&correction<300&correction>0)
  f.res.2[[i]]<-with(subby,lm(correction~distance-1))
}

plot((as.numeric(lapply(f.res.1,coef)))~(as.numeric(lapply(f.res.2,coef))))
abline(lm(((as.numeric(lapply(f.res.1,coef)))~(as.numeric(lapply(f.res.2,coef))))))


names(my.data)

require(gplot2)
c.id<-6
participants$id
pdf('single_player_data.pdf')
for(c.id in 1:max(my.data$player))
print(ggplot(aes(y=correction,x=distance),data=subset(my.data,correction<180&player%in%unique(my.data$player)[c.id]))+geom_point(aes(colour=factor(treat)),size=1)+facet_grid(id~treatVerb,scales="free")+theme_classic()+scale_colour_manual(values=c("red","green","blue"))+theme(legend.position="none")+theme(text=element_text(size=20))+geom_smooth(method="lm"))
dev.off()

