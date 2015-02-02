######
#analysis kids adolescents slope
#####
#execute first lines of modelling 2 first
slopes<-data.frame(value=rep(0,nrow(participants)*3))
slopes$treat<-''
require(ggplot2)
require(MASS)
for (i in 1:nrow(participants)){
subby<-subset(my.data,id==unique(my.data$id)[i])
#names(my.data)
#ggplot(aes(y=correction,x=distance),data=subset(subby,correction<180))+geom_point()+geom_smooth(method="lm")+theme_classic()+facet_wrap(~treatVerb)
#needed: 3 linear regressions through the origin
helper<-as.numeric(coef(lm(correction~distance-1,data=subset(subby,treatVerb=="Adult Kid"&correction<180))))
helper<-c(helper,as.numeric(coef(lm(correction~distance-1,data=subset(subby,treatVerb=="Ind Kid"&correction<180)))))
helper<-c(helper,as.numeric(coef(lm(correction~distance-1,data=subset(subby,treatVerb=="Ind Adult"&correction<180)))))
slopes$value[((i-1)*3+1):((i-1)*3+3)]<-helper
slopes$treat[((i-1)*3+1):((i-1)*3+3)]<-c("Adult Peer","Ind Peer","Ind Adult")
}
slopes$id<-rep(unique(my.data$id),each=3)
slopes$age<-rep((participants$age),each=3)
slopes$opt.weight<-0
slopes$opt.weight[(1+seq(0,nrow(slopes)-1,3))]<-0.5
slopes$opt.weight[(2+seq(0,nrow(slopes)-2,3))]<-participants$w.IS1
slopes$opt.weight[(3+seq(0,nrow(slopes)-3,3))]<-participants$w.IS1
slopes$deviation<-slopes$value-slopes$opt.weight
ggplot(aes(y=deviation,x=treat),data=slopes)+geom_boxplot()+ylim(-0.5,0.5)+facet_wrap(~age)
slopes$acc.ind<-rep(participants$acc.ind,each=3)
slopes$age2<-factor(slopes$age,labels=c("adolescents","kids"))
pdf("figure1.pdf")
fig.1<-ggplot(aes(y=deviation,x=acc.ind,col=age2),data=subset(slopes,acc.ind<20))+geom_point()+ylim(-0.25,0.5)+facet_wrap(~treat)+stat_smooth(method="lm",se=F)+theme_classic()+xlab("Individual accuracy degrees")+ylab("Deviation from Bayes Optimality")
print(fig.1)
dev.off()


plot(correction~distance-1,data=subset(subby,treatVerb=="Ind Kid"&correction<180))
participants$w.IS1[1]

a<-subset(slopes,treat=="Ind Adul"&age=="k")$deviation
b<-subset(slopes,treat=="Ind Kid"&age=="k")$deviation
mean(b)
wilcox.test(a,b,paired=T)

subby$treatVerb
sum(my.data$decisionTime==my.data$endTime)
nrow(my.data)

#mean accuracy
names(my.data)
require(ggplot2)
participants$age2<-factor(participants$age, labels=c("adolescents","kids"))
participants$gender2<-factor(participants$gender,labels=c("male","female"))
pdf("Figure_1.pdf")
ggplot(aes(y=acc.ind,x=age2,col=gender2),data=participants)+geom_boxplot()+scale_color_manual(values=c("red","blue"))+theme_classic()+xlab("")+ylab("Mean accuracy [degrees]")
dev.off()
participants$acc.ind2





