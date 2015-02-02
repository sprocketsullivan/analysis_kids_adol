##########
#Analysis Reaction times
#########
require(arm)

m.RT<-lmer(RT~factor(treat)+log(movement)+(1|id),data=subset(my.data,RT>0&movement>0))
display((m.RT))

m.RT.sim<-sim(m.RT)
fixef(m.RT)
se.coef(m.RT)$fixef
coef.names<-c("IND\nS_Low","IND\nS_High","S_Low\nS_High","Distance\nmoved")
pdf("reaction_times_estimates.pdf")
coefplot(as.numeric(fixef(m.RT))[2:5],se.coef(m.RT)$fixef[2:5],xlim=c(0,0.7),varnames=coef.names)
dev.off()
hist(my.data$movement)
names(my.data)

########################
#plot for deviations
########################
coef.p<-as.data.frame(coefficients.diff)
coef.p$id<-participants$ID
coef.p$acc.ind<-participants$acc.ind
names(coef.p)<-c("I:S_High","I:S_Low","S_Low:S_High","id","accuracy")
require(reshape)
require(ggplot2)
coef.p<-melt(coef.p,id.vars=c("id","accuracy"))
pdf("deviation_plot1.pdf")
p.devia<-ggplot(aes(y=value,x=variable),data=coef.p)+geom_boxplot()+ylim(-0.5,0.5)+theme_bw()+ylab("Deviation from\noptimal choice")+xlab("")+scale_x_discrete(labels=(c(expression(I:S[HIGH]), expression(I:S[LOW]),expression(S[LOW]:S[HIGH]))))+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+geom_abline(intercept=0,slope=0,linetype=2)
print(p.devia)
dev.off()
cor.test(participants$acc.ind,coefficients.diff[,1],method="kendall")
cor.test(participants$acc.ind,coefficients.diff[,2],method="kendall")

coef.p.sub<-subset(coef.p,variable%in%c("I:S_High","I:S_Low"))
coef.p.sub$vline<-log(acc.s1)
coef.p.sub$vline[coef.p.sub$variable=="I:S_High"]<-log(acc.s2)
levels(coef.p.sub$variable)<-c(expression(I:S[HIGH]),expression(I:S[LOW]),expression(S[LOW]:S[HIGH]))
pdf("deviation_plot2.pdf")
p.cor.dev<-ggplot(aes(y=value,x=log(accuracy)),data=coef.p.sub)+facet_grid(.~variable,labeller=label_parsed)+geom_point()+theme_bw()+ylim(-0.5,0.5)+geom_smooth(method="lm",colour="black")+ylab("Deviation from\noptimal choice")+theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())+geom_vline(aes(xintercept=vline),data=coef.p.sub, linetype="dotted")
print(p.cor.dev)
dev.off()
