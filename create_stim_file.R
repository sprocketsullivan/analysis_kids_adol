#####
#create new stimulus file
#####
stimulus.data<-read.csv("stimulus_kids_same.csv",sep=";")
target_accuracy<-12
accuracies.1<-rnorm(60,0,12)
accuracies.2<-sample(accuracies.1,replace=F)
mean(accuracies.1)
sd(accuracies.1)
stimulus.data$s1[2:61]<-stimulus.data$s1[2:61]+accuracies.1
stimulus.data$s2[2:61]<-stimulus.data$s2[2:61]+accuracies.2
stimulus.data$s1[2:61]<-ifelse(stimulus.data$s1[2:61]>360,stimulus.data$s1[2:61]-360,stimulus.data$s1[2:61])
stimulus.data$s1[2:61]<-ifelse(stimulus.data$s1[2:61]<0,360+stimulus.data$s1[2:61],stimulus.data$s1[2:61])
stimulus.data$s2[2:61]<-ifelse(stimulus.data$s2[2:61]>360,stimulus.data$s2[2:61]-360,stimulus.data$s2[2:61])
stimulus.data$s2[2:61]<-ifelse(stimulus.data$s2[2:61]<0,360+stimulus.data$s2[2:61],stimulus.data$s2[2:61])
write.table(stimulus.data,paste("stimulus_acc_",target_accuracy,"_same.csv",sep=""),sep=";")
