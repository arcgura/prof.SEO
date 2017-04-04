rm(list=ls())
raw <-read.csv("ICU.csv")

#sec.list<-list()
#sec <- 1
#sec.list[[1]]<-data.frame()
#for (i in 1:467984) {
#  if (raw$sec[i] < sec) {
#        sec.list[[sec]]<-rbind(sec.list[[sec]], raw[i,])
#  }     
#  else {
#    sec<-sec+1
#    sec.list[[sec]]<-data.frame()
#  }
#}


### split by sec
sec<-as.integer(raw$sec)
raw$sec<-sec
s<-split(raw, raw$sec)
m<-lapply(s, colMeans)
d<-as.data.frame(m)
d2<-as.data.frame(t(d))
row.names(d2)<-d2$sec


### split by min

split(d2,cut)

