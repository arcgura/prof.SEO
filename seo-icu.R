#read date

raw <-read.csv("ICU.csv")

### split by sec

sec<-as.integer(raw$sec)
#sec <- trunc(ICU_monitor_data$sec)

raw$sec<-sec
s<-split(raw, raw$sec)
m<-lapply(s, colMeans)
d<-as.data.frame(m)
d2<-as.data.frame(t(d))
row.names(d2)<-d2$sec

#ICP.m<-tapply(raw$ICP, raw$sec, mean)
#MAP.m<-tapply(raw$MAP, raw$sec, mean)
#bind<-rbind(ICP.m,MAP.m)
#bind<-as.data.frame(t(bind))
#write.csv(mean_file_sec, "C:/Users/AMC/Desktop/ICU monitor data/mean per sec by CHS")


### split by min and correlation 

m<-as.integer(d2$sec/60)
#min <- trunc(mean_file_sec$sec/60)

min.d<-d2
min.d$sec<-m

m.split<-split(min.d,min.d$sec)
m.col<-lapply(m.split, function(x){
    cor(x[,2], x[,3])
})
#correlation_min <- sapply(split(ICU_monitor_data_min, ICU_monitor_data_min$min), function(x){
# cor(x$ICP, x$MAP, method = "pearson"))  }

m.col.v<-as.numeric(m.col)
m.col.d<-data.frame(names(m.col), m.col.v)
names(m.col.d)<-c("minute","correlation")


#write.csv(correlation_file_min, "C:/Users/AMC/Desktop/ICU monitor data/correlation coefficient per min by CHS")
