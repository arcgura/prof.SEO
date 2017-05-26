t_mon<-as.data.frame(table(rawdata$month))
t_mon<-mutate(t_mon, percentage = Freq / sum(Freq) * 100)

t_wday<-as.data.frame(table(rawdata$wday))
t_wday<-mutate(t_wday, percentage = Freq / sum(Freq) * 100)

t_hour<-as.data.frame(table(rawdata$visit_hour))
t_hour<-mutate(t_hour, percentage = Freq / sum(Freq) * 100)


b_mon<-as.data.frame(table(burndata$month))
b_mon<-mutate(b_mon, percentage = Freq / sum(Freq) * 100)

b_wday<-as.data.frame(table(burndata$wday))
b_wday<-mutate(b_wday, percentage = Freq / sum(Freq) * 100)

b_hour<-as.data.frame(table(burndata$visit_hour))
b_hour<-mutate(b_hour, percentage = Freq / sum(Freq) * 100)


ggplot(data=t_mon, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total/month")
ggplot(data=b_mon, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of burn/month")

ggplot(data=t_wday, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total/week")
ggplot(data=b_wday, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of burn/week")

ggplot(data=t_hour, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total/hour")

ggplot(data=b_hour, aes(x=Var1, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of burn/hour")


b_t_wday <- as.data.frame(table(best$day_week))
b_t_wday <- mutate(b_t_wday, percentage = Freq / sum(Freq) * 100)
w <- c(5,4,3,1,0,6,2)
b_t_wday <- cbind(b_t_wday, w)
b_t_wday <- arrange(b_t_wday, w)
b_t_wday

ggplot(data=b_t_wday, aes(x=w, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total/wday/bestian")



b_t_wday <- as.data.frame(table(best$))
b_t_wday <- mutate(b_t_wday, percentage = Freq / sum(Freq) * 100)
w <- c(5,4,3,1,0,6,2)
b_t_wday <- cbind(b_t_wday, w)
b_t_wday <- arrange(b_t_wday, w)
b_t_wday

ggplot(data=b_t_wday, aes(x=w, y=percentage)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total/wday/bestian")