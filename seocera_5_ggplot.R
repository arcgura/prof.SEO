### time
ggplot(rawdata, aes(month, fill=sex)) + geom_bar() + facet_grid(.~wday) +
    ggtitle("Total / Wday / Month / Sex ")
    
ggplot(rawdata, aes(month)) + geom_bar(fill="sky blue", colour="black") + facet_grid(.~year) +
    ggtitle("Total / Month / year ")


ggplot(burndata, aes(month)) + geom_bar(fill="sky blue", colour="black") + facet_grid(.~year) +
    ggtitle("Burn / Month / year ")


ggplot(burndata, aes(wday)) + geom_freqpoly(aes(group = sex, colour = sex), stat = "count") +
    ggtitle("burn / Weak / Sex")

ggplot(burndata, aes(month)) + geom_freqpoly(stat = "count") + facet_grid(.~year) +
    ggtitle("burn / Month/ Year")



ggplot(burndata, aes(route)) + geom_bar(fill="sky blue", colour="black")+
    ggtitle("Burn / route ")

ggplot(rawdata, aes(route)) + geom_bar(fill="sky blue", colour="black")+
    ggtitle("Total / route ")

ggplot(rawdata, aes(x = month, y = (..count..)/sum(..count..))) +  facet_grid(.~vector_burn) +
    geom_bar() +
    scale_y_continuous(labels=percent)



ggplot(subdata, aes(x = burn, y= pulse_rate)) + geom_boxplot(na.rm = TRUE)


ggplot(subdata, aes(x = route)) + geom_bar() + facet_grid(.~route_vehicle)

