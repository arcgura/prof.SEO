#####   1    ##########################
getdata<-read.table("getdata.csv", header=TRUE, sep=",")

ibrary(tidyr)
library(dplyr)

getdata<-tbl_df(getdata)
getdata<-mutate(getdata, XX = getdata$ACR ==3 & getdata$AGS==6 )
which(getdata$XX)

######   2   #########################

install.packages("jpeg")
library(jpeg)

jp<-readJPEG("jf.jpg", native=TRUE)

class(jp)

quantile(jp, probs=c(0.3,0.8))

######   3   #######################

getdata2<-read.csv("getdata2.csv", header=TRUE, sep=",", stringsAsFactors = F, na.strings="")
getdata2<-getdata2[c(5:194), ]

getdata3<-read.csv("getdata3.csv", header=TRUE, sep=",", stringsAsFactors = F)


get2nc<-as.character(getdata2$X)
get2nc<-get2nc[!is.na(get2nc)]

get3nc<-getdata3$CountryCode

mat<-match(get2nc, get3nc)

sum(!is.na(mat))

#######   4    #######################################

get3sel<-select(getdata3, CountryCode, Income.Group)
get2sel<-getdata2[ ,c(1:2)]
get3sel<-get3sel[mat, ]
final<-bind_cols(get3sel, get2sel)
names(final)<-c("code","group","code2","rank")
final$rank<-as.numeric(final$rank)
final.gr<-group_by(final, group)
final.gr
summarise(final.gr, mean(rank))


#######   5   ######################################

final.ft<-filter(final, rank<=38)
sum(final.ft$group == "Lower middle income")

