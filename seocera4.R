rm(list=ls())
library(dplyr)
library(tidyr)

## read data without exclusion code
codebook <- read.csv("~/R/codebook.csv",stringsAsFactors = F)
excode<-codebook$code_for_exclusion

sarcopenia <- read.table("~/R/Sarcopenia.csv", header=TRUE, sep=",", comment.char="", na.strings=c(excode,"NA","","."), stringsAsFactors=F)
library(readr)
index.date<-parse_date(sarcopenia$Date, format="%d-%b-%y")
sarcopenia[,2]<-index.date

## subset for demographic data
demographic.data<-sarcopenia[,c(1:10)]

## subset for code data  
l.code <- grepl("code", names(sarcopenia))
code.data <- sarcopenia[,l.code]
sum(!is.na(code.data)) 

## subset for date data
l.data <- grepl("date", names(sarcopenia))
date.data <- sarcopenia[, l.data]
sum(!is.na(date.data)) 


## remove unmatching data of date.data & convert date.date to dif.date
dif.date <- date.data

for (i in 1:82) {
  date.data[is.na(code.data[,i]), i] <- NA
  dif.date[[i]] <- as.numeric(as.Date(date.data[[i]], "%d/%m/%Y") - demographic.data[,2])
  
}

hist(as.matrix(dif.date), breaks = 100 )

Number<-demographic.data$Number
dif.date <- cbind(Number, dif.date)

tidy.dif.date <- gather(dif.date, index, dif, pre_CT01_date:post_CT50_date, na.rm = TRUE )
arr.tidy.dif.date <- arrange(tidy.dif.date, Number, desc(dif))

 
tidy.low <- filter(arr.tidy.dif.date, dif >= -180 & dif<=-90)
tidy.high <- filter(arr.tidy.dif.date, dif >= -1 & dif<=1)
       
rm.low <- tidy.low[!duplicated(tidy.low$Number),]
rm.high <- tidy.high[!duplicated(tidy.high$Number),]

inter <- intersect(rm.high$Number, rm.low$Number)

rm.low.sub <- filter(rm.low, Number %in% inter)
rm.high.sub <- filter(rm.high, Number %in% inter)
demo.sub<-filter(demographic.data, Number %in% inter)

pre_CT<-rm.low.sub$dif
index_CT<-rm.high.sub$dif

merge_tidy <- cbind(demo.sub, pre_CT, index_CT)

merge_tidy$pre_CT <- merge_tidy$pre_CT + merge_tidy$Date
merge_tidy$index_CT <- merge_tidy$index_CT + merge_tidy$Date
