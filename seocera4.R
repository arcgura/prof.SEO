rm(list=ls())
library(dplyr)
library(tidyr)

## read data without exclusion code
codebook <- read.csv("~/R/codebook.csv",stringsAsFactors = F)
excode<-codebook$code_for_exclusion
sarcopenia <- read.table("~/R/Sarcopenia.csv", header=TRUE, sep=",", comment.char="", na.strings=c(excode,"NA","","."), stringsAsFactors=F)

## change 'Date'(index date) to readable one
library(readr)
index.date<-parse_date(sarcopenia$Date, format="%d-%b-%y")
sarcopenia[,2]<-index.date

## divide to demographic, CT code, CT date data
demographic.data<-sarcopenia[,c(1:10)]

  #l.code <- grepl("code", names(sarcopenia))
  #code.data <- sarcopenia[,l.code]
code.data <- select(sarcopenia, contains("_code"))
sum(!is.na(code.data)) 

  #l.data <- grepl("date", names(sarcopenia))
  #date.data <- sarcopenia[, l.data]
date.data <- select(sarcopenia, contains("_date"))
sum(!is.na(date.data)) 


## remove non matching data with "code data" in "date data" &  Convert 'date' to 'date difference' 
dif.date <- date.data

for (i in 1:82) {
  date.data[is.na(code.data[,i]), i] <- NA
  dif.date[[i]] <- as.numeric(as.Date(date.data[[i]], "%d/%m/%Y") - demographic.data[,2])
}

sum(!is.na(date.data)) 

hist(as.matrix(dif.date), breaks = 100 )

## make index for "diff.date"
Number<-demographic.data$Number
dif.date <- cbind(Number, dif.date)

## makle variable to value (get tidy data)
tidy.dif.date <- gather(dif.date, CT, dif, pre_CT01_date:post_CT50_date, na.rm = TRUE )

     ### sorting for solving of tie-break (tie-break rule : higer)
     #arr.tidy.dif.date <- arrange(tidy.dif.date, Number, desc(dif))

## make a subset according to ranges of given date differences
tidy.low <- filter(tidy.dif.date, dif >= -180 & dif<=-90)
tidy.high <- filter(tidy.dif.date, dif >= -1 & dif<=1)

## remove extra value according to tie-break rule (pre : lower / index : higher)  
tidy.low <- arrange(tidy.low, Number, dif)
tidy.high <- arrange(tidy.high, Number, desc(dif))
rm.low <- tidy.low[!duplicated(tidy.low$Number),]
rm.high <- tidy.high[!duplicated(tidy.high$Number),]

## extract targetting rows
inter <- intersect(rm.high$Number, rm.low$Number)
rm.low.sub <- filter(rm.low, Number %in% inter)
rm.high.sub <- filter(rm.high, Number %in% inter)
demo.sub<-filter(demographic.data, Number %in% inter)

## merging of tidy data

#pre_CT<-rm.low.sub$dif
#index_CT<-rm.high.sub$dif
#merge_tidy <- cbind(demo.sub, pre_CT, index_CT)

names(rm.low.sub) <- c("Number", "pre.CT", "pre.CT.date")
names(rm.high.sub) <- c("Number", "index.CT", "index.CT.date")
merge1 <- merge(rm.low.sub, rm.high.sub)
merge2 <- merge(demo.sub, merge1)


## converting date differencce to CT date

#merge_tidy$pre_CT <- merge_tidy$pre_CT + merge_tidy$Date
#merge_tidy$index_CT <- merge_tidy$index_CT + merge_tidy$Date

# merge2$pre.CT.date <- merge2$pre.CT.date + merge2$Date
# merge2$index.CT.date <- merge2$index.CT.date + merge2$Date

merge2 <- mutate(merge2, pre.CT.date = pre.CT.date + merge2$Date, index.CT.date = index.CT.date + merge2$Date)
