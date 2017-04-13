
library(dplyr)

### Cleaning
## Exclude chest CT, Open raw data.
code <- read.table("~/R/codebook.csv", header=TRUE, sep=",", comment.char="", na.string=c("NA", "", "."), stringsAsFactors=F)
code1 <- code[[1]]
s_raw <- read.table("~/R/Sarcopenia.csv", header=TRUE, sep=",", comment.char="", na.string=c(code1,"NA","","."), stringsAsFactors=F)

## Conversion of date class, Remove date of chest CT
library(readr)
index.date<-parse_date(s_raw$Date, format="%d-%b-%y")
s_raw$Date<-index.date

for(i in 12:174){
    if (i%%2==0){
        s_raw[[i]][is.na(s_raw[[i+1]])]<- NA  }
}

for(i in 12:175 ) {
    if (i%%2 == 0) {
        s_raw[[i]]<-as.Date(s_raw[[i]], "%d/%m/%Y")
    }
}

## Check numbers of CT are correct. And replace it.
CT.num<-0
total.CT.num<-0
for(i in 1:1547) {
      CT.num <- sum(!is.na(s_raw[i,c(12:175)]))/2
      s_raw$Total.CT[i]<-CT.num
      total.CT.num<-total.CT.num+CT.num
}
print(total.CT.num)

## mutate for date calculation
s_raw_mut<-s_raw

for(i in 12:175) {
    if (i%%2 == 0) {
        s_raw_mut[,i] <- as.numeric(difftime(s_raw[,i], s_raw[,2], units="days"))
    }
}

### Subsetting
## filter within 2 years only
for(i in 1:1547) {
    for(j in 12:174) {
         if (j%%2  == 0) {
            if (is.na(s_raw_mut[i,j])) {
                s_raw_mut[i,j] <- NA
            }
            else if (s_raw_mut[i,j]>365 | s_raw_mut[i,j] < -365) {
                s_raw_mut[i,j] <- NA
            }
         }
    }
}

## s_index: index dataset: day -1 (48.6%) to day 1 (63.0%)
s<-vector()

for (i in 1:1547) {
    if ( 0 %in% s_raw_mut[i,c(12:174)]  | -1 %in% s_raw_mut[i,c(12:174)]  | 1 %in% s_raw_mut[i,c(12:174)] ) {
        s[i] <- i
    }
}

s2<-s[!is.na(s)]
length(s2)
index<-s_raw_mut[s2,]

## pre: 90~180 days, post = 6,7,8 days
PRE<-vector()
POST<-vector()
for(i in 1:821) {
      PRE[i]<-any( index[i,c(12:175)] >= -180 & index[i,c(12:175)] <= -90, na.rm=TRUE)
      POST[i]<- any( index[i,c(12:175)] >= 6 & index[i,c(12:175)] <= 8, na.rm=TRUE)
}     


index[,176]<-PRE
index[,177]<-POST

result <- data.frame()

result<-index[index[,86]==TRUE & index[,87]==TRUE,]

nrow(result)
