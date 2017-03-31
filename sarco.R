rm(list=ls())

## read data without exclusion code

codebook <- read.csv("~/R/codebook.csv",stringsAsFactors = F)
excode<-codebook$code_for_exclusion

sarcopenia <- read.table("~/R/Sarcopenia.csv", header=TRUE, sep=",", comment.char="", na.strings=c(excode,"NA","","."), stringsAsFactors=F)
library(readr)
index.date<-parse_date(sarcopenia$Date, format="%d-%b-%y")

## reset number
sarcopenia[,1]<-c(1:1547)


sarco.code<-sarcopenia[c(1,2,11)]
sarco.date<-sarcopenia[c(1,2,11)]

## remove of unmatching date &  divide code column from main data
for(i in 12:175)  {
      if (i%%2 == 1)  {
            sarco.code<-cbind(sarco.code,sarcopenia[i])
            for(j in 1:1547) {
                  if (is.na(sarcopenia[j,i])==TRUE) {
                        sarcopenia[j,i-1]<-NA
                  }
            }
      }
}

## divide date column from main data and conversion to date class
for(i in 12:175 ) {
      if (i%%2 == 0) {
            sarcopenia[[i]]<-as.Date(sarcopenia[[i]], "%d/%m/%Y")
            sarco.date<-cbind(sarco.date,sarcopenia[i])
      }
}

## calculate the number of CT
for(i in 1:nrow(sarco.code) ) {
      code.row<-sarco.code[i,]
      code.row.n<-sum(!is.na(code.row[,c(4:85)]))
      sarco.code[i,3]<-code.row.n
      sarco.date[i,3]<-code.row.n
}

CT.number<-sum(sarco.code[,3])
print(CT.number)

## calculate the !is.na  date
sum(!is.na(sarco.date[,c(4:85)]))      
      
      
## calculate the number of patients
CT.people<-0
for(i in 1:nrow(sarco.code)) {
      if (sarco.code[i,3]==0)  {
            CT.people<-CT.people
      }
      else {
            CT.people<-CT.people + 1
      }
}
print(CT.people)

## Mutation 'scarco.date' to difference of date  
## and print histogram
sarco.date[,2]<-index.date
dif<-sarco.date
for(i in 4:ncol(sarco.date) ) {
            dif[,i]<-as.numeric(difftime(sarco.date[,i],sarco.date[,2], units="days"))
}

dif2<-as.matrix(dif[,c(4:85)])

hist(dif2, breaks = 100 )

## sort out cases of checking CT on index date.
s<-vector()

for (i in 1:1547) {
  if ( 0 %in% dif[i,c(4:85)]  | -1 %in% dif[i,c(4:85)]  | 1 %in% dif[i,c(4:85)] ) {
    s[i] <- i
  }

}
s2<-s[!is.na(s)]
length(s2)
index<-dif[s2,]


## make function for subsetting according to target range

dif3  <-index

sort<-function(pre_low, pre_up, post_low, post_up) {
  for(i in 1:821) {
    for(j in 4:85) {
      if ( is.na(index[i,j]) == TRUE ) {
        dif3[i,j] <- NA
      }
      else if  (index[i,j]  >= pre_low & index[i,j] <= pre_up)   {
        dif3[i,j]<-"PRE"
      }     
      else if  (index[i,j] >= post_low & index[i,j] <= post_up)    {
        dif3[i,j]<-"POST"
      }
      else{
        dif3[i,j]<-NA
      }
    }
  }
  count<-0
  result<-data.frame()
  result2<-data.frame()
  for(i in 1:821) {
    if (length(levels(as.factor(as.matrix(dif3[i,c(4:85)])))) == 2) {
      count<-count+1
      result<-rbind(result,sarco.date[dif3[i,1],])
      result2<-rbind(result2, dif[dif3[i,1],])
    }
  }  
  print(count)
  View(result)
  View(result2)
}
sort(-180,-90,6,8)

# I can not find "2010-9-15" and "2011-12-13" in "Date" column of given raw data. 
# So my final result is "17"
