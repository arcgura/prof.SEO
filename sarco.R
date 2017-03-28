rm(list=ls())

## read data without exclusion code

codebook <- read.csv("~/R/codebook.csv",stringsAsFactors = F)
excode<-codebook$code_for_exclusion

sarcopenia <- read.table("~/R/Sarcopenia.csv", header=TRUE, sep=",", comment.char="", na.strings=c(excode,"NA","","."), stringsAsFactors=F)

## reset number
sarcopenia[,1]<-c(1:1547)

## divide date column from main data and conversion to date class
## divide code column from main data

sarco.code<-sarcopenia[c(1,2,11)]
sarco.date<-sarcopenia[c(1,2,11)]

for(i in 12:175 ) {
      if (i%%2 == 0) {
            sarcopenia[[i]]<-as.Date(sarcopenia[[i]], "%d/%m/%Y")
            sarco.date<-cbind(sarco.date,sarcopenia[i])
      }
      else {
            sarco.code<-cbind(sarco.code,sarcopenia[i])
      }
}

## calculate the number of CT
for(i in 1:nrow(sarco.code) ) {
      code.row<-sarco.code[i,]
      code.row.n<-sum(!is.na(code.row[,c(4:85)]))
      sarco.code[i,3]<-code.row.n
}

CT.number<-sum(sarco.code[,3])
print(CT.number)

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
