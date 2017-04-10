### 2 Finding the best hospital in a state
rm(list=ls())
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6


outcome2<-outcome[,c(1,2,7,11,17,23)]


outcome2<-tbl_df(outcome2)
names(outcome2)<-c("id","hospital","state","HA","HF","PN")
outcome3<-gather(outcome2, disease, rate, HA:PN, na.rm=TRUE)
outcome4<-arrange(outcome3, rate, hospital)
state.level<-levels(as.factor(outcome4$state))



best <- function(st, out) {
  if ( (st %in% state.level ) == FALSE ) {  stop("invaild state") }
  ######
  if (out =="heart attack") {  out<-"HA"  }
  else if (out == "heart failure") { out<-"HF"}
  else if (out == "pneumonia")  {out<-"PN"}
  else { stop("invalid outcome")}
  ######
  aa<-filter(outcome4, state==st, disease==out)
  print(as.data.frame(aa[1,2]))

}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")  



### 3 Ranking hospitals by outcome in a state
rm(list=ls())

library(dplyr)
library(tidyr)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6


outcome2<-outcome[,c(1,2,7,11,17,23)]


outcome2<-tbl_df(outcome2)
names(outcome2)<-c("id","hospital","state","HA","HF","PN")
outcome3<-gather(outcome2, disease, rate, HA:PN, na.rm=TRUE)
outcome4<-arrange(outcome3, rate, hospital)
state.level<-levels(as.factor(outcome4$state))

rankhospital <- function(st, out, num = "best") {
  if ( (st %in% state.level ) == FALSE ) {  stop("invaild state") }
  ######
  if (num == "best") {num <- 1}
  else if (num == "worst")  {num <- num }
  else if (num > nrow(outcome)) {print(NA) ;stop("invalid number")}
  ######
  if (out =="heart attack") {  out<-"HA"  }
  else if (out == "heart failure") { out<-"HF"}
  else if (out == "pneumonia")  {out<-"PN"}
  else { stop("invalid outcome") }
  ######
  aa<-filter(outcome4, state==st, disease==out)
  if (num == "worst") { num<-nrow(aa)}
  print(as.data.frame(aa[num,2]))
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)



###  4 Ranking hospitals in all states
rm(list=ls())


library(dplyr)
library(tidyr)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6


outcome2<-outcome[,c(1,2,7,11,17,23)]


outcome2<-tbl_df(outcome2)
names(outcome2)<-c("id","hospital","state","HA","HF","PN")
outcome3<-gather(outcome2, disease, rate, HA:PN, na.rm=TRUE)
outcome4<-arrange(outcome3, rate, hospital)
state.level<-levels(as.factor(outcome4$state))


rankall <- function(out, num=1 ) {
  if (num == "best") {num <- 1}
  else if (num == "worst")  {num <- num }
  else if (num > nrow(outcome4)) {print(NA) ;stop("invalid number")}
  ######
  if (out =="heart attack") {  out<-"HA"  }
  else if (out == "heart failure") { out<-"HF"}
  else if (out == "pneumonia")  {out<-"PN"}
  else { stop("invalid outcome") }
  ######
  aa<-filter(outcome4, disease==out)
  bb<-tapply(aa$hospital, aa$state, function(x){
    if ( num == "worst" )  {num <- length(x)}
    x[num]  
  })  
  print(cbind(as.data.frame(bb), names(bb)))
}

rankall("heart attack", 20)
rankall("pneumonia", "worst")
rankall("heart failure")
