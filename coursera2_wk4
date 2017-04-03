### 2 Finding the best hospital in a state
rm(list=ls())

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
                                            #rank           7
                                            #city name      2
                                            #state          3

outcome2<-outcome[,c(1,2,7,11,17,23)]




best <- function(state, out) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
      outcome_sub<-subset(outcome2, outcome$State == state)
      
      if (out =="heart attack") {
             rmna<-subset(outcome_sub, is.na(outcome_sub[,4])==FALSE)
             rank<-rank(rmna[,4], ties.method = "last")
             rmna<-cbind(rmna,rank)
      }
      else if (out == "heart failure") {
            rmna<-subset(outcome_sub, is.na(outcome_sub[,5])==FALSE)
            rank<-rank(rmna[,5], ties.method = "last")
            rmna<-cbind(rmna,rank)
      }      
      else if (out == "pneumonia") {
            rmna<-subset(outcome_sub, is.na(outcome_sub[,6])==FALSE)
            rank<-rank(rmna[,6], ties.method = "last")
            rmna<-cbind(rmna,rank)
      }
      
      result<-rmna[rmna$rank==1,2]
      print(result)
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")








### 3 Ranking hospitals by outcome in a state
rm(list=ls())

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
#rank           7
#city name      2
#state          3

outcome2<-outcome[,c(1,2,7,11,17,23)]


rankhospital <- function(state, out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
      outcome_sub<-subset(outcome2, outcome$State == state)
      
      if (out =="heart attack") {
            rmna<-subset(outcome_sub, is.na(outcome_sub[,4])==FALSE)
            rank<-rank(rmna[,4], ties.method = "last")
            rmna<-cbind(rmna,rank)
            
      }
      else if (out == "heart failure") {
            rmna<-subset(outcome_sub, is.na(outcome_sub[,5])==FALSE)
            rank<-rank(rmna[,5], ties.method = "last")
            rmna<-cbind(rmna,rank)
            
      }      
      else if (out == "pneumonia") {
            rmna<-subset(outcome_sub, is.na(outcome_sub[,6])==FALSE)
            rank<-rank(rmna[,6], ties.method = "last")
            rmna<-cbind(rmna,rank)
            
      }      
    
      if (num == "best") {
            num <- 1
      }
      else if (num == "worst") {
            num <- nrow(rmna)
      }  

      if (num <= nrow(rmna)) {
            result<-rmna[rmna$rank==num,2]
            print(result)
      }
      else {
            print("NA")
      }
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)



###  4 Ranking hospitals in all states

rm(list=ls())

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
#rank           7
#city name      2
#state          3

outcome2<-outcome[,c(1,2,7,11,17,23)]





rankall <- function(out, n ) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
     outcome3<-outcome2[order(outcome2$Hospital.Name), ]
      fa<-as.factor(outcome3$State)
      num<-as.numeric(fa)
      fa2<-attr(fa,"levels")
      outcome4<-outcome3
      outcome4$State<-num
      r.host<-vector()
      r.state<-vector()
      
    for (i in 1:54) {
       state.sub<-subset(outcome4, outcome4$State == i)
        
      
      
      if ( out == "heart attack") {
        sub.out<-subset(state.sub, is.na(state.sub[,4]) == FALSE )
        rank<-rank(sub.out[,4], ties.method = "first" )
        sub.out<-cbind(sub.out, rank)
          if( n > nrow(sub.out) ) {
               r.host[i] <- NA
               r.state[i] <- fa2[i] 
          }
          else {
              r.host[i]  <-  sub.out[sub.out$rank==n,2] 
              r.state[i] <- fa2[i]
          }
      }
               #############
       if ( out == "heart failure") {
         sub.out<-subset(state.sub, is.na(state.sub[,5]) == FALSE )
         rank<-rank(sub.out[,5], ties.method = "first" )
         sub.out<-cbind(sub.out, rank)
         if( n > nrow(sub.out) ) {
           r.host[i] <- NA
           r.state[i] <- fa2[i] 
         }
         else {
           r.host[i]  <-  sub.out[sub.out$rank==n,2] 
           r.state[i] <- fa2[i]
         }
       }
              ##############
       if ( out == "pneumonia") {
         sub.out<-subset(state.sub, is.na(state.sub[,6]) == FALSE )
         rank<-rank(sub.out[,6], ties.method = "first" )
         sub.out<-cbind(sub.out, rank)
         if( n > nrow(sub.out) ) {
           r.host[i] <- NA
           r.state[i] <- fa2[i] 
         }
         else {
           r.host[i]  <-  sub.out[sub.out$rank==n,2] 
           r.state[i] <- fa2[i]
         }
       }
       
       

    }
      final.result<-cbind(r.host,r.state)
      print(final.result)          
}      
    
 
aa<-rankall("heart attack", 20)

bb<-rankall("pneumonia", "worst")

cc<-rankall("heart failure",1)



