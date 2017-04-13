### 2 Finding the best hospital in a state


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
#rank           7
#city name      2
#state          3

best <- function(state, out) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  
  outcome2<-outcome[,c(1,2,7,11,17,23)]
  outcome2<-outcome2[order(outcome2$Hospital.Name), ] 
  outcome_sub<-subset(outcome2, outcome2$State == state)
  out.list<-list()
  out.list[[1]]<-outcome_sub[,c(1,2,3,4)]
  out.list[[2]]<-outcome_sub[,c(1,2,3,5)]
  out.list[[3]]<-outcome_sub[,c(1,2,3,6)]
 
  aa<-lapply(out.list, function(x){
    rank<-rank(x[,4], ties.method = "first")
    x<-cbind(x,rank)
    result<-x[x$rank==1,2]
    })
  
  if ( (state %in% outcome2$State) == FALSE ) {
    stop("invaild state")
  }
  
  if (out =="heart attack") {
    print(aa[[1]])
  }
  else if (out == "heart failure") {
    print(aa[[2]])
  }      
  else if (out == "pneumonia") {
    print(aa[[3]])
  }
  else {
    stop("invalid outcome")
  }


  
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")




### 3 Ranking hospitals by outcome in a state


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
#rank           7
#city name      2
#state          3


rankhospital <- function(state, out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcome2<-outcome[,c(1,2,7,11,17,23)]
  outcome2<-outcome2[order(outcome2$Hospital.Name), ] 
  outcome_sub<-subset(outcome2, outcome2$State == state)
  rmna1<-subset(outcome_sub, is.na(outcome_sub[,4])==FALSE)
  rmna2<-subset(outcome_sub, is.na(outcome_sub[,5])==FALSE)
  rmna3<-subset(outcome_sub, is.na(outcome_sub[,6])==FALSE)
  out.list<-list()
  out.list[[1]]<-rmna1[,c(1,2,3,4)]
  out.list[[2]]<-rmna2[,c(1,2,3,5)]
  out.list[[3]]<-rmna3[,c(1,2,3,6)]
  
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst") {
    if (out == "heart attack") {
          num <- nrow(rmna1)
    }
    else if (out == "hear failure") {
          num <- nrow(rmna2)
    }
    else if (out == "pneumonia") {
          num <- nrow(rmna3)
    }
  }
  
  if (num > nrow(outcome_sub)) {
    print(NA)
    stop("invalid number")
    
  }
  
  aa<-lapply(out.list, function(x){
    rank<-rank(x[,4], ties.method = "first")
    x<-cbind(x,rank)
    result<-x[x$rank==num,2]
  })  
  
  if (out =="heart attack") {
    print(aa[[1]])
  }
  else if (out == "heart failure") {
    print(aa[[2]])
  }      
  else if (out == "pneumonia") {
    print(aa[[3]])
  }
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)





###  4 Ranking hospitals in all states


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6
#rank           7
#city name      2
#state          3

rankall <- function(out, n ) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  outcome2<-outcome[,c(1,2,7,11,17,23)]
  outcome2<-outcome2[order(outcome2$Hospital.Name), ] 
  fa<-as.factor(outcome2$State)
  num<-as.numeric(fa) 
  fa2<-attr(fa,"levels")
  outcome3<-outcome2
  outcome3$State<-num
  r.host<-vector()
  r.state<-vector()
  

  #out.list<-list()
  #out.list[[1]]<-outcome3[,c(1,2,3,4)]
  #out.list[[2]]<-outcome3[,c(1,2,3,5)]
  #out.list[[3]]<-outcome3[,c(1,2,3,6)]
  
  out.list<-list()
  out.list[[1]]<-outcome2[,c(1,2,3,4)]
  out.list[[2]]<-outcome2[,c(1,2,3,5)]
  out.list[[3]]<-outcome2[,c(1,2,3,6)]
  
  #3#if ( out == "heart attack") {
  #  state.sub<-subset(outcome4, outcome4$State == i)
  #  sub.out<-subset(state.sub, is.na(state.sub[,4]) == FALSE )
  #  rank<-rank(sub.out[,4], ties.method = "first" )
  #  sub.out<-cbind(sub.out, rank)
  #  if( n > nrow(sub.out) ) {
  #    r.host[i] <- NA
  #    r.state[i] <- fa2[i] 
  #  }
  #  else {
  #    r.host[i]  <-  sub.out[sub.out$rank==n,2] 
  #    r.state[i] <- fa2[i]
  #  }
  #}

  aa<-lapply(out.list, function(x){
    for (i in fa2) {
      state.sub<-subset(x, x$State == i)
      rmna<-subset(state.sub, is.na(state.sub[,4]) == FALSE )
      rank<-rank(rmna[,4], ties.method = "first")
      rmna<-cbind(rmna,rank)
   
      if ( n > nrow(rmna)) {
        r.host[i] <- NA
        r.state[i] <- i
      }
      else if (n <= nrow(rmna)) {
        r.host[i] <- rmna[rmna$rank == n, 2]
        r.state[i] <- i
      }
    }
    result<-as.data.frame(cbind(r.host,r.state))
  })
  
  #aa<-lapply(out.list, function(x){
   # for (i in 1:54) {
    #  state.sub<-subset(x, x$State == i)
     # sub.out<-subset(state.sub, is.na(state.sub[,4]) == FALSE )
    #  rank<-rank(sub.out[,4], ties.method = "first")
    #  sub.out<-cbind(sub.out,rank)
      
    #  if ( n > nrow(sub.out)) {
    #    r.host[i] <- NA
    #    r.state[i] < fa2[i]
    #  }
    #  else {
    #    r.host[i] <- sub.out[sub.out$rank == n, 2]
    #    r.state[i] < fa2[i]
    #  }
    
#    final.result<-as.data.frame(cbind(r.host,r.state))
#      }
#  })
    
  
  if (out =="heart attack") {
      print(aa[[1]])
  }
  else if (out == "heart failure") {
      print(aa[[2]])
  }      
  else if (out == "pneumonia") {
      print(aa[[3]])
  }
}
    
 
  
  
  
bb<-rankall("heart attack", 20)
  
cc<-tail(rankall("pneumonia", 50))
  
dd<-tail(rankall("heart failure",10))
