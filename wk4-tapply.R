### 2 Finding the best hospital in a state

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])  #heart attack   4
outcome[, 17] <- as.numeric(outcome[, 17])  #heart failure  5
outcome[, 23] <- as.numeric(outcome[, 23])  #pneumonia      6

out.ha<-outcome[,c(1,2,7,11)]
out.hf<-outcome[,c(1,2,7,17)]
out.pn<-outcome[,c(1,2,7,23)]

out.ha<-out.ha[order(out.ha[,4]),]
out.hf<-out.hf[order(out.hf[,4]),]
out.pn<-out.pn[order(out.pn[,4]),]


best <- function(state, out) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  
  if ( (state %in% outcome$State) == FALSE ) {
    stop("invaild state")
  }
  #################################
  if (out =="heart attack") {
    aa<-tapply(out.ha[,2], out.ha$State, function(x){
      x[1]  
    })
    print(aa[state])
  }  
  else if (out == "heart failure") {
    aa<-tapply(out.hf[,2], out.hf$State, function(x){
      x[1]  
    })
    print(aa[state])
  }      
  else if (out == "pneumonia") {
    aa<-tapply(out.pn[,2], out.pn$State, function(x){
      x[1]  
    })
    print(aa[state])
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

out.ha<-outcome[,c(1,2,7,11)]
out.hf<-outcome[,c(1,2,7,17)]
out.pn<-outcome[,c(1,2,7,23)]

out.ha<-subset(out.ha, is.na(out.ha[,4])==FALSE )
out.hf<-subset(out.hf, is.na(out.hf[,4])==FALSE )
out.pn<-subset(out.pn, is.na(out.pn[,4])==FALSE )

out.ha<-out.ha[order(out.ha[,4], out.ha[,2]), ]
out.hf<-out.hf[order(out.hf[,4], out.hf[,2]), ]
out.pn<-out.pn[order(out.pn[,4], out.pn[,2]), ]


rankhospital <- function(state, out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst"){
    num <- num
  }
  else if (num > nrow(outcome)) {
    print(NA)
    stop("invalid number")
  }
  ##########################################
  if (out =="heart attack") {
    aa<-tapply(out.ha[,2], out.ha$State, function(x){
      if ( num == "worst" ){
        num <- length(x)
      }
      x[num]  
    })
    print(as.vector(aa[state]))
  }  
  else if (out == "heart failure") {
    aa<-tapply(out.hf[,2], out.hf$State, function(x){
      if ( num == "worst" ){
        num <- length(x)
      }
      x[num]  
    })
    print(as.vector(aa[state]))
  }      
  else if (out == "pneumonia") {
    aa<-tapply(out.pn[,2], out.pn$State, function(x){
      if ( num == "worst" ){
        num <- length(x)
      }
      x[num]  
    })
    print(as.vector(aa[state]))
  }
  else {
    stop("invalid outcome")
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

out.ha<-outcome[,c(1,2,7,11)]
out.hf<-outcome[,c(1,2,7,17)]
out.pn<-outcome[,c(1,2,7,23)]

out.ha<-subset(out.ha, is.na(out.ha[,4])==FALSE )
out.hf<-subset(out.hf, is.na(out.hf[,4])==FALSE )
out.pn<-subset(out.pn, is.na(out.pn[,4])==FALSE )

out.ha<-out.ha[order(out.ha[,4], out.ha[,2]), ]
out.hf<-out.hf[order(out.hf[,4], out.hf[,2]), ]
out.pn<-out.pn[order(out.pn[,4], out.pn[,2]), ]


rankall <- function(out, n=1 ) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  if (n == "best") {
    n <- 1
  }
  else if (n == "worst"){
    n <- n
  }
  else if (n > nrow(outcome)) {
    print(NA)
    stop("invalid number")
  }
  ##################################################
  if (out =="heart attack") {
    aa<-tapply(out.ha[,2], out.ha$State, function(x){
      if ( n == "worst" ){
        n <- length(x)
      }
      x[n]  
    })
    print(cbind(as.data.frame(aa), names(aa)))
  }   
  else if (out =="heart failure") {
    aa<-tapply(out.hf[,2], out.hf$State, function(x){
      if ( n == "worst" ){
        n <- length(x)
      }
      x[n]  
    })
    print(cbind(as.data.frame(aa), names(aa)))
  } 
  else if (out =="pneumonia") {
    aa<-tapply(out.pn[,2], out.pn$State, function(x){
      if ( n == "worst" ){
        n <- length(x)
      }
      x[n]  
    })
    print(cbind(as.data.frame(aa), names(aa)))
  }  
  
   
}
rankall("heart attack", 20)
rankall("pneumonia", "worst")
rankall("heart failure")
