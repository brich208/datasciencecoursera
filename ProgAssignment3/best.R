best <- function(state, outcome) {
  
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  all_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% all_outcomes)) {
    stop("invalid outcome")
  }
  
  oc <- if(outcome=="heart attack") {
    11
  }
  else if(outcome=="heart failure") {
    17
  }
  else {
    23
  }
  
  all_states <- unique(x[,"State"])
  if(!(state %in% all_states)){
    stop("invalid state")
  }
 
  print(state)
  print(outcome)
  print(oc)
  
  x <- x[ , c(2, 7, oc)]
  names(x) <- c("hospital", "st", "outcome")
  x <- subset(x, st==state)
  x <- na.omit(x)  
  
  #test <- subset(x, state==st)
  #test
  
  x <- x[order(x$st, x$outcome, x$hospital) , ]
  
  #x <- x[1, ]
  
  #x <- x[, "hospital"]
  
  x
  
}