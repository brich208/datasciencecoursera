rankhospital <- function(state, outcome, num = "best") {
  
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
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
  
  ##check that the state input is a valid input
  
  all_states <- unique(x[,"State"])
  if(!(state %in% all_states)){
    stop("invalid state")
  }
  
  st <- state
  
  x <- x[ , c(2, 7, oc)]
  x <- na.omit(x)
  names(x) <- c("hospital", "state", "outcome")
  x <- subset(x, state==st)
  x <- x[order(x$state, x$outcome, x$hospital) , ]
  
  
  if(num=="best") {x <- x[1, ]}
  else if(num=="worst") {x <- tail(x, n =1)}
  else{x <- x[num, ]}
  
  x <- x[, "hospital"]
  
  x
}