rankall <- function(outcome, num = "best") {
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
  all_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% all_outcomes)) {
    stop("invalid outcome")
  }
  oc <- if(outcome==all_outcomes[1]) {
    11
  }
  else if(outcome==all_outcomes[2]) {
    17
  }
  else {
    23
  }
  
  if(!(num > 0)) {
    stop("invalid number")
  }
  
  ##check that the state input is a valid input
  #all_states <- unique(x[,"State"])
  #if(!(state %in% all_states)){
    #stop("invalid state")
  #}
  
  x <- na.omit(x[ , c(2, 7, oc)])
  names(x) <- c("hospital", "state", "outcome")
  
  if(num=="best") {
    num <- 1
    x <- x[order(x$state, x$outcome, x$hospital) , ]
  }
  else if(num=="worst") {
    num <- 1
    x <- x[order(x$state, -x$outcome, x$hospital) , ]
  }
  else{
    num <- num
    x <- x[order(x$state, x$outcome, x$hospital) , ]
  }
  
  s <- do.call(rbind.data.frame, 
               lapply(split(x, x$state), function(y) y[num, ]))
  s <- s[c("hospital", "state")]
  
  s

}