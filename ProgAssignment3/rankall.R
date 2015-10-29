rankall <- function(outcome, num = "best") {
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
  
  if(!(num > 0)) {
    stop("invalid number")
  }
  
  ##check that the state input is a valid input
  #all_states <- unique(x[,"State"])
  #if(!(state %in% all_states)){
    #stop("invalid state")
  #}
  
  x <- x[ , c(2, 7, oc)]
  x <- na.omit(x)
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
  
  s <- split(x, x$state)
  s <- lapply(s, function(y) y[num, ])
  s <- do.call(rbind.data.frame, s)
  keep <- c("hospital", "state")
  s <- s[keep]
  
  s

}