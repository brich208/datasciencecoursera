rankall <- function(outcome, num = "best") {
    
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
  
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
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
    x <- x[order(x$state, x$outcome, x$hospital) , ]
  }
  
  s <- split(x, x$state)
  s <- lapply(s, function(y) y[num, ])
  s <- do.call(rbind.data.frame, s)
  s <- s[c("hospital", "state")]
  
  s

}