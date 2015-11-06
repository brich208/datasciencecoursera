rankall <- function(outcome, num = "best") {
  
  ##check that outcome is valid input
  all_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% all_outcomes)) {
    stop("invalid outcome")
  }
  
  ##check for valid num input, if a # must be greater than 0 and only "best" and
  ##"worst" are accepted
  all_choices <- c("best", "worst")
  if(is.numeric(num)) {
    if(!(num > 0)) {stop("invalid number")}
  }
  if(is.character(num)) {
    if(!(num %in% all_choices)) {stop("invalid rank")}
  }
  
  ##assign oc (outcome column) based on outcome input
  oc <- if(outcome==all_outcomes[1]) {
    11
  }
  else if(outcome==all_outcomes[2]) {
    17
  }
  else {
    23
  }
  
  ##read in data, assign NA string, and Factors = FALSE to account for leading 0's
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
  ##pull only relevant columns and rename columns
  x <- na.omit(x[ , c(2, 7, oc)])
  names(x) <- c("hospital", "state", "outcome")
  
  ##order columns in reverse for "worst" and normally for "best" or a # input
  if(num=="worst") {
    num <- 1
    x <- x[order(x$state, -x$outcome, x$hospital) , ]
  }
  else{
    x <- x[order(x$state, x$outcome, x$hospital) , ]
    if(num=="best"){num <- 1}
  }
  
  ##split the ordered frame by state; lapply to pull the row from each state for
  ##the appropriate rank based on previous if/else block; do.call to rbind lapply
  ##result back into a data frame; pull only hospital and state columns
  s <- split(x, x$state)
  s <- lapply(s, function(y) y[num, ])
  s <- do.call(rbind.data.frame, s)
  s <- s[c("hospital", "state")]
  
  s 

}