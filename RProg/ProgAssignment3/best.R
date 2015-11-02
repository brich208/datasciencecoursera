best <- function(state, outcome) {
  
  ##read the data into x, marking "Not Available" as NA. stringsAsFactors = FALSE
  ##fixes errors with leading 0's
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
  ##check that the state input is a valid input
  all_states <- unique(x[,"State"])
  if(!(state %in% all_states)){
    stop("invalid state")
  }
  
  ##check that the outcome input is a valid input
  all_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% all_outcomes)) {
    stop("invalid outcome")
  }
  
  ##determine which column the outcome is found in
  oc <- if(outcome=="heart attack") {
    11
  }
  else if(outcome=="heart failure") {
    17
  }
  else {
    23
  }
  
  ##pull the hospital name, state, and outcome (from input) columns, remove the NAs,
  ##rename the columns, subset the data for the state input, order the list and pull
  ##only the first row hospital name
  
  x <- na.omit(x[ , c(2, 7, oc)])
  names(x) <- c("hospital", "state", "outcome")
  x <- x[order(x$state, x$outcome, x$hospital) , ]
  x <- x[x$state==state,"hospital"][1]
  
  x
  
}