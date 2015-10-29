best <- function(state, outcome) {
  
  ##read the data into x, marking "Not Available" as NA. stringsAsFactors = FALSE
  ##fixes errors with leading 0's
  
  x <- read.csv("outcome-of-care-measures.csv",  
                na.strings="Not Available", stringsAsFactors = FALSE)
  
  ##check that the outcome input is a valid input, if so determine which column
  ##the outcome is found in
  
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
  
  ##pull the hospital name, state, and outcome (from input) columns, remove the NAs,
  ##rename the columns, subset the data for the state input, order the list and pull
  ##only the first row hospital name
  
  x <- x[ , c(2, 7, oc)]
  x <- na.omit(x)
  names(x) <- c("hospital", "st", "outcome")
  x <- subset(x, st==state)
  x <- x[order(x$st, x$outcome, x$hospital) , ]
  
  x <- x[1, ]
  x <- x[, "hospital"]
  
  x
  
}