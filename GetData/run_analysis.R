run_analysis <- function() {
  setwd("C:/Users/518617/datasciencecoursera/GetData")
  x_test <- read.table("./UCIHARDataset/test/X_test.txt")
  y_test <- read.table("./UCIHARDataset/test/y_test.txt")
  subject_test <- read.table("./UCIHARDataset/test/subject_test.txt")
  
  feat <- read.table("./UCIHARDataset/features.txt")
  feat_names <- as.vector(feat[, 2])
  names(x_test) <- feat_names  
  
  test <<- data.table(subject_test, y_test, x_test)
}