corr <- function (directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  files_num <- length(files)
  data <- data.frame()
  x <- numeric()
  y <- numeric()
  
  for(i in 1:files_num) {
    #create data frame for i instance
    data <- read.csv(files[i])
    
    #count the number of complete rows in the data frame
    count <- nrow(data[complete.cases(data), ])
    
    #if the no. of complete cases is more than threshold compute cor() on file
    if(count > threshold) {
      x <- cor(data[, "sulfate"], data[, "nitrate"], use = "complete.obs")
      
      #append the result to the cor() vector
      y <- append(y, x)
    }
  }
  print(y)
}