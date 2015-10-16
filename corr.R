corr <- function (directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  files_num <- length(files)
  data <- data.frame()
  x <- numeric()
  y <- numeric()
  
  for(i in 1:files_num) {
    #create data frame for i instance
    data <- read.csv(files[i])
    
    #create data frame of just complete cases
    com_cases <- complete.cases(data)
    data_clean <- data[com_cases, ] 
    
    #count the number of rows in the complete data frame
    count <- nrow(data_clean)
    
    #if the no. of complete cases is more than threshold compute cor() on file
    if(count > threshold) {
      x <- cor(data[, "sulfate"], data[, "nitrate"], use = "complete.obs")
      
      #append the result to the cor() vector
      y <- append(y, x)
    }
  }
  print(y)
}