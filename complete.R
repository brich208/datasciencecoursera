complete <- function(directory, id = 1:332) {
  #create files list and black data frame and vectors
  files <- list.files("specdata", full.names = TRUE)
  data <- data.frame()
  id_vec<- numeric()
  nobs_vec <- numeric()
  
  for (i in id) {
	
	#read a specified CSV into file
    data <- read.csv(files[i])
    
    #create a data frame of only complete cases
    com_cases <- complete.cases(data)
    data <- data[com_cases, ] 
      
    #count the number of rows in the complete data frame
    count <- nrow(data)
    
    #add the current record ID and its count to the vectors
    id_vec <- append(id_vec, i)
    nobs_vec <- append(nobs_vec, count)
  	
  	}
 
 	#create output data frame from clean vectors and name columns
  	output <- data.frame(id_vec, nobs_vec)
  	colnames(output) <- c("id", "nobs")
  	output

}