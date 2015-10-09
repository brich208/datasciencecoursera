complete <- function(directory, id = 1:332) {
  #create files list and black data frame and vectors
  files <- list.files("specdata", full.names = TRUE)
  data <- data.frame()
  id_vec<- numeric()
  nobs_vec <- numeric()
  
  for (i in id) {
	#read a specified CSV into file
    data <- read.csv(files[i])
    
    #create a data frame of only pollutant data
    data_s <- data[, "sulfate"]
    data_n <- data[, "nitrate"]
    data_all <- data.frame(data_s, data_n) 
      
    #analyze frame to fine pairs of observations  
    analysis_frame <- data.frame(data_all, bothObs = (data_all$data_s > data_all$data_n))
    
    #count the number of results that have complete observations
    count_vec <- analysis_frame[, "bothObs"]
    count <- sum(length(which(!is.na(count_vec))))
    
    #add the current record ID and its count to the vectors
    id_vec <- append(id_vec, i)
    nobs_vec <- append(nobs_vec, count)
    
    #clean NAs from vectors
    id_clean <- id_vec[which(!is.na(id_vec))]
   	nobs_clean <- nobs_vec[which(!is.na(nobs_vec))]
  	
  	}
 
 	#create output data frame from clean vectors and name columns
  	output <- data.frame(id_clean, nobs_clean)
  	colnames(output) <- c("id", "nobs")
  	output

}