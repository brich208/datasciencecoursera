complete <- function(directory, id = 1:332) {
  files <- list.files("specdata", full.names = TRUE)
  data <- data.frame()
  id_vec <- numeric()
  nobs_vec <- numeric()
  
  for (i in id) {

    data <- read.csv(files[i])
    data_s <- data[, "sulfate"]
    data_n <- data[, "nitrate"]
    data_all <- data.frame(data_s, data_n) 
    analysis_frame <- data.frame(data_all, bothObs = (data_all$data_s > data_all$data_n))
    count_vec <- analysis_frame[, "bothObs"]
    count <- sum(length(which(!is.na(count_vec))))
    
    id_vec[i] <- i
    nobs_vec[i] <- count
  }
  
  id_clean <- id_vec[which(!is.na(id_vec))]
  nobs_clean <- nobs_vec[which(!is.na(nobs_vec))]
  
  output <- data.frame(id_clean, nobs_clean, stringsAsFactors = FALSE)
  colnames(output) <- c("id", "nobs")
  output

}