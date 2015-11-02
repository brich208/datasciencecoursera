pollutantmean <- function(directory, pollutant, id = 1:332) {
  #create full path file list
  files <- list.files(directory, full.names = TRUE)
  
  #create blank data frame
  data <- data.frame()
  
  #bind all data into 'data'
  for(i in id) {
    data <- rbind(data, read.csv(files[i]))
  }
  
  #pull all data for selected pollutant
  data_pol <- data[, pollutant]
  
  #find mean while removing NAs
  mean(data_pol, na.rm = TRUE)
}