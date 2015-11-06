name_test <- function() {
  count <- nrow(full_act)
  for(i in 1:count) {
    num <- full_act[i, 1]
    print(num)
    name <- as.vector(full_act[i, 2])
    print(name)
    
    clean_names <- sub(num, name, as.character(test_y)))
  }
  clean_names
}