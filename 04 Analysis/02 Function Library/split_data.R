# Function that inputs the data and outputs the test and training data
# Arguments are i) data ii) proportion split
split_data <- function(data = survey_data, prop_trained = 0.66) {

  sample_size <- floor(prop_trained * nrow(data))   # We want 66% (2/3) of the data to be the sample size 
  train_index <- sample(seq_len(nrow(data)), size = sample_size)   # Extract a random sample from the survey data with a size == sample size (usually 2/3 of survey_data)
  train_data <- data[train_index, , drop = F]   # create train data dataframes
  test_data <- data[-train_index, , drop = F]   # create train test dataframes
  return(list(train_data = train_data, test_data = test_data))

}