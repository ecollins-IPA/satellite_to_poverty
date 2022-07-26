bootstrap_pov_level_test <-
  function(data_boot_test = test_data,
           y,
           bootstrap_sample_size,
           bootstrap_reps,
           score_probability) {
    error_tables <- matrix(NA,  1, 14)
for (s in bootstrap_sample_size) {
  error_matrix <- matrix(NA,  bootstrap_reps, 2)
  for (i in 1:bootstrap_reps) {
  row_df <- seq(1:nrow(data_boot_test))
  
sample_rows <- sample(x = row_df,
                      size = s ,
                      replace = F)
pr_error <- mean(data_boot_test[sample_rows,][[score_probability]], na.rm=T)*100
pr_error_benchmark <- mean(data_boot_test[sample_rows,][[y]], na.rm=T)*100

results<- cbind(pr_error, pr_error_benchmark)
error_matrix[i,] <- results
  }
  mean_error_model <-  mean(error_matrix[,1], na.rm=T)
  sd_error_model <-  sd(error_matrix[,1], na.rm=T)
  demeaned_error_model <- error_matrix[,1] - mean_error_model
  mean_error_random <-  mean(error_matrix[,2], na.rm=T)
  sd_error_random <-  sd(error_matrix[,2], na.rm=T)
  demeaned_error_random <- error_matrix[,2] - mean_error_random
  demeaned_error_matrix <- as.matrix(cbind(demeaned_error_model,demeaned_error_random))
  
  quantiles <- c(0.025, 0.05,  0.5, 0.95, 0.975)
  errors <- as.matrix(colQuantiles(demeaned_error_matrix, probs = quantiles, na.rm = T))
  error_table<- cbind(errors[1,,drop=F],mean_error_model,sd_error_model, errors[2,,drop=F], mean_error_random, sd_error_random)
  error_tables <- rbind(error_tables, error_table)
}
    error_tables <- error_tables[-1, , drop = F]
    
    
   
}
