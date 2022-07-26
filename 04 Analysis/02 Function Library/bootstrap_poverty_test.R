bootstrap_pov_test <-  function(data_boot_test = test_data, y , bootstrap_sample_size,
           bootstrap_reps, weight, score_probability) {
  
    error_tables <- matrix(NA,  1, 3) #Error table in blank (we use this as an output later)
    for (s in bootstrap_sample_size) {
      error_matrix <- matrix(NA, bootstrap_reps, 2)
      
      for (i in 1:bootstrap_reps) {
        row_df <- seq(1:nrow(data_boot_test)) #Rows for all the test set
        sample_rows <- sample(x = row_df, size = s , replace = F) #Randomly sample a set of rows from the test set
                  #pr_error <- (mean(data_boot_test[sample_rows,][[score_probability]] - data_boot_test[sample_rows,][[target]], na.rm=T))*100
        #Sample poverty rate error (test set subsamble estimated poverty rate - real poverty dummy)
        pr_error <- (weighted.mean(data_boot_test[sample_rows,][[score_probability]] - data_boot_test[sample_rows,][[y]], w= data_boot_test[sample_rows,][[weight]]))*100
        #Sample poverty rate benchmark (average poverty rate in train set - subsample of test set chosen with its real)
        pr_error_benchmark <- (mean(mean(train_data[[y]]) - data_boot_test[sample_rows,][[y]], na.rm=T))*100
        results<- cbind(pr_error, pr_error_benchmark)
        error_matrix[i,] <- results
        }
      mean_error_model <-  mean(error_matrix[,1], na.rm=T) #Mean error rate across all iterations
      demeaned_error_model <- error_matrix[,1] - mean_error_model #Demeaned error rates
      mean_error_random <-  mean(error_matrix[,2], na.rm=T) #Mean benchmark error rate across all iterations 
      demeaned_error_random <- error_matrix[,2] - mean_error_random #Demeaned benchmark error rates
      demeaned_error_matrix <- as.matrix(cbind(demeaned_error_model,demeaned_error_random)) #Both demeaned vectors
      quantiles <- c(0.025, 0.975) #Quantiles of interest (usually for the 5% significance level)
      errors <- as.matrix(colQuantiles(demeaned_error_matrix, probs = quantiles, na.rm = T)) #Mean demeaned error values for 
      error_table <- cbind(errors[1,,drop=F],mean_error_model)
      error_tables <- rbind(error_tables, error_table)
      }
    error_tables <- error_tables[-1, , drop = F]
  }
