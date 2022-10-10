# Function inputs a dataframe
# Predicts the elastic net on a random sample and generates confidence intervals

bootstrap_test <- function(data_boot_test, y, bootstrap_sample_size, bootstrap_reps, score) {
  error_tables <- matrix(NA,  1, 10)
  
  for (s in bootstrap_sample_size) { #Loop over the two sample sizes we care about (usually 100 and 500)
    row_df <- seq(1:nrow(data_boot_test)) #Number of rows in the data-set
    error_matrix <- matrix(NA,  bootstrap_reps, 38) #NA matrix to store errors
    set.seed(1234)
    
    for (i in 1:bootstrap_reps) { #Bootstrap n times
      sample_rows <- sample(x = row_df, size = s , replace = F) #randomly select the rows that will be used in this iteration
      #Put together scores (1-100) with target variable (usually if they were poor at a particular poverty line)
      show <- data.frame(cbind(data_boot_test[sample_rows,][[score]], data_boot_test[sample_rows,][[y]]))
      values <- seq(5, 95, by = 5) #These are the values that we care about (5-95)
      results <- matrix(c(0, 0), nrow = 1)
      
      for (j in values) { #Loop over the values we specified (usually from 5 to 95 from 5 unit intervals)
        prob <- j
        ###################Exclusion error#############################################
        # Poor households (show[, 2] == 1) that are labelled non-poor  
        # i.e. have the score/prob less than the cutoff (show[,1]< prob)
        show$exclusion <- ifelse(show[, 1] >= prob & show[, 2] == 1, 1, 0) #excluded hh
        # Proportion of excluded hhs divided by the total proportion of poor households
        show$excl_error <- sum(show$exclusion,na.rm=T)/sum(show[, 2],na.rm=T)
        exclusion_results <- as.matrix(mean(show$excl_error,na.rm=T)) #save exclusion error for this iteration
        
        ###################Inclusion error#############################################
        # Non-Poor households (show[, 2] == 0) that are labelled poor (score prob is higher than cutoff)
        show$inclusion <- ifelse(show[, 1] < prob & show[, 2] == 0, 1, 0)
        # Proportion of included non-poor hhs divided by the total proportion of non-poor households
        show$incl_error <- sum(show$inclusion,na.rm=T)/sum(1 - show[, 2],na.rm=T)
        inclusion_results <- as.matrix(mean(show$incl_error,na.rm=T )) #save inclusion error for this iteration
        target_errors <- c(exclusion_results, inclusion_results) #Store inclusion and exclusion errors together
        
        results <- as.matrix(rbind(results, target_errors))
        
      }
      results <- results[-1,]
      error_matrix[i,] <- results
      
    }
    
    quantiles <- c(0.025, 0.05, 0.5, 0.95, 0.975)
    q <- as.matrix(colQuantiles(error_matrix, probs = quantiles,na.rm = T))
    error_table <- cbind(q[1:19, , drop = F], q[20:38, , drop = F])
    error_tables <- rbind(error_tables, error_table)
  }
  error_tables <- error_tables[-1, , drop = F]
  error_tables <- cbind(seq(5,95, by=5),error_tables)
  error_tables
}