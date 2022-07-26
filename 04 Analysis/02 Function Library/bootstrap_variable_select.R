# Function inputs a dataframe
# Estimates an elastic net logit and outputs a coefficient vector
mark_time <- function(msg="It's currently", formatstring="%a %b %d %X %Y", filename=FALSE, verbose=TRUE){
      msg <- sprintf("%s %s", msg, format(Sys.time(), formatstring))
      if (verbose) print(msg)
      if (is.character(filename)) write(msg, file=filename, append=TRUE)
    }

bootstrap_variable_select <-function(data_boot, bootstrap_sample_fraction,
           bootstrap_reps, question_key, y, numb_questions, alpha, weight,
           penalty_vector= penalty_vector_full, fam = "binomial", iter=100) {
    
    # Number of coefficients (used to create an empty coefficient matrix) 
    numb_coefficients <- (candidate_feature_last_col - candidate_feature_first_col + 1)
    # Empty coefficient matrix that is populated by the bootstrap sample elastic net results (bootstrap reps X coef N)
    coeff_matrix <- matrix(NA,  bootstrap_reps, numb_coefficients)
    # Get a row index used to create a random bootstrap sample without replacement
    row_index <- seq(1:nrow(data_boot))
    
    #Bootstrap glmnet and stores the coefficients of the lambda that minimizes the cross validation error
    for (i in 1:bootstrap_reps) {
      sample_rows <- sample(x = row_index, size = round(nrow(data_boot)*bootstrap_sample_fraction),
               replace = F) # Chooses a random subsample of the of the trainning set. Size = (bootstrap_sample_fraction)*(# of obs in train set)
      # Populates the row of the coefficient matrix to get the coefficients for each iteration
      if (iter & !(i%%iter) )  mark_time(paste("Iteration", i, ":"))
      coeff_matrix[i, ] <- coefficients_elastic_net(data_var_select = data_boot[sample_rows, ], 
               y, alpha = alpha, weight = weight_name, penalty_vector= penalty_vector_full, fam = fam)
    }
     coeff_matrix2 <- coeff_matrix
     coeff_matrix[coeff_matrix!=0]<- 1 # Set the non-zero coefficients to one
     f_coeff <- colMeans(coeff_matrix) # Frequency of Non-Zero Estimate per variable
     frequency_df <- data.frame(cbind(f_coeff,question_key[,qkey_num])) # Merge with question number 
             frequency_df2 <- data.frame(cbind(f_coeff,question_key))
     colnames(frequency_df)<- c("frequency", "unique_q_number") #Set names
             colnames(frequency_df2)<- c("frequency", "variable_name","unique_q_number") #Set names
     frequency_df$max_freq <- with(frequency_df,ave(frequency_df[,1], 
                                   frequency_df[,2],FUN= max)) # Add the max freq for each question
             frequency_df2$max_freq <- with(frequency_df,ave(frequency_df[,1], 
                                                            frequency_df[,2],FUN= max)) # Add the max freq for each question
                    #Store the frequencies for each variable + the avg coefficient and export it into a CSV file
                    mean_coeff <- colMeans(coeff_matrix2)
                    mean_coeff <- data.frame(cbind(mean_coeff,frequency_df2))
                          colnames(mean_coeff)[1] <- c("Mean_coeff")
                    write.csv(mean_coeff, paste(clean, "Freq_selected_variables.csv"))
                    
     #Store the variables with the max freqs only 
     select_question<- frequency_df[frequency_df$max_freq == frequency_df[,1],]
     #Make sure they were not duplicated
     select_question = select_question[!duplicated(select_question$unique_q_number),]
     #The questions with the highest frequency == 1  and else == 0. This is subject to the desired # of questions
     select_question$selector_indicator<-  ifelse(select_question$max_freq < min(tail(sort(select_question$max_freq),
                                                  numb_questions)),0,1)
     #Merge selected questions with qkey
     merged_df <- merge(question_key,select_question,by ="unique_q_number")
     #Keep if the indicator was among those with the highest freqs
     stable_merged_df<- merged_df[which(merged_df$selector_indicator != 0), , drop =F]
     stable_coef_names <- stable_merged_df[, 2]
    
  # The function returns stable_coef_names
  return(stable_coef_names)
}
