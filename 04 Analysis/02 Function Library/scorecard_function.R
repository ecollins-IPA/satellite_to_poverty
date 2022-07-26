# This function inputs the results of the elastic net
# An input matrix with the selected indicators
# And outputs the scorecard as well the scores for every household

scorecard_function <- function(data, y, input_first_col = candidate_feature_first_col, 
                               input_last_col = candidate_feature_last_col,
                               coef_vector, stable_coef_names, question_key) {
  
    input_matrix <-  as.matrix(data[, input_first_col:input_last_col])
    stable_input_matrix <- input_matrix[, stable_coef_names, drop = F]
    scorecard_coef <- coef_vector
      
    scorecard_coefficients <- scorecard_coef #Store coefficients again
    max_scorecard_coefficient <- max(scorecard_coefficients) # Select the min absolute score. Since they are all negative, this is a max.
    # Substract the max coefficient (minimum in absolute terms) from all the coefficients. The max will be set to zero
    # and all others will be negative (this is the shifting)
    scorecard_coeff <- scorecard_coefficients - max_scorecard_coefficient
    score_indicators <- data.frame(stable_coef_names, scorecard_coeff) # Combine the adjusted coeffs with their names
    names(score_indicators)[1] <- "variable_name" #Call this the variable name
    score_indicators_q <- merge(question_key, score_indicators, by = "variable_name") #Merge with question key   
    # We want to ensure that the sum of the maximum possible responses to every question are equal to 100 
    score_indicators_q$max_score <- as.numeric(with(score_indicators_q, ave(scorecard_coeff, 
                                    unique_q_number, FUN = min)))  # Get the maximum score/coeff for every question
    #Store only one variable per question with the highest score/coefficient
    select_score <- score_indicators_q[score_indicators_q$max_score == score_indicators_q$scorecard_coeff, ]
    maximum_score <- sum(select_score[, 4]) #Store the maximum possible score
    scorecard_coef_std <- scorecard_coeff * 100 / maximum_score # Rescaling: Multiply by 100 and divide by the maximum score
    scorecard_coef_std <- round(scorecard_coef_std) # Round: Multiply by 100 and divide by the maximum score
    
    # This is the  scorecard: has two columns: the coefficient names, and the associated scaled and shifted scores
    scorecard <- data.frame(stable_coef_names, scorecard_coef_std)
    
    # Now we calculate the score for every observation (row) in the input matrix
    score <- stable_input_matrix %*% scorecard_coef_std
    score<- replace(score, score>100, 100)
    list(scorecard_indicator_weights = scorecard, score = score)
  }

