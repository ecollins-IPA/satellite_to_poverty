# Function inputs a dataframe
# Estimates an unpenalized logit using the training dataframe 
# Predicts the probabilities 

unpenalized_logit <-
  function(train_data,
           test_data,
           y = target,
           weight = weight_name,
           input_first_col = candidate_feature_first_col,
           input_last_col = candidate_feature_last_col
           ) 
    {
    input_matrix <- train_data[, input_first_col:input_last_col]
    input_matrix<- as.matrix(input_matrix)
    
    target_vector <- train_data[[y]]
    weight <- train_data[[weight]]
    
    logit_df <- data.frame(target_vector,input_matrix)
    
    logit_fit <-
      glm(target_vector~.,
        family=binomial,
        data = logit_df
      )
    
    fitted.results <- predict(logit_fit,newdata= data.frame(test_data[,input_first_col:input_last_col]),type='response')
    
    return(
      fitted.results
    )
  }