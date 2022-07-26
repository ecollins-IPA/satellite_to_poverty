# This function is used inside the bootstrap_variable_select function
# It inputs a dataframe which is a random bootstrap sample
# Estimates an elastic net logit and then outputs a coefficient vector

coefficients_elastic_net <- function(data_var_select, y, weight,
           input_first_col = candidate_feature_first_col, 
           input_last_col = candidate_feature_last_col,
           alpha = alpha, penalty_vector = penalty_vector_full, 
           nfold = cv_nfold, fam = "binomial") {

   input_matrix <- as.matrix(data_var_select[, input_first_col:input_last_col]) # Data matrix with only candidate variables
   target_vector <- data_var_select[[target_base]] # Specify the dependent variable (usually the national poverty line) with which the model will be trained
   weight <- data_var_select[[weight]] # Specify the weights to be used in the model (usually household weights)
   foldid <- sample(rep(seq(nfold)), size = length(target_vector),  replace = TRUE) #Gens a vector of length = subsample used in this analysis with random allocated values from 1-10
   
   cvfit <-  cv.glmnet(input_matrix, target_vector, family = fam ,  weight = weight,
              nfold = nfold, foldid = foldid, alpha = alpha, penalty.factor = penalty_vector,
              type.measure = "deviance", dfmax = numb_questions + region_number - 1)
    
   coef <- as.matrix(coef(cvfit, s = "lambda.min")) #Take the coefficients of the lambda that minimizes 
   as.vector(coef[-1])#We don't need the intercept: this is only to choose among the feature/explanatory variables
}
