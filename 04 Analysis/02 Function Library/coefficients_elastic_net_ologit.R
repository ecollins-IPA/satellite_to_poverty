# This function is used inside the bootstrap_variable_select function
# It inputs a dataframe which is a random bootstrap sample
# Estimates an elastic net logit and then outputs a coefficient vector

coefficients_elastic_net_ologit <- function(data_var_select, y, weight,
           input_first_col = candidate_feature_first_col, 
           input_last_col = candidate_feature_last_col,
           alpha = alpha, penalty_vector = penalty_vector_full, 
           nfold = cv_nfold, fam = "binomial") {

   input_matrix <- as.data.frame(data_var_select[, input_first_col:input_last_col]) # Data matrix with only candidate variables
   target_vector <- data_var_select[[target_base]] # Specify the dependent variable (usually the national poverty line) with which the model will be trained
   weight <- data_var_select[[weight]] # Specify the weights to be used in the model (usually household weights)
   foldid <- sample(rep(seq(nfold)), size = length(target_vector),  replace = TRUE) #Gens a vector of length = subsample used in this analysis with random allocated values from 1-10
   
   cvfit <- glmnetcr(input_matrix, target_vector, weights = weight, nlambda = 100, 
                     penalty.factor = penalty_vector, alpha = a, 
                     dfmax = numb_questions + region_number - 1,
                     trace.it = 0)
   
   best_fit_step = select.glmnetcr(cvfit, which = "BIC") ##We store the step that yielded the best fitting models
   coef<-coef(cvfit, s = best_fit_step)$beta #Take the coefficients of the lambda that minimizes
   as.vector(coef[1:(length(coef)-length(pov_lev))])
}
