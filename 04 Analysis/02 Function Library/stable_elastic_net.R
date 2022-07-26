# Function inputs a dataframe
# Estimates an elastic net logit and outputs a coefficient vector
# Does not penalize the subnational question
stable_elastic_net <- function(data, y, weight = weight_name,
           input_first_col = candidate_feature_first_col,
           input_last_col = candidate_feature_last_col, fam = "binomial",
           alpha = alpha, nfold = cv_nfold, stable_coef) {
  
    input_matrix <- data[, input_first_col:input_last_col] #subset data so only candidate variables are used
    input_matrix<- as.matrix(input_matrix) #Make it a matrix
        colnames(input_matrix) <- colnames(data[, input_first_col:input_last_col])
    stable_input_matrix <- input_matrix[, stable_coef] # subset data so only selected variables are used
    target_vector <- data[[y]] #store the y (dependent) variable
    weight <- data[[weight]] #store the weights
    repeat_cv <- 5 # Repeat 5 times the fold Cross-Validation
    lambda <- matrix(c(0), nrow = 1) #We'll store the lambdas that minimize our errors here 
    min_cv_error <- matrix(c(0), nrow = 1) #We'll store the minimum errors here
    
    for (i in 1:repeat_cv) { #We repeat this a set of times so that the result is not dependent on the randomnsess of the folds
      cvfit <- cv.glmnet( stable_input_matrix, target_vector, family = fam,
          weights = weight, nfold = nfold, alpha = alpha, type.measure = "deviance")
      
      one_min_cv_error<- cvfit$cvm[cvfit$lambda == cvfit$lambda.min]
      one_lambda <- cvfit$lambda.min
      lambda <- as.matrix(rbind(lambda, one_lambda))
      min_cv_error <- as.matrix(rbind(min_cv_error, one_min_cv_error))
    }
    
    # select the best one
    lambda <- lambda[-1]
    median_lambda <- median(lambda)
    min_cv_error <- min_cv_error[-1]
    median_min_cv_error <- median(min_cv_error)
    final_elastic_net <- glmnet(stable_input_matrix, target_vector,
             family = fam, alpha = alpha, weights = weight) #This does not do any n-fold cross validation
    
    return(list(final_elastic_net = final_elastic_net, median_lambda = median_lambda, min_cv_error = median_min_cv_error))
    
  }
