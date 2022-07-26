# Function inputs a dataframe
# Estimates an elastic net logit and outputs a coefficient vector
# Does not penalize the subnational question
stable_elastic_net_ologit <- function(data, y, weight = weight_name,
           input_first_col = candidate_feature_first_col,
           input_last_col = candidate_feature_last_col,
           alpha = alpha, nfold = cv_nfold, stable_coef) {
  
    input_matrix <- data[, input_first_col:input_last_col] #subset data so only candidate variables are used
    input_matrix<- as.matrix(input_matrix) #Make it a matrix
    stable_input_matrix <- input_matrix[, stable_coef] # subset data so only selected variables are used
    target_vector <- data[[y]] #store the y (dependent) variable
    weight <- data[[weight]] #store the weights
    
    alpha_seq = seq(from=0.05, to=1, by=0.025)
    AIC_BIC = matrix(data=NA, nrow=length(alpha_seq), ncol=3) #NA matrix to store all BIC and AIC
    AIC_BIC[,1] = alpha_seq
    colnames(AIC_BIC) = c("Alpha", "AIC", "BIC")
    
    for (i in 1:length(alpha_seq)) {
    cvfit <- glmnetcr(stable_input_matrix, target_vector, weights = weight, nlambda = 100, 
                      alpha = alpha_seq[i], dfmax = numb_questions + region_number - 1,
                      trace.it = 0)
    
    best_fit_step = select.glmnetcr(cvfit, which = "BIC") ##We store the step that yielded the best fitting models
    fitted<-fitted(cvfit, s = best_fit_step)
    AIC_BIC[i, 2] = fitted$AIC
    AIC_BIC[i, 3] = fitted$BIC
    }
    
    optimal_alpha = AIC_BIC[which.min(AIC_BIC[,"BIC"]), "Alpha"]
    final_cvfit <- glmnetcr(stable_input_matrix, target_vector, weights = weight, nlambda = 100, 
                      alpha = optimal_alpha, dfmax = numb_questions + region_number - 1,
                      trace.it = 0)    

    return(list(final_elastic_net = final_cvfit, optimal_alpha = optimal_alpha))
  }
