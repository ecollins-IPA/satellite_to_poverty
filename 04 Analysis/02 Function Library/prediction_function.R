# Inputs the elastic net object and input matrix
# Outputs the predicted probabilities

prediction_function <- function(data, y , weight = weight_name, input_first_col = candidate_feature_first_col,
           input_last_col = candidate_feature_last_col, glm_object , lambda, stable_coef_names, type = "response",
           fam = "binomial") {
    
  weight <- data[[weight]]
  input_matrix <- as.matrix(data[, input_first_col:input_last_col])
  stable_input_matrix <-input_matrix[, stable_coef_names, drop = F]
  predicted_pr <- predict(object = glm_object, newx = stable_input_matrix, family = fam,
         type = type , s = lambda, weights = weight)
  }