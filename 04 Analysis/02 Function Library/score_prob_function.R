# Inputs a scorecard and loess object trained on the training data
# Outputs a predicted probability

score_prob_function <-
  function(loess_object,
           score) {
    pred_test_pr <- data.frame(predict(loess_object, score), score)
    pred_test_pr[, 1]
  }