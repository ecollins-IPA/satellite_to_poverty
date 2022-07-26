# Inputs the lookup data df (which is the scores and predicted prob from either the training or full survey)
# Outputs a lookup table with probabilities associated with scores between 0 and 100, in increments of 1

# Last modification: Manuel Cardona (11/30/2021) - Included if function that selects the appropriate variable
#                                                  between X1 and s1.

create_lookup_table <-  function(lookup_data) {
  if (colnames(lookup_data)[2]=="X1") {
    glm_model <- glm(X1 ~ V1,family=binomial,data=lookup_data)
  }
  if (colnames(lookup_data)[2]=="s1") {
    glm_model <- glm(s1 ~ V1,family=binomial,data=lookup_data)
  }
    new_data<- data.frame(seq(0, 100, 1))
    names(new_data) <- "V1"
    fitted.results <- as.vector(predict(glm_model,newdata = new_data, type='response'))
    as.matrix(cbind(fitted.results,new_data))
}