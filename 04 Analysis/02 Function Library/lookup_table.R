# Function that inputs the min and max score and outputs the probability
  lookup_table <- function(minL, maxL) {
    scores <- 0:100
    ls <- minL + scores * (maxL - minL) / 100
    prob_poverty <- 1/(1 + exp(-ls))
    matrix(rbind(scores, prob_poverty))  
}