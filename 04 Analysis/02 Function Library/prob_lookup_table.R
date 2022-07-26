# Inputs the lookup table matrix (which is the scores and predicted prob from either the training or full survey) and the score vector
# Outputs  probabilities associated with scores 

prob_lookup_table <- function(table, score) {
  
    table<- as.data.frame(table) #Transform table as data-frame
    score <- as.data.frame(score) #Transform score as data_frame
    score$id  <- 1:nrow(score)
    merge_score_lookup <- merge(table, score, by ="V1")
    merge_score_lookup[order(merge_score_lookup$id),2] 
  }