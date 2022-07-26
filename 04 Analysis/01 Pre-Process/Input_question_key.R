# Function that inputs the question key in csv format and outputs an RData file.
# Arguments are i) country ii) survey name
# The data.dta file is stored in the directory PPI_Design/Processed_Survey_Data/Country/Survey

input_question_key <- function(data = data){
  data<- read.csv(paste(clean,data,sep=""),sep=",")
  return(data)
}

