# Function that inputs the survey data in Stata format and outputs an RData file.
# Arguments are i) Name of Stata data file (always called data.dta) ii) country iii) survey name
# The data.dta file is stored in the directory PPI_Design/Processed_Survey_Data/Country/Survey

Input_Stata_Data <- function(data = data){
  data<- read.dta(paste(clean,data,sep=""), convert.factors=F)
  return(data)
}