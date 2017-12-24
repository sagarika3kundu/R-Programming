rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid outcome")
  
  
  
  if(outcome == "heart attack"){
    sub_data <- subset(data, data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
    #tmp is a list of dataframes segregated by states.
    tmp <- with(data, split(data, data$State))
    if(num == "best")
      num = 1
    if(num == "worst")
      num = nrow(sub_data)
    if(num > nrow(sub_data)) 
      return("NA....")
    #lapply(tmp, function(x){
     #                       x[order(as.numeric(tmp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
      #                              tmp$Hospital.Name)]
       #                     }      )
          
    
    print(tmp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }
  summary(tmp)
}