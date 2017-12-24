rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% data$State)
    return("Invalid State")
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid outcome")
  
  sub_data <- subset(data, data$State == state)
  
  if(outcome == "heart attack"){
    sub_data <- subset(data, data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
    if(num == "best")
      num = 1
    if(num == "worst")
      num = nrow(sub_data)
    if(num > nrow(sub_data)) 
      return("NA....")
    ans <- sub_data[order(as.numeric(sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), sub_data$Hospital.Name),]
    #ans <- arrange(sub_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
    ans <- ans[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    print(ans[num,])
  }
   
  if(outcome == "heart failure"){
    sub_data <- subset(data, data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
    if(num == "best")
      num = 1
    if(num == "worst")
      num = nrow(sub_data)
    #if(num > nrow(sub_data)) 
     # return("NA....")
    ans <- sub_data[order(as.numeric(sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), sub_data$Hospital.Name),]
    #ans <- arrange(sub_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
    ans <- ans[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    print(ans[num,])
  }
    
  if(outcome == "pneumonia"){
    sub_data <- subset(data, data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
    if(num == "best")
      num = 1
    if(num == "worst")
      num = nrow(sub_data)
    if(num > nrow(sub_data)) 
      return("NA....")
    ans <- sub_data[order(as.numeric(sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), sub_data$Hospital.Name),]
    #ans <- arrange(sub_data, Hospital.30.Day.Death..Mortality..Rates.from.pneumonia, Hospital.Name) 
    ans <- ans[,c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    print(ans[num,])
  }
  
}
