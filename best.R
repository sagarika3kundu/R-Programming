
#library plyr is used to rename the column names.
library(plyr)

best <- function(state, outcome){
 data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 if (!state %in% data$State)
 return("Invalid State")
 if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
 return("Invalid outcome")
 
 #removing NA values
 #data[data == "Not Available"] <- 0
 
 #extracting rows having State name as passed to the function
 sub_data <- subset(data, data$State == state)
 
 # renaming column name to access them properly
 sub_data <- rename(sub_data, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"= "Heart_Attack", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"= "Pneumonia", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"= "Heart_Failure"))
 if(outcome == "heart attack"){
   
   #which.min[,] will give the entire row in the dataset where column is minimum
   ans_row <- sub_data[which.min(sub_data$Heart_Attack), ]
   
   #to sort the hospital names in ascending order but it can only be done on data frames not to vectors.
   ans <- arrange(ans_row, Hospital.Name)
   ans <- ans_row$Hospital.Name
   print(ans)
 }
 if(outcome == "pneumonia"){
   ans_row <- sub_data[which.min(sub_data$Pneumonia), ]
   #print(ans_row)
   ans <- arrange(ans_row, Hospital.Name)
   ans <- ans_row$Hospital.Name
   print(ans)
 }
 if(outcome == "heart failure"){
   ans_row <- sub_data[which.min(sub_data$Heart_Failure), ]
   ans <- arrange(ans_row, Hospital.Name)
   ans <- ans_row$Hospital.Name
   print(ans)
 }
}

