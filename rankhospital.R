## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. For example, the call:

## rankhospital("MD", "heart failure", 5)

## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.

## Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
## of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
## the hospitals with lowest 30-day mortality rate for heart failure are shown here.

rankHospital <- function(state, outcome, num) {
  
  ## read into data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state argument is valid
  
  if(!(state %in% data$State)) {
    result <- "invalid state"
  }
  
  ## Check that outcome argument is valid
  
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    result <- "invalid outcome"
  }
  
  ## Have now confirmed valid arguments
  
  else {
    
    ## take the state argument and return data just for desired state
    
    data_by_state <- split(data, data$State)
    state_to_eval <- data_by_state[[state]]
   
    
    ## take the outcome argument and return corresponding column to print
    
    column_directory <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    column_to_print <- column_directory[outcome]
    
    ## coerce outcome to numeric for eval, organize by lowest rate, remove missing values, alpha ro tiebreak
    
    outcome_data <- as.numeric(state_to_eval[, column_to_print])
    clean <- complete.cases(outcome_data) 
    outcome_data <- outcome_data[clean]
    state_to_eval <- state_to_eval[clean, ]
    state_to_eval <- state_to_eval[ order( outcome_data, state_to_eval["Hospital.Name"]), ]
    
    ## Handeling num arg longer than list, best, and worse
    
    
    
    if (num %in% c("best","worst")){
        if (num == "best") { result <- state_to_eval[1, "Hospital.Name"]}
    
        else if (num == "worst") {result <- state_to_eval[length(outcome_data), "Hospital.Name"]}}
    
    else if (as.numeric(num) > length(state_to_eval)) { result <- NA}
    
    else result <- state_to_eval[as.numeric(num), "Hospital.Name"]

  }
  
  
  result
    
  }

