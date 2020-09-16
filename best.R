## Part 2 - Write a function that takes state and outcome as arguments and returns
## the name of the hospital with the lowest 30-day mortality for the specified
## outcome in that state. The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.

## Handling ties - if there is a tie for a given outcome, then use alphabetical
## order to break ties (i.e. if hospitals "a" and "d" are tied then "a' will be returned)

best <- function(state, outcome) {
  
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
    
  ## take the state argument and return data just for desired state in alpha order for tiebreaker
    
    data_by_state <- split(data, data$State)
    state_to_eval <- data_by_state[[state]]
    state_to_eval <- state_to_eval[ order(state_to_eval["Hospital.Name"]), ]
                                    
  ## take the outcome argument and return corresponding column to print
    
    column_directory <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    column_to_print <- column_directory[outcome]
    
  ## coerce outcome to numeric for eval, organize by lowest rate, return hospital
    
    outcome_data <- as.numeric(state_to_eval[, column_to_print])
    clean <- complete.cases(outcome_data) 
    outcome_data <- outcome_data[clean]
    state_to_eval <- state_to_eval[clean, ]
    best <- min(outcome_data)
    index <- match(best, outcome_data)
    result <- state_to_eval[index, 2]
  }
  
  result 
}