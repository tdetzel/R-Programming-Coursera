best <- function(state, outcome) {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  columns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
    
  ## Return hospital name in specifsourceied state with lowest 30-day death rate
  options(warn = -1)
  colindex<-match(outcome, outcomes)
  data<-subset(data, data$State==state)
  data[, columns[[colindex]]]<-as.numeric(data[, columns[[colindex]]])
  data<-subset(data, !is.na(data[, columns[[colindex]]]))
  sorted.data<-data[order(data[, columns[[colindex]]], data$Hospital.Name), ]
  sorted.data$Hospital.Name[1]
}