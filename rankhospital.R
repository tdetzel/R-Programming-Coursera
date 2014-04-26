rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## Return hospital name in that state with lowest 30-day death rate
  index<-match(outcome, outcomes)
  #filter data
  data<-subset(data, data$State==state)
  data[, columns[[index]]]<-as.numeric(data[, columns[[index]]])
  data<-subset(data, !is.na(data[, columns[[index]]]))
  options(warn = -1)
  if (num=="best") {
    num<-1
  } else if (num=="worst") {
    num<-nrow(data)
  } else if (!((class(num)=="numeric")||(class(num)=="integer"))) {
    return(NA)
  }
  if (!(num>0 && num<=nrow(data))) {
    return(NA)
  }
  sorted.data<-data[order(data[, columns[[index]]], data$Hospital.Name), ]
  sorted.data$Hospital.Name[num]
  }