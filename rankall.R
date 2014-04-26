rankall <- function(outcome, num = "best") {
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
  
  ## Get data ready
  index<-match(outcome, outcomes)
  states<-sort(unique(data$State))  # Get all states
  data[, columns[[index]]]<-as.numeric(data[, columns[[index]]])
  data<-subset(data, !is.na(data[, columns[[index]]]))  # remove NA
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hospitals<-c()
  for (state in states) {
    statedata<-subset(data, data$State==state)
    idx<-as.numeric(num)
    if (num=="best") {
      idx<-1
    } else if (num=="worst") {
      idx<-nrow(statedata)
    } else if (!((class(num)=="numeric")||(class(num)=="integer"))) {
      idx<-NaN
    }
    if (!(idx>0 && idx<=nrow(data))) {
      idx<-NaN
    }
    if (!is.na(idx)) {
      sorted.statedata<-statedata[order(statedata[, columns[[index]]], statedata$Hospital.Name), ] 
      hospitals<-append(hospitals, sorted.statedata$Hospital.Name[idx])
    } else {
      hospitals<-append(hospitals, NA)
    }
  }
  data.frame(hospital=hospitals, state=states)
}