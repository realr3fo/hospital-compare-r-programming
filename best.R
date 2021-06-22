best <- function(state, outcome) {
    ## Read outcome data
    hospital <- read.csv("hospital-data.csv", colClasses = "character")
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validStates <- unique(hospital$State)
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% validStates)) stop("invalid state")
    if (!(outcome %in% validOutcome)) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    hospitalByState <- hospital[hospital$State == state,]
    hospitalNames <-hospitalByState$Hospital.Name
    outcomeByHospitalName <- outcomeData[outcomeData$Hospital.Name %in% hospitalNames & outcomeData$State == state,]
    orderedOutcome <- data.frame(Hospital.Name=character())
    if (outcome == "heart attack") {
        outcomeByHospitalName <- outcomeByHospitalName[outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
        orderedOutcome <- outcomeByHospitalName[order(as.numeric(outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcomeByHospitalName$Hospital.Name),]
    }
    if (outcome == "heart failure") {
        outcomeByHospitalName <- outcomeByHospitalName[outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]
        orderedOutcome <- outcomeByHospitalName[order(as.numeric(outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), outcomeByHospitalName$Hospital.Name),]
    }
    if (outcome == "pneumonia") {
        outcomeByHospitalName <- outcomeByHospitalName[outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",]
        orderedOutcome <- outcomeByHospitalName[order(as.numeric(outcomeByHospitalName$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcomeByHospitalName$Hospital.Name),]
    }
    orderedOutcome$Hospital.Name[1]
}
