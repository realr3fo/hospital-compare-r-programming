rankall <- function(outcome, num = "best") {
    ## Read outcome data
    hospital <- read.csv("hospital-data.csv", colClasses = "character")
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states <- unique(hospital$State)
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    validNum <- c("best", "worst")
    if (!(outcome %in% validOutcome)) stop("invalid outcome")
    if (!(num %in% validNum) && !is.numeric(num)) stop("invalid ranking")
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result <- data.frame(hospital=character(), state=character())
    for (currentState in states) {
        hospitalByState <- hospital[hospital$State == currentState,]
        hospitalNames <-hospitalByState$Hospital.Name
        outcomeByHospitalName <- outcomeData[outcomeData$Hospital.Name %in% hospitalNames & outcomeData$State == currentState,]
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
        currentHospital <- ""
        if (length(orderedOutcome$Hospital.Name) == 0) {
            next
        }
        if (num == "worst") {
            currentHospital <- orderedOutcome$Hospital.Name[length(orderedOutcome$Hospital.Name)]
        }
        if (is.numeric(num)) {
            currentHospital <- orderedOutcome$Hospital.Name[num]    
        }
        if (num == "best") {
            currentHospital <- orderedOutcome$Hospital.Name[1]    
        }
        currentResult <- data.frame(hospital = currentHospital, state = currentState)
        result <- rbind(result, currentResult)
    }
    rownames(result) <- result$state
    result <- result[order(result$state),]
    result
}