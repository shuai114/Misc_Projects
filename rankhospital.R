rankhospital <- function(state, outcome, num = "best") {
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state %in% unique(outcomedata$State))) {
        if (!(state %in% c("AS","MP","UM"))) {stop("invalid state")
        } else {message("no such state in data")
                return()
        }
    }
    if (outcome=="heart attack") {index<-11
    } else if (outcome=="heart failure") {index<-17
    } else if (outcome=="pneumonia") {index<-23
    } else {stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank in 30-day death rate
    outcomedata[, index] <- as.numeric(outcomedata[, index])
    outcomedata<-outcomedata[(outcomedata$State==state)&(!is.na(outcomedata[, index])),]
    besthosp<-outcomedata[order(outcomedata[, index],outcomedata[, 2]),2]
    if (num=="best") {besthosp<-besthosp[1]
    } else if (num=="worst") {besthosp<-besthosp[length(besthosp)]
    } else if (is.numeric(num) && length(num)==1 && num>=1 && num<=length(besthosp)) {besthosp<-besthosp[num]
    } else {besthosp<-NA
    }
    besthosp
}