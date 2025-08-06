rankall <- function(outcome, num = "best") {
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (outcome=="heart attack") {index<-11
    } else if (outcome=="heart failure") {index<-17
    } else if (outcome=="pneumonia") {index<-23
    } else {stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    outcomedata[,index]<-as.numeric(outcomedata[,index])
    state<-sort(unique(outcomedata$State))
    hospital<-character()
    for (eachstate in state) {
        bestall<-outcomedata[(outcomedata$State==eachstate)&(!is.na(outcomedata[,index])),c(2,index)]
        bestall<-bestall[order(bestall[,2],bestall[,1]),1]
        if (num=="best") {hospital<-c(hospital,bestall[1])
        } else if (num=="worst") {hospital<-c(hospital,bestall[length(bestall)])
        } else if (is.numeric(num) && length(num)==1 && num>=1 && num<=length(bestall)) {
            hospital<-c(hospital,bestall[num])
        } else {hospital<-c(hospital,"<NA>")
        }
    }
    data.frame(hospital,state)
}