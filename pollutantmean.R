pollutantmean <- function(directory, pollutant, id=1:332) {
    sumandct<-numeric(2)
    for (idnum in id) {
        if (idnum>0 && idnum<10) {leadz <- "00"
        } else if (idnum>=10 && idnum<100) {leadz <- "0"
        } else {leadz <- ""
        }
        data <- read.csv(paste(directory,"/",leadz,idnum,".csv",sep=""))
        if (pollutant=="sulfate") { pol <- data$sulfate
        } else if (pollutant=="nitrate") { pol <- data$nitrate
        }
        sumandct[1] <- sumandct[1]+sum(pol,na.rm=TRUE)
        sumandct[2] <- sumandct[2]+sum(!is.na(pol))
    }
    sumandct[1]/sumandct[2]
}