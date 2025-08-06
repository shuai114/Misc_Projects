complete <- function(directory, id = 1:332) {
    nobs <- numeric()
    for (idnum in id) {
        if (idnum>0 && idnum<10) {leadz <- "00"
        } else if (idnum>=10 && idnum<100) {leadz <- "0"
        } else {leadz <- ""
        }
        data <- read.csv(paste(directory,"/",leadz,idnum,".csv",sep=""))
        nobs <- c(nobs,sum(complete.cases(data)))
    }
    data.frame(id,nobs)
}