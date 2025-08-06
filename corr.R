corr <- function(directory, threshold = 0) {
    corr <- numeric()
    for (idnum in 1:332) {
        if (idnum>0 && idnum<10) {leadz <- "00"
        } else if (idnum>=10 && idnum<100) {leadz <- "0"
        } else {leadz <- ""
        }
        data <- read.csv(paste(directory,"/",leadz,idnum,".csv",sep=""))
        if (sum(complete.cases(data))>threshold) {
            corr <- c(corr,cor(data$sulfate,data$nitrate,use="complete.obs"))
        }
    }
    corr
}