## Data Preparation
power<-read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings="?",stringsAsFactors=FALSE)
date<-as.Date(power$Date,"%d/%m/%Y")
power<-power[(date>="2007-02-01")&(date<="2007-02-02"),]

## Draw Plot
png(file="plot1.png")
hist(power$Global_active_power,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power")
dev.off()
