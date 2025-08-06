## Data Preparation
power<-read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings="?",stringsAsFactors=FALSE)
date<-as.Date(power$Date,"%d/%m/%Y")
power<-power[(date>="2007-02-01")&(date<="2007-02-02"),]
power$datetime<-strptime(paste(power$Date, power$Time), "%d/%m/%Y %H:%M:%S")

## Draw Plot
png(file="plot2.png")
with(power,plot(datetime,Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)"))
dev.off()
