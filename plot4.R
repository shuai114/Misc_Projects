## Data Preparation
power<-read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings="?",stringsAsFactors=FALSE)
date<-as.Date(power$Date,"%d/%m/%Y")
power<-power[(date>="2007-02-01")&(date<="2007-02-02"),]
power$datetime<-strptime(paste(power$Date, power$Time), "%d/%m/%Y %H:%M:%S")

## Draw Plots
png(file="plot4.png")
par(mfrow=c(2,2))
with(power,plot(datetime,Global_active_power,type="l",xlab="",ylab="Global Active Power"))
with(power,plot(datetime,Voltage,type="l"))
with(power,plot(datetime,Sub_metering_1,type="n",xlab="",ylab="Energy sub metering"))
with(power,lines(datetime,Sub_metering_1,type="l"))
with(power,lines(datetime,Sub_metering_2,type="l",col="red"))
with(power,lines(datetime,Sub_metering_3,type="l",col="blue"))
legend("topright",col=c("black","red","blue"),lty=1,bty="n",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
with(power,plot(datetime,Global_reactive_power,type="l"))
dev.off()
