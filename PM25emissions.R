#Question1 (for plot1.png)
NEI <- readRDS("summarySCC_PM25.rds")
totemit<-tapply(NEI$Emissions,NEI$year,sum)
Year<-dimnames(totemit)[[1]]
png(file="plot1.png",width=720,height=720)
plot(Year,totemit,type="l",ylab="Total PM2.5 emission (ton)",main="Emission trend in the United States")
dev.off()

#Question2 (for plot2.png)
NEI <- readRDS("summarySCC_PM25.rds")
baltemit<-NEI[NEI$fips=="24510",]
totemit<-tapply(baltemit$Emissions,baltemit$year,sum)
Year<-dimnames(totemit)[[1]]
png(file="plot2.png",width=720,height=720)
plot(Year,totemit,type="l",ylab="Total PM2.5 emission (ton)",main="Emission trend in the Baltimore City")
dev.off()

#Question3 (for plot3.png)
NEI <- readRDS("summarySCC_PM25.rds")
baltemit<-NEI[NEI$fips=="24510",]
totemit<-tapply(baltemit$Emissions,interaction(baltemit$year,baltemit$type),sum)
fac<-dimnames(totemit)[[1]]
facsplit<-strsplit(fac,"\\.")
Year<-sapply(facsplit,function(x){x[1]})
type<-sapply(facsplit,function(x){x[2]})
baltplot<-data.frame(totemit,Year,type)
library(ggplot2)
png(file="plot3.png",width=720,height=720)
qplot(Year,totemit,data=baltplot,facets=.~type,ylab="Total PM2.5 emission (ton)", main="Emission trend by source type in the Baltimore City")
dev.off()

#Question4 (for plot4.png)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
coalSCC<-SCC$SCC[grepl("Comb|comb",SCC$EI.Sector)&grepl("Coal|coal",SCC$EI.Sector)]
coalcomb<-NEI[NEI$SCC %in% coalSCC,]
totemit<-tapply(coalcomb$Emissions,coalcomb$year,sum)
Year<-dimnames(totemit)[[1]]
png(file="plot4.png",width=720,height=720)
plot(Year,totemit,type="l",ylab="Total PM2.5 emission (ton)",main="Emission trend from coal combustion-related sources in U.S.")
dev.off()

#Question5 (for plot5.png)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
motorSCC<-SCC$SCC[grepl("Vehicle|vehicle",SCC$SCC.Level.Two)]
baltmotor<-NEI[NEI$fips=="24510"&(NEI$SCC %in% motorSCC),]
totemit<-tapply(baltmotor$Emissions,baltmotor$year,sum)
Year<-dimnames(totemit)[[1]]
png(file="plot5.png",width=720,height=720)
plot(Year,totemit,type="l",ylab="Total PM2.5 emission (ton)",main="Emission trend from motor vehicle sources in Baltimore City")
dev.off()

#Question6 (for plot6.png)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
motorSCC<-SCC$SCC[grepl("Vehicle|vehicle",SCC$SCC.Level.Two)]
tctymotor<-NEI[(NEI$fips %in% c("24510","06037"))&(NEI$SCC %in% motorSCC),]
tctymotor$city<-ifelse(tctymotor$fips=="24510","Baltimore City","Los Angeles County")
totemit<-tapply(tctymotor$Emissions,interaction(tctymotor$year,tctymotor$city),sum)
fac<-dimnames(totemit)[[1]]
facsplit<-strsplit(fac,"\\.")
Year<-sapply(facsplit,function(x){x[1]})
city<-sapply(facsplit,function(x){x[2]})
#balt99<-totemit[fac=="1999.Baltimore City"]
#la99<-totemit[fac=="1999.Los Angeles County"]
#base99<-ifelse(city=="Baltimore City",balt99,la99)
#emitchange<-totemit-base99
tctyplot<-data.frame(totemit,Year,city)
library(ggplot2)
png(file="plot6.png",width=720,height=720)
qplot(Year,totemit,data=tctyplot,facets=.~city,ylab="Total PM2.5 emission (ton)", main="Emission trend from motor vehicle sources in two cities")
dev.off()
