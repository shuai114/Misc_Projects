act<-read.csv("activity.csv")

## Histogram, mean and median
dailysteps<-tapply(act$steps,act$date,sum,na.rm=TRUE)
hist(dailysteps,col="green")
mean(dailysteps)
median(dailysteps)

## Time series plot and maxima
tssteps<-tapply(act$steps,act$interval,mean,na.rm=TRUE)
interval<-dimnames(tssteps)[[1]]
plot(interval,tssteps,type="l",xlab="Time in Day",ylab="Average Number of Steps Taken",main="Average Number of Steps Taken During a Day")
names(which(tssteps==max(tssteps)))

## Imputing missing values
sum(is.na(act$steps))
newact<-data.frame(interval=as.integer(interval),tssteps)
newact<-merge(act,newact,all.x=TRUE)
naind<-is.na(newact$steps)
newact$steps[naind]<-newact$tssteps[naind]
# Make them equal
newact$tssteps<-NULL
newact<-newact[,c(2,3,1)]
newact<-newact[order(newact$date,newact$interval),]
rownames(newact)<-1:dim(newact)[1]
# Histogram, mean and median
newdailysteps<-tapply(newact$steps,newact$date,sum)
hist(newdailysteps,col="green")
mean(newdailysteps)
median(newdailysteps)

## Are there differences in activity patterns between weekdays and weekends?
weekday<-weekdays(as.Date(newact$date,"%Y-%m-%d"))
newact$weekday<-factor(ifelse(weekday=="Saturday"|weekday=="Sunday","weekend","weekday"))
# Using the lattice package for the panel plot
tssteps<-tapply(newact$steps,interaction(newact$interval,newact$weekday),mean)
fac<-dimnames(tssteps)[[1]]
facsplit<-strsplit(fac,"\\.")
interval<-sapply(facsplit,function(x){x[1]})
weekday<-sapply(facsplit,function(x){x[2]})
paneldat<-data.frame(tssteps,Interval=as.integer(interval),weekday)
library(lattice)
xyplot(tssteps~Interval|weekday,data=paneldat,type="l",layout=c(1,2),ylab="Average number of steps taken")
# Using the base plotting system for the panel plot
splitact<-split(newact,newact$weekday)
tssteps1<-tapply(splitact[[1]]$steps,splitact[[1]]$interval,mean)
interval1<-dimnames(tssteps1)[[1]]
tssteps2<-tapply(splitact[[2]]$steps,splitact[[2]]$interval,mean)
interval2<-dimnames(tssteps2)[[1]]
op<-par(mfrow=c(2,1))
plot(interval1,tssteps1,type="l",xlab="Time in Day",ylab="Average Number of Steps Taken",main="Average Number of Steps Taken During Weekday")
plot(interval2,tssteps2,type="l",xlab="Time in Day",ylab="Average Number of Steps Taken",main="Average Number of Steps Taken During Weekend")
par(op)
