##Merge the training and the test sets to create one data set
#training sets
x_train<-read.table('./UCI HAR Dataset/train/X_train.txt')
y_train<-read.table('./UCI HAR Dataset/train/y_train.txt')
subject_train<-read.table('./UCI HAR Dataset/train/subject_train.txt')
train<-cbind(subject_train,y_train,x_train)
#test sets
x_test<-read.table('./UCI HAR Dataset/test/X_test.txt')
y_test<-read.table('./UCI HAR Dataset/test/y_test.txt')
subject_test<-read.table('./UCI HAR Dataset/test/subject_test.txt')
test<-cbind(subject_test,y_test,x_test)
#merge them
activity<-rbind(train,test)

##Extract only the mean and standard deviation for each measurement
features<-read.table('./UCI HAR Dataset/features.txt')
extractInd<-sort(c(grep('mean()',features$V2,fixed=TRUE),grep('std()',features$V2,fixed=TRUE)))
activity<-activity[,c(1,2,extractInd+2)]

##Use descriptive activity names to name the activities in the data set
actlabel<-read.table('./UCI HAR Dataset/activity_labels.txt')
activity[,2]<-factor(activity[,2],labels=actlabel$V2)

##Appropriately label the data set with descriptive variable names
varname<-as.character(features$V2[extractInd])
varname[61:66]<-c('fBodyAccJerkMag-mean()','fBodyAccJerkMag-std()','fBodyGyroMag-mean()','fBodyGyroMag-std()','fBodyGyroJerkMag-mean()','fBodyGyroJerkMag-std()')
colnames(activity)<-c('subject','activity',varname)

##Create a tidy data set with the average of each variable for each activity and each subject
tidyact<-activity
tidyact$activity<-as.numeric(tidyact$activity)
tidyact<-apply(tidyact,2,function(elt) as.numeric(tapply(elt,interaction(activity$subject,activity$activity),mean)))
tidyact<-as.data.frame(tidyact)
tidyact$activity<-factor(tidyact$activity,labels=actlabel$V2)
write.table(tidyact,'tidyact.txt',row.names=FALSE)
