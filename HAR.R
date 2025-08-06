training<-read.csv("pml-training.csv")
#summary(training)
#remove those variables with most missing
missing<-sapply(training, function(var) mean(is.na(var)|var==""))
table(missing)
training<-training[,missing==0]
###training$cvtd_timestamp<-strptime(training$cvtd_timestamp,"%d/%m/%Y %H:%M")
#remove those meaningless or cheating variables
training$X<-NULL
###part1<-(training$raw_timestamp_part_1-mean(training$raw_timestamp_part_1))/sd(training$raw_timestamp_part_1)
###part2<-(training$raw_timestamp_part_2-mean(training$raw_timestamp_part_2))/sd(training$raw_timestamp_part_2)
###boxplot(part1~classe,data=training)
###boxplot(part2~classe,data=training)
training$raw_timestamp_part_1<-NULL
training$raw_timestamp_part_2<-NULL
training$cvtd_timestamp<-NULL #using it is like cheating

#train
library(caret)
library(randomForest)
set.seed(56789)
inTrain<-createDataPartition(y=training$classe,p=0.75,list=F)
train<-training[inTrain,]
test<-training[-inTrain,]
#modFit<-train(classe~roll_belt+magnet_belt_y+magnet_arm_x+yaw_dumbbell+magnet_dumbbell_x+accel_forearm_x,data=train,method="rf") #accuracy 0.93
#modFit<-train(classe~.,data=train,method="rf") #accuracy 0.9982
load("HAR.RData")
confusionMatrix(test$classe,predict(modFit,newdata=test))
#save(modFit,file="HAR.RData")

#test
modFit<-train(classe~roll_belt+magnet_belt_y+magnet_arm_x+yaw_dumbbell+magnet_dumbbell_x+accel_forearm_x,data=training,method="rf")
testing<-read.csv("pml-testing.csv")
pred<-predict(modFit,newdata=testing)



#error analysis
mediansd<-apply(train[,-c(1,2,56)],2,function(x) sd(tapply(x,train$classe,median)))
sort(mediansd,decreasing=T)

error<-test[test$classe!=predict(modFit,newdata=test),]
correct<-test[test$classe==predict(modFit,newdata=test),]
par(mfrow=c(1,3))
compare<-function(var){
tempe<-error[,var]
tempt<-train[,var]
tempc<-test[,var]
boxplot(tempe~classe,data=error,xlab="error")
boxplot(tempt~classe,data=train,xlab="train")
boxplot(tempc~classe,data=test,xlab="test")
}

#candidate predictors
num_window
yaw_belt
total_accel_belt
accel_belt_y
accel_belt_z
magnet_belt_z
accel_arm_x
magnet_arm_y
magnet_arm_z
roll_dumbbell
pitch_dumbbell
accel_dumbbell_y
magnet_dumbbell_y
magnet_dumbbell_z
roll_forearm
pitch_forearm
yaw_forearm
magnet_forearm_x
magnet_forearm_y

#old program
chosenvar<-c("classe","roll_belt","magnet_belt_y","magnet_arm_x","yaw_dumbbell","magnet_dumbbell_x","accel_forearm_x")
train<-train[,chosenvar]
test<-test[,chosenvar]
modFit<-train(classe~.,data=train,method="rf")
modFit<-train(classe~.,data=train,method="gbm",verbose=F)
modFit<-train(classe~.,data=train,method="rpart")
modFit<-train(classe~.,data=train,method="nb")
#bagEarth,treebag,bagFDA
modFit<-train(classe~.,data=trainPC,method="rpart",preProcess="pca")
modFit<-train(classe~.,data=train,method="lda")

#use PCA
chvarind<-which(names(train) %in% c("user_name","new_window","classe"))
###M<-abs(cor(train[,-chvarind]))
###diag(M)<-0
###which(M>0.8,arr.ind=T)
preproc<-preProcess(train[,-chvarind],method="pca",thresh=0.8)
preproc$numComp
preproc<-preProcess(train[,-chvarind],method="pca",pcaComp=13)
trainPC<-predict(preproc,train[,-chvarind])
trainPC<-cbind(classe=train$classe,trainPC)
modFit<-train(classe~.,data=trainPC,method="rf")

#cross validation preparation
library(caret)
set.seed(12345)
train<-createFolds(training$classe,returnTrain=T)

#things to think of
predictor
preProcess
method
trControl=trainControl(method="cv")

