library(caret)
library(rpart)
library(randomForest)
datos<-read.table("pml-training.csv",sep=',',header=TRUE,na.strings=c("NA", "#DIV/0!"))
datos<-data.frame(datos[,8:11],datos[,37:49],datos[,60:68],datos[,84:86],datos[,115:124],datos[,151:160])
colnames(datos)
inTrain<-createDataPartition(y=datos$classe,p=0.7,list=FALSE)
training<-datos[inTrain,]
testing<-datos[-inTrain,]
c<-cor(training[1:48],as.numeric(training$classe))
plot(c,main="Correlation between class and the predictors variables")

set.seed(1234)
modelFitRpart<-train(classe ~ ., data=training,na.action=na.omit,method='rpart')
modelFitRfor<-train(classe ~ ., data=training,na.action=na.omit,method='rf')
predModelFitRpart<-predict(modelFitRpart,newdata=testing)
predModelFitRfor<-predict(modelFitRfor,newdata=testing)
plot(modelFitRpart$finalModel,uniform=TRUE,main="Classification tree")
text(modelFitRpart$finalModel,use.n=TRUE,all=TRUE,cex=0.6)
confusionMatrix(predModelFitRpart,testing$classe)
confusionMatrix(predModelFitRfor,testing$classe)

datos2<-data.frame(datos$roll_belt,datos$pitch_forearm,datos$magnet_dumbbell_y,datos$roll_forearm,datos$classe)
inTrain2<-createDataPartition(y=datos2$datos.classe,p=0.7,list=FALSE)
training2<-datos[inTrain2,]
testing2<-datos[-inTrain2,]
modelFitRfor2<-train(classe ~ ., data=training2,na.action=na.omit,method='rf')
predModelFitRfor2<-predict(modelFitRfor2,newdata=testing2)
confusionMatrix(predModelFitRfor2,testing2$classe)

