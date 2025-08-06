# unadjusted relation between automatic/manual transmission and MPG
boxplot(mpg~am,data=mtcars)
summary(lm(mpg~am,data=mtcars))$coef
plot(resid(fit)~fitted(fit))

# model selection
library(car)
fitall<-lm(mpg~.-cyl-gear+as.factor(cyl)+as.factor(gear),data=mtcars)
vif(fitall)
## repeat the above process (removing the variable with the largest VIF in each step)
## and get the final model after VIF screening as the following
fitall<-lm(mpg~.-cyl-disp-gear,data=mtcars)
vif(fitall)
summary(fitall)$coef
## start with car weight (which is the only significant one in the above final model after VIF screening)
## and add variables one by one
fit<-lm(mpg~wt,data=mtcars)
sapply(c("hp","vs","carb","drat","am","qsec"), function(var){
    fit2<-update(fit,paste0("mpg~wt+",var))
    anova(fit,fit2)[2,6]})
## get the model with car weight and horsepower only
## repeat the above process of adding variables one by one
fit<-lm(mpg~wt+hp,data=mtcars)
sapply(c("vs","carb","drat","am","qsec"), function(var){
    fit2<-update(fit,paste0("mpg~wt+hp+",var))
    anova(fit,fit2)[2,6]})
## the final model got is the one with car weight and horsepower only
## change to their reciprocals to make more sense of the model
fit<-lm(mpg~I(1/wt)+I(1/hp),data=mtcars)
## no more variables can be added for this variant final model
sapply(c("vs","carb","drat","am","qsec"), function(var){
    fit2<-update(fit,paste0("mpg~I(1/wt)+I(1/hp)+",var))
    anova(fit,fit2)[2,6]})

# residual diagnosis
plot(hatvalues(fit))
outlier<-which(hatvalues(fit)>0.2) #get some leverage points
plot(dffits(fit))
dffits(fit)[outlier] #one of them has high influence
plot(cooks.distance(fit))
cooks.distance(fit)[outlier] #one of them has high influence (get the same point as above)

# refit the model after removing the influence point
cars<-mtcars[-19,]
fit<-lm(mpg~I(1/wt)+I(1/hp),data=cars)
plot(resid(fit)~fitted(fit)) #now the residual plot looks good
abline(h=0)
summary(fit)

# group the predictors with respect to their correlation from common sense
#hp, vs, carb, cyl, disp
#drat, am, gear
#wt
#qsec
