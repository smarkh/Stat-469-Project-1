## Project 1- Teaching Pedagogy

## R code

## Load necessary libraries

library(tidyverse)
library(MASS)
library(multcomp)
library(nlme)

## Read in data

classes <- read.table("https://mheaton.byu.edu/docs/files/Stat469/Topics/1%20-%20Independence/3%20-%20Project/Data/ClassAssessment.txt", header=TRUE)

#########
## EDA ##
#########

ggplot(data=classes, aes(x=NStudents, y=Final)) + geom_point()
classes$Semester <- as.factor(classes$Semester)
ggplot(data=classes, aes(x=Semester,y=Final)) + geom_boxplot()
ggplot(data=classes, aes(x=Exam1, y=Final)) + geom_point()+ geom_smooth(se=FALSE)
ggplot(data=classes, aes(x=Exam2, y=Final)) + geom_point()+ geom_smooth(se=FALSE)
ggplot(data=classes, aes(x=Exam3, y=Final)) + geom_point()+ geom_smooth(se=FALSE)
ggplot(data=classes, aes(x=HW, y=Final)) + geom_point()+ geom_smooth(se=FALSE)
ggplot(data=classes, aes(x=Quiz, y=Final)) + geom_point()+ geom_smooth(se=FALSE)

#######################
## Analysis with MLR ##
#######################

## 1. Fit a MLR model and build a 95% CI for the effects
mlr <- lm(Final~., data=classes)
confint(mlr,level=.95)
summary(mlr)
# appears that only Exams are significant

## 3. Show that the N & E assumptions of the MLR model are not met 
ggplot(data=mlr, aes(x=fitted(mlr), y=resid(mlr))) + geom_point()+ geom_smooth(se=FALSE)
ggplot()+geom_histogram(mapping=aes(x=stdres(mlr)))

## 4. Show that transformations are not going to fix the assumptions of the MLR model
ggplot(data=classes, aes(x=log(Quiz), y=Final)) + geom_point()+ geom_smooth(se=FALSE)

# none of these changed anything
mlr1 <- lm(log(Final)~log(NStudents)+log(Exam1)+log(Exam2)+log(Exam3)+log(HW)+log(Quiz)+Semester, data=classes)
ggplot(data=mlr1, aes(x=fitted(mlr1), y=resid(mlr1))) + geom_point()+ geom_smooth(se=FALSE)
ggplot()+geom_histogram(mapping=aes(x=stdres(mlr1)))

mlr1 <- lm(Final~NStudents+log(Exam1)+Exam2+Exam3+HW+log(Quiz)+Semester, data=classes)
ggplot(data=mlr1, aes(x=fitted(mlr1), y=resid(mlr1))) + geom_point()+ geom_smooth(se=FALSE)
ggplot()+geom_histogram(mapping=aes(x=stdres(mlr1)))

mlr1 <- lm(log(Final)~NStudents+log(Exam1)+Exam2+Exam3+HW+log(Quiz)+Semester, data=classes)
ggplot(data=mlr1, aes(x=fitted(mlr1), y=resid(mlr1))) + geom_point()+ geom_smooth(se=FALSE)
ggplot()+geom_histogram(mapping=aes(x=stdres(mlr1)))

mlr1 <- lm(log(Final)~., data=classes)
ggplot(data=mlr1, aes(x=fitted(mlr1), y=resid(mlr1))) + geom_point()+ geom_smooth(se=FALSE)
ggplot()+geom_histogram(mapping=aes(x=stdres(mlr1)))


## Fit heteroskedastic model
gls.fit <- gls(model = Final ~ ., data = classes, method = "ML", 
              weights=varExp(form=~Quiz)) 

source("C:/Users/savani/Documents/BYU/Winter 2020/STAT 469/in_class/predictgls.R")

## describe how well your model fits your data
## cross-validation
n.cv <- nrow(classes) #Number of CV studies to run
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Split into test and training sets
  test.set <- classes[cv,]
  train.set <- classes[-cv,]
  
  ## Fit a lm() using the training data
  train.lm <- gls(model = Final ~ ., data = classes, method = "ML", 
              weights=varExp(form=~Quiz))
  
  ## Generate predictions for the test set
  my.preds <- predictgls(glsobj=train.gls, newdframe=test.set, level=0.95)
  
  ## Calculate bias
  bias[cv] <- mean(my.preds[,'Prediction']-test.set[['EatingOut']])
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Final']]-my.preds[,'Prediction'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ((test.set[['Final']] > my.preds[,'lwr']) & (test.set[['Final']] < my.preds[,'upr'])) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
}
mean(rpmse)
mean(cvg)

## describe how well your model does at predicting
my.preds <- predictgls(gls.fit, classes, level=0.95)

ggplot(my.preds, aes(x=HW,y=Final)) + geom_point() + geom_smooth(se=FALSE) + 
  labs(title="Scatterplot of Predictions with 95% Prediction Intervals",
       x = "Exam1", 
       y = "Final") +
      geom_line(aes(y=Prediction), col="orange") +
      geom_line(aes(y=lwr), col="orange", lty=2) +
      geom_line(aes(y=upr), col="orange", lty=2)


## do you think there are any class activities associated with improved learning? If so, which ones?
confint(gls.fit,level=.95)




# check linearity with lm object and avplots
# because fixing equal variance with gls won't change/affect the linearity
# 3 cases of hetero... fan: exponential, clear grouping: movie, if all July vs all January (groupd defined by month).... fixed variance, you know exactly what the D matrix is

# trying to determine variance
# think about the response variable and what it is... something about it being an average?
# calculate your R^2... R^2= 1- sum()/sum()