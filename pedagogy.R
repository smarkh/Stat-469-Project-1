# 
# Mark Hamilton
#

# Libraries
library(ggplot2)
library(nlme)

# Grab data
ped <- read.table("https://mheaton.byu.edu/docs/files/Stat469/Topics/1%20-%20Independence/3%20-%20Project/Data/ClassAssessment.txt",
                  header = TRUE,
                  sep = " ")

# Exploratory Data Analysis
# boxplot of semester
ped$Semester <- as.factor(ped$Semester) # change semester to factor 
ggplot(data=ped, aes(x=Semester, y=Final)) + geom_boxplot()

# scater plot of number of students and final score
ggplot(ped, aes(x=NStudents, y=Final)) + 
  xlab("Number of Students") +
  ylab("Final Grade") +
  geom_point()

# scater plot of exam 1 score and final score 
ggplot(ped, aes(x=Exam1, y=Final)) + 
  xlab("Exam1 Score") +
  ylab("Final Grade") +
  geom_point()

# scater plot of exam 2 and final score
ggplot(ped, aes(x=Exam2, y=Final)) + 
  xlab("Exam2 Score") +
  ylab("Final Grade") +
  geom_point()

# scatter plot of exam3 and final score
ggplot(ped, aes(x=Exam3, y=Final))  + 
  xlab("Exam3 Score") +
  ylab("Final Grade") +
  geom_point()

# scatter plot of homework score and final score
ggplot(ped, aes(x=HW, y=Final))  + 
  xlab("Homework Score") +
  ylab("Final Grade") +
  geom_point()

# scatter plot of quiz scores and final scores
ggplot(ped, aes(x=Quiz, y=Final))  + 
  xlab("Quiz Score") +
  ylab("Final Grade") +
  geom_point()

# Fit Heteroskedastic model with unequal variance on quizes
hs_mod <- gls(model = Final ~ ., data = ped, method = "ML", 
              weights=varExp(form=~Quiz)) 
confint(hs_mod)

summary(hs_mod)
#betas
hs_mod$coefficients
# Theta
coef(hs_mod$modelStruct, unconstrained=FALSE)
# sigma
hs_mod$sigma


# check assumptions on hs model
# Construct added variable plots and assess if the linearity assumption is OK for this data.
# see eda plots above
# the linearity assumption seems correct for every continuous quantifiable variable. 

# Construct a histogram of the standardized residuals and run a KS-test to see if the normality 
# assumption is OK for this data.
ggplot()+geom_histogram(mapping=aes(x=resid(hs_mod, type = "pearson")))
# This data appears to be normally distributed

# Draw a scatterplot of the fitted values vs. standardized residuals and run a BP-test to see if 
# the equal variance assumption is OK for this data
ggplot(data=ped, aes(x=fitted.values(hs_mod), y=resid(hs_mod, type = "pearson"))) + geom_point()
# this plot looks like the standardized residuals are centered around zero

# do cross validation to evaluate coverage and rmse using monte carlo
source("predictgls.R")

n.cv <- nrow(ped) #Number of CV studies to run
rpmse <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
# n <- nrow(bw)
for(cv in 1:n.cv){
  ## Split into test and training sets
  test.set <- ped[cv,]
  train.set <- ped[-cv,]
  
  ## Fit a lm() using the training data
  train.lm <- gls(model = Final ~ ., data = ped, method = "ML",
                  weights=varExp(form=~Quiz))
  
  ## Generate predictions for the test set
  my.preds <- predictgls(glsobj=train.gls, newdframe=test.set, level=0.95)
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Final']]-my.preds[,'Prediction'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ((test.set[['Final']] > my.preds[,'lwr']) & (test.set[['Final']] < my.preds[,'upr'])) %>% mean()
}
# plot RPMSE
hist(rpmse)
mean(rpmse)
# plot coverage
hist(cvg)
mean(cvg)
# all of these plots appear to be approximately normal
