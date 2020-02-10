# 
# Mark Hamilton
#

# Libraries
library(ggplot2)
library(GGally)
library(car)
library(MASS)
library(lmtest)
library(multcomp)
library(magrittr)
library(nlme)


# Grab data
ped <- read.table("https://mheaton.byu.edu/docs/files/Stat469/Topics/1%20-%20Independence/3%20-%20Project/Data/ClassAssessment.txt",
                  header = TRUE,
                  sep = " ")

# Exploratory Data Analysis
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

# fit hetero skedastic and linear and transform quizes check assumptions on all of them
lm_mod <- lm(Final~. , data = ped)
summary(lm_mod)

# check assumptions
# Construct added variable plots and assess if the linearity assumption is OK for this data.
avPlots(lm_mod)
# the linearity assumption seems correct because the plot appears to be linear

# Construct a histogram of the standardized residuals and run a KS-test to see if the normality 
# assumption is OK for this data.
ggplot()+geom_histogram(mapping=aes(x=stdres(lm_mod)))
# This data appears to be normally distributed

# Draw a scatterplot of the fitted values vs. standardized residuals and run a BP-test to see if 
# the equal variance assumption is OK for this data
plot(stdres(lm_mod) ~ fitted.values(lm_mod), 
     ylab = 'standardized residuals',
     xlab = 'fitted values')
abline(0, 0, col='red')
bptest(lm_mod)
# the plot appears to show equal variance because the residuals are not equally varied around zero, the bp test 
# also confirms this as the pvalue is greater than .05

##############################################################
hs_mod <- gls(model = Final ~ ., data = ped, method = "ML", 
              weights=varExp(form=~Quiz)) 
summary(hs_mod)


#betas
hs_mod$coefficients
# Theta
coef(hs_mod$modelStruct, unconstrained=FALSE)
# sigma
hs_mod$sigma
