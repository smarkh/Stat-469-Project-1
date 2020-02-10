# 
# Mark Hamilton
#

# Libraries
library(ggplot2)


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
