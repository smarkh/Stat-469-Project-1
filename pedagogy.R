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
ggplot(ped, aes(x=NStudents, y=Final)) + 
  xlab("Number of Students") +
  ylab("Final Grade") +
  geom_point()

ggplot(ped, aes(x=Exam1, y=Final)) + 
  xlab("Exam1 Score") +
  ylab("Final Grade") +
  geom_point()

ggplot(ped, aes(x=Exam2, y=Final)) + 
  xlab("Exam2 Score") +
  ylab("Final Grade") +
  geom_point()

ggplot(ped, aes(x=Exam3, y=Final))  + 
  xlab("Number of Students") +
  ylab("Final Grade") +
  geom_point()

ggplot(ped, aes(x=HW, y=Final)) + geom_point()
ggplot(ped, aes(x=Quiz, y=Final)) + geom_point()

