setwd("/Users/nguyentri/Desktop/Learn/Stat2/FinalReport/")
getwd()
StudentData <- read.csv("StudentsPerformance.csv")
StudentData
attach(StudentData)
summary(StudentData)
length(race.ethnicity)
#Frequency of race and its pie chart
RaceFreq<-table(race.ethnicity)
RaceFreq
pie(RaceFreq,main = "Race Pie Chart")
#Frequency of gender and its pie chart
GenderFreq<-table(gender)
GenderFreq
pie(GenderFreq)
#Frequency of lunch category
LunchFreq<-table(lunch)
LunchFreq
pie(LunchFreq)
#Frequency of test preparation
TestPreFreq<-table(test.preparation.course)
TestPreFreq
pie(TestPreFreq)
#Frequency of parental education
ParentalFreq<-table(parental.level.of.education)
ParentalFreq
pie(ParentalFreq)
#Relationship of Gender and Test preparation course
GenderTestPreFreq<-table(gender,test.preparation.course)
GenderTestPreFreq
barplot(GenderTestPreFreq,main = "Gender over Test preparation course",
        beside=TRUE,xlab="course",ylab="Count",legend.text = T)
TestPreGenderFreq<-table(test.preparation.course,gender)
TestPreGenderFreq
barplot(TestPreGenderFreq,beside=TRUE,legend.text = T)
#Relationship of Test preparation course and Parental level of education
TestPreParentalFreq<-table(test.preparation.course,parental.level.of.education)
TestPreParentalFreq
barplot(TestPreParentalFreq,main = "Test preparation course over parental education",
        beside=TRUE,xlab="",ylab="Count",legend.text = T,las=2)
#Relationship of Gender and Race
GenderRaceFreq<-table(gender,race.ethnicity)
GenderRaceFreq
barplot(GenderRaceFreq,main = "Gender over Race Ethnicity",beside=TRUE,xlab="Race",ylab="Count",legend.text = T)
RaceGenderFreq<-table(race.ethnicity,gender)
RaceGenderFreq
barplot(RaceGenderFreq,beside=TRUE,legend.text = T)
#Relationship between math score and gender
MathScoreGender<-table(gender,math.score)
MathScoreGender
boxplot(math.score~gender)
GenderMathScore<-table(math.score,gender)
GenderMathScore
barplot(GenderMathScore,beside=TRUE)
#Relationship between reading score and gender
ReadingScoreGender<-table(gender,reading.score)
ReadingScoreGender
boxplot(reading.score~gender)
#math score in different race
boxplot(math.score~race.ethnicity,ylab="math score",main="math score vs race",las=1)
#reading score in different race
boxplot(reading.score~race.ethnicity,ylab="reading score",main="reading score vs race",las=1)
#Boxplot for math score and test preparation course
boxplot(math.score~test.preparation.course,ylab="math score",main="math score vs test preparation course",las=1)
#Boxplot for reading score and test preparation course
boxplot(reading.score~test.preparation.course,ylab="reading score",main="reading score vs test preparation course",las=1)
#Boxplot for writing score and test preparation course
boxplot(writing.score~test.preparation.course,ylab="writing score",main="writing score vs test preparation course",las=1)
#Boxplot for math score and parental level of education
boxplot(math.score~parental.level.of.education,ylab="math score",
        main="math score vs parental level of education",las=2,xlab="")
#Scatterplot for math score and reading score within gender female and male
par(mfrow=c(1,2))
plot(math.score[gender=="female"],reading.score[gender=="female"],xlab="math score",ylab="reading score",
     main="mathscore and reading score of female")
plot(math.score[gender=="male"],reading.score[gender=="male"],xlab="math score",ylab="reading score",
     main="mathscore and reading score of male")
#Scatterplot for math score and reading score within test preparation None and completed
plot(math.score[test.preparation.course=="none"],reading.score[test.preparation.course=="none"],xlab="math score",ylab="reading score",
     main="mathscore and reading score of none")
plot(math.score[test.preparation.course=="completed"],reading.score[test.preparation.course=="completed"],
     xlab="math score",ylab="reading score",
     main="mathscore and reading score of completed")
##reset to only one plot for one screen
par(mfrow=c(1,1))
#Boxplot for math score and test preparation course within race group A
boxplot(math.score[race.ethnicity=="group A"]~test.preparation.course[race.ethnicity=="group A"],
        ylab="math score",xlab="test prep course",main="math score vs test preparation course for group A",las=1)
##############################################################################
#####Cac bien dinh luong
summary(math.score)
sd(math.score)
#histogram cua bien dinh luong
#math score
hist(math.score,freq=FALSE,main="Histogram of math score",ylim=c(0,0.03),las=1)
#adding curve
lines(density(math.score),col=2,lwd=3)
summary(math.score)
#plot(density(math.score),add=TRUE,main="Probabiltiy of math score")
boxplot(math.score,ylab="math score",main="Boxplot for Math Score",las=1)

#reading score
hist(reading.score,freq=FALSE,main="Histogram of reading score",ylim=c(0,0.03),las=1)
lines(density(reading.score),col=4,lwd=3)
#plot(density(reading.score),main="Probability of reading score")
boxplot(reading.score,ylab="reading score",main="Boxplot for Reading Score",las=1)

hist(writing.score,freq=FALSE,main="Histogram of writing score",ylim=c(0,0.03),las=1)
lines(density(writing.score),col=5,lwd=3)
#plot(density(reading.score),main="Probability of writing score")
boxplot(writing.score,ylab="writing score",main="Boxplot for Writing Score",las=1)

#math score with gender in different race
boxplot(math.score~gender*race.ethnicity,ylab="math score",xlab="",
        main="math score with gender, in diffrent races",las=2,col=c(3,4))
########convert numeric into categorical
classifyMathScore<-cut(math.score,breaks=c(0,10,30,60,90,100),labels=c("Fail","Bad","Sufficient","Good","Excellent"))
FreqClassifyMathScore<-table(classifyMathScore)
FreqClassifyMathScore
barplot(FreqClassifyMathScore,main="Classify Math Score")
#scatterplot for 2 numeric variable math score and reading score
cor(math.score,reading.score)#pretty strong linear association
plot(math.score,reading.score,main="Scatterplot for math and reading score",las=1)
abline(lm(math.score~reading.score),col=2,lwd=5)
################################################################
#One sample t test with math score
boxplot(math.score)
#H0 mu < 65
t.test(math.score,mu=65,alternative = "less",conf.level=0.95)
#two sided confident interval
t.test(math.score,mu=65,conf.level=0.99)
#####################
#Two sample t test for math score and gender ,test for mean
boxplot(math.score~gender)
#two sided test H0 mean math score of female = male
#var.eq = T is boxplot show that one plot has a larger variation than the other
t.test(math.score~gender,mu=0,alt="two.sided",conf=0.95,var.eq=F)
##################
#Correlation between two numeric variables math score and reading score
cor(math.score,reading.score)
#CI for correlation
cor.test(math.score,reading.score)
#Covariance between math score and reading score
cov(math.score,reading.score)
#Produce pair wise plot
pairs(StudentData[,6:8])





# Regression without condition
## Single regression
### Create model and summary
model1 <- lm(reading.score ~ math.score)
summary(model1)

### Draw plot
plot(reading.score,math.score,col='blue')
abline(model1,col='red')

### we check the Hypothesis H0: B1 = 0, with alpha = 0.05
t_c1 <- qt(1 - 0.05/2, df = 998)
### |t_test1| > |t_c1| with t_test1 = 14.1 so we reject the hypothesis 

### we check the Hypothesis H0: B2 = 0, with alpha = 0.05
t_c2 <- qt(1 - 0.05/2, df = 998)
### |t_test2| > |t_c2| with t_test2 = 44.85 so we reject the hypothesis 

### So we have model ReadingScore = 17.14181 + 0.78723 * MathScore

### 90% interval for model1:
confint(model1, level=0.9)
### B1: (15.2,19.1]
### B2: (0.76,0.82)

### Predict model:
predict(model1, data.frame(math.score = 76))
### So when the math.score is 76 the reading score is 77

### We do the same with model2
model2 <- lm(reading.score ~ writing.score)
summary(model2)
### Draw plot
plot(reading.score,writing.score,col='blue')
abline(model2,col='red')

t_c1 <- qt(1 - 0.05/2, df = 998)
### |t_test1| > |t_c1| with t_test1 = 10.69 so we reject the hypothesis B1 = 0

t_c2 <- qt(1 - 0.05/2, df = 998)
### |t_test2| > |t_c2| with t_test2 = 101.23 so we reject the hypothesis B2 = 0

### So we have model ReadingScore = 6.75051 + 0.91719 * MathScore

confint(model2, level=0.9)
### B1: [5.71,7.79]
### B2: [0.9,0.93]

predict(model2, data.frame(writing.score = 93))
### So when the math.score is 93 the reading score is 92

### The Adjusted R-squared of model1 and model2 respectively are: 0.6681 and 0.9112 
### => model 2 is better. 
### This's close to reality because we can see the reading score should close to the writing score than the math score.

## Multiple regression
### Create model and summary
model3 <- lm(reading.score ~ math.score + writing.score)
summary(model3)

t_c1 <- qt(1 - 0.05/2, df = 997)
### |t_test1| > |t_c1| with t_test1 = 8.182 so we reject the hypothesis B1 = 0
### |t_test2| > |t_c1| with t_test2 = 9.538 so we reject the hypothesis B2 = 0
### |t_test3| > |t_c1| with t_test3 = 55.389 so we reject the hypothesis B3 = 0

### So we have model ReadingScore = 5.13979 + 0.13906 * MathScore + 0.80582 * WritingScore

confint(model3, level=0.9)
### B1: [4.11,6.17]
### B2: [0.12,0.16]
### B3: [0.79,0.82]

predict(model3, data.frame(writing.score = 88, math.score = 69))
### So when the math.score is 69 and writing score is 88 the reading score is 86

### Compare the those above single model the Adjusted R-sauared is bigger suggest that
### the reading score should depend on both writing score and math score.

# Regression with condition
# We will check all the female student
femaleStu <- subset(StudentData, gender == 'female')
summary(femaleStu)
attach(femaleStu)

# create model4, 5 and 6 
t_c = qt(1 - 0.05/2, 516)

model4 <- lm(reading.score ~ math.score)
summary(model4)

model5 <- lm(reading.score ~ writing.score)
summary(model5)

model6 <- lm(reading.score ~ math.score + writing.score)
summary(model6)

# Compare model 4,5,6 to model 1,2,3 we can see they're pretty similar
# So we conclude that the relations between don't different where you're male or female.

