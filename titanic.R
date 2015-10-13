setwd("~/Documents/learn/Titanic")

trainData <- read.csv("./train.csv")

View(trainData)

prop.table(table(trainData$Sex, trainData$Survived), margin = 1)
#above line shows much of the females survived while the male died
trainData$Child <- 0
trainData$Child[trainData$Age < 18] <- 1

aggregate(trainData$Survived ~ trainData$Child + trainData$Sex, data=trainData, FUN = function(x) {
  sum(x)/length(x)
})

#above function didnt add any new information to the already exsisting information

trainData$quantizedFare <- "30+"
trainData$quantizedFare[trainData$Fare < 30 & trainData$Fare >= 20] <- "20-30"
trainData$quantizedFare[trainData$Fare < 20 & trainData$Fare >= 10] <- "10-20"
trainData$quantizedFare[trainData$Fare < 10] <- "<10"
trainData$quantizedFare <- factor(trainData$quantizedFare, levels = c("<10", "10-20", "20-30", "30+"), ordered = T )

# aggregate(Survived ~ quantizedFare + Child + Sex, data=trainData, FUN=function(x) {sum(x)/length(x)})

#GOING TO INSERT THE MISSING AGE VALUES WITH MEDIAN AGE VALUE
trainData$Age[is.na(trainData$Age)] <- median(trainData$Age, na.rm = T);

#to convert male and female to 1, 0 respectivly
trainData$numSex <- 0;
#trainData$numSex <- ifelse(trainData$Sex=="male", 1, 0)
trainData$numSex <- as.numeric(trainData$Sex == "male") 
  
  #Performing some test if linear regression is valid
  
  #performing correlation test 
  cor(trainData$Survived, trainData$numSex, method = "spearman")
cor(trainData$Survived, trainData$numSex, method = "pearson")

#results shows Sex should definetly be used as we are getting the value -0.5433
#Since the output is categorial variable hence we cant directly plot or find the correlation do binning of the input 
#variable and do manual inspection. as far as i can think right now may be there are better methods ??

prop.table(table(trainData$Survived, trainData$Pclass), 2)*100
prop.table(table(trainData$Survived, trainData$quantizedFare), 2)*100

#finding corleation for quantized fare
cor(trainData$Survived, as.numeric(trainData$quantizedFare), method = "spearman");
cor(trainData$Survived, as.numeric(trainData$quantizedFare), method = "pearson");

trainData$quantizedAge <- "<10"
trainData$quantizedAge[trainData$Age >= 10 & trainData$Age < 20] <- "10-20"
trainData$quantizedAge[trainData$Age >= 20 & trainData$Age < 30] <- "20-30"
trainData$quantizedAge[trainData$Age >= 30 & trainData$Age < 40] <- "30-40"
trainData$quantizedAge[trainData$Age >= 40] <- "40+"

trainData$quantizedAge <- factor(trainData$quantizedAge, levels = c("<10", "10-20", "20-30", "30-40", "40+"), ordered = T)

trainData$familySize <- trainData$Parch + trainData$SibSp

mylogit <- glm(Survived ~ Pclass + numSex + Age + Fare + familySize + Child + Parch + SibSp, data=trainData, family = "binomial")

summary(mylogit)


#creating feature in testData
testData <- read.csv("/home/aman/Documents/learn/Titanic/test.csv")

testData$Child <- 0
testData$Child[testData$Age < 18] <- 1

testData$quantizedFare <- "30+"
testData$quantizedFare[testData$Fare < 30 & testData$Fare >= 20] <- "20-30"
testData$quantizedFare[testData$Fare < 20 & testData$Fare >= 10] <- "10-20"
testData$quantizedFare[testData$Fare < 10] <- "<10"
testData$quantizedFare <- factor(testData$quantizedFare, levels = c("<10", "10-20", "20-30", "30+"), ordered = T )

testData$Age[is.na(testData$Age)] <- median(testData$Age, na.rm = T)

testData$numSex <- 0;
#trainData$numSex <- ifelse(trainData$Sex=="male", 1, 0)
testData$numSex <- as.numeric(testData$Sex == "male") 

testData$quantizedAge <- "<10"
testData$quantizedAge[testData$Age >= 10 & testData$Age < 20] <- "10-20"
testData$quantizedAge[testData$Age >= 20 & testData$Age < 30] <- "20-30"
testData$quantizedAge[testData$Age >= 30 & testData$Age < 40] <- "30-40"
testData$quantizedAge[testData$Age >= 40] <- "40+"

testData$quantizedAge <- factor(testData$quantizedAge, levels = c("<10", "10-20", "20-30", "30-40", "40+"), ordered = T)

testData$familySize <- testData$Parch + testData$SibSp


prob <- predict(mylogit, testData, type = "response") 
toSubmit <- ifelse(prob > 0.5, 1, 0)
out <- data.frame(testData$PassengerId, toSubmit)
View(out)

write.csv(out, file = "/home/aman/Documents/learn/Titanic/out2.csv", row.names = F)
