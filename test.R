setwd("~/Documents/learn/Titanic/")

test <- read.csv("./test.csv")

test$Survived <- rep(0, 418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "theyallperish.csv", row.names = FALSE)