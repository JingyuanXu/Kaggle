setwd("C:/Users/xuj/Downloads/titanic")
trainset <- read.csv('train.csv')
testset <- read.csv('test.csv')
table(trainset$Survived)
prop.table(table(trainset$Survived))
testset$Survived <- rep(0,418)

#predict by gender and fare
prop.table(table(trainset$Sex,trainset$Survived)) #by gender and age

summary(trainset$Fare)

trainset$Fare2 <- '>30'
trainset$Fare2[trainset$Fare>=20 & trainset$Fare<30] <-'20-30'
trainset$Fare2[trainset$Fare>=10 & trainset$Fare<20] <- '10-20'
trainset$Fare2[trainset$Fare<10] <- '<10'

prop.table(table(trainset$Fare2,trainset$Survived)) # fare over 30 have better survive rate

aggregate(Survived ~ Sex+Fare2+Pclass, data=trainset, FUN = function(x){ sum(x)/length(x)})

testset$Survived<-0
testset$Survived[testset$Sex=='female'] <-1
testset$Survived[testset$Sex=='female' & testset$Pclass==3 &testset$Fare >20] <-0-1

write.csv(data.frame(PassengerId=testset$PassengerId,Survived=testset$Survived),file = "submit1.csv")



           