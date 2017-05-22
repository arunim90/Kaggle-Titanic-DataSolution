train_data<-read.table(file.choose(),sep=",",header=TRUE) 
test_data<-read.table(file.choose(),sep=",",header=TRUE) 
head(train_data)
str(train_data)
table(train_data$Sex)
table(train_data$Age)
summary(train_data)
table(train_data$Survived)
table(train_data$Pclass)
summary(train_data$Sex)
summary(train_data$Age)
prop.table(table(train_data$Survived, train_data$Sex),1) #gives the row wise proportion
test_data$Survived <- 0
test_data$Survived[test_data$Sex == 'female'] <- 1
table(test_data$Survived)
table(test_data$Sex)
prop.table(table(test_data$Survived, test_data$Sex),1)
new_file <- data.frame(PassengerId = test_data$PassengerId, Survived = test_data$Survived)
write.csv(new_file, file = "Arunim_Prediction1.csv", row.names = FALSE)
#The prediction is if the passengers aboard are female they survive.

med <- median(train_data$Age)
train_data$Age[is.na(train_data$Age)] <- 0 #replacing the null values with 0
med
train_data$Age[which(train_data$Age==0)] <- med #replacing 0 with median of age
train_data$Age 
#Checking the factors affecting survival of passengers
aggregate(Survived ~ Sex, data = train_data,sum)
aggregate(Survived ~ Sex, data = train_data,length)
aggregate(Survived ~ Sex, data = train_data,function(x){sum(x)/length(x)})
aggregate(Survived ~ Age, data = train_data, sum)
aggregate(Survived ~ Sex + Age, data = train_data,sum)
aggregate(Survived ~ Pclass, data = train_data,sum)
aggregate(Survived ~ Sex + Pclass, data = train_data,sum)
train_data["Child"] <- 0
x <- 1
while(x<=891){
if(train_data$Age[x] < 18){
  train_data$Child[x] <- 1
}
x <- x + 1  
}
train_data$Child
table(train_data$Child)
head(train_data)
aggregate(Survived ~ Child, data = train_data,sum)
aggregate(Survived ~ Child, data = train_data,length)
aggregate(Survived ~ Sex + Child, data = train_data,sum)
aggregate(Survived ~ Sex + Child, data = train_data,length)
aggregate(Survived ~ Sex + Child, data = train_data,function(x){sum(x)/length(x)})
aggregate(Survived ~ Sex + Child + Pclass, data = train_data,sum)
aggregate(Survived ~ Sex + Child + Pclass, data = train_data,length)
aggregate(Survived ~ Sex + Child + Pclass, data = train_data,function(x){sum(x)/length(x)})
aggregate(Survived ~ Sex + Pclass, data = train_data,function(x){sum(x)/length(x)})
aggregate(Survived ~ Fare, data = train_data,sum)
aggregate(Survived ~ Embarked, data = train_data,sum)
aggregate(Survived ~ Embarked + Sex, data = train_data,sum)
aggregate(Survived ~ Embarked + Sex, data = train_data,length)
summary(train_data$Embarked)
is.na(train_data$Embarked)
aggregate(Survived ~ Sex + Embarked, data = train_data,function(x){sum(x)/length(x)})
aggregate(Survived ~ Sex + Embarked + Pclass, data = train_data,sum)
aggregate(Survived ~ Sex + Embarked + Pclass, data = train_data,length)
aggregate(Survived ~ Sex + Embarked + Pclass, data = train_data,function(x){sum(x)/length(x)})
test_data$Survived <- 0
test_data$Survived[test_data$Sex == 'female'] <- 1
test_data$Survived[test_data$Sex == 'female' & test_data$Embarked == 'S' & test_data$Pclass == 3] <- 0
new_file <- data.frame(PassengerId = test_data$PassengerId, Survived = test_data$Survived)
write.csv(new_file, file = "C:/Users/Arunim Das/Desktop/Data Science/Kaggle/Arunim_Prediction2.csv", row.names = FALSE)
#The prediction is if the passengers aboard are female and have boarded from Southampton in passenger class 3 they will not survive.

#prediction 3: Decision Tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train_data, method="anova")
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train_data, method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
library('rpart.plot')
fancyRpartPlot(fit)
Prediction <- predict(fit, test_data, type = "class")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/Arunim Das/Desktop/Data Science/Kaggle/Arunim_Prediction3.csv", row.names = FALSE)

#Feature Engineering
test_data$Survived <- NA
train_data2 <- read.table(file.choose(),sep=",",header=TRUE) 
bind <- rbind(train_data2, test_data)
head(bind)
bind$Name <- as.character(bind$Name)
bind$Name[1]
strsplit(bind$Name[1], split='[,.]')
strsplit(bind$Name[1], split='[,.]')[[1]]
strsplit(bind$Name[1], split='[,.]')[[1]][2]
bind$Title <- sapply(bind$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]} )
table(bind$Title)
bind$Title <- sub(' ','', bind$Title)
bind$Title[bind$Title %in% c('Mme', 'Mlle', 'Dona', 'Lady', 'Ms', 'the Countess')] <- 'Lady'
bind$Title[bind$Title %in% c('Capt', 'Col', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
table(bind$Title)
bind$Title<- factor(bind$Title)
bind$Family <- bind$SibSp + bind$Parch + 1
bind$Surname <- sapply(bind$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]} )
table(bind$Surname)
bind$Surname[bind$Surname <= 2] <- 'Small'
bind$FamilyID <- paste(as.character(bind$Family), bind$Surname, sep = "")
bind$FamilyID[bind$FamilyID <= 2] <- 'Small'
table(bind$FamilyID)
famIDs <- data.frame(table(bind$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
bind$FamilyID[bind$FamilyID %in% famIDs$Var1] <- 'Small'
bind$FamilyID <- factor(bind$FamilyID)
train_data2 <- bind[1:891,]
test_data <- bind[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Family + FamilyID, data=train_data2, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test_data, type = "class")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/Arunim Das/Desktop/Data Science/Kaggle/Arunim_Prediction4.csv", row.names = FALSE)

#Random Forests
summary(bind$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Family, data=bind[!is.na(bind$Age),], method="anova")
bind$Age[is.na(bind$Age)] <- predict(Agefit, bind[is.na(bind$Age),])
summary(bind)
summary(bind$Embarked)
which(bind$Embarked == '')
bind$Embarked[c(62,830)] = 'S'
bind$Embarked <- factor(bind$Embarked)
summary(bind$Fare)
which(is.na(bind$Fare))
bind$Fare[1044] <- 0
bind$Fare[1044] <- median(bind$Fare)
bind$Fare[1044]
bind$FamilyID2 <- bind$FamilyID 
bind$FamilyID2 <- as.character(bind$FamilyID2)
bind$FamilyID2[bind$Family <= 3] <- 'Small'
bind$FamilyID2 <- factor(bind$FamilyID2)
install.packages('randomForest')
library(randomForest)
set.seed(520)
train_data2 <- bind[1:891,]
test_data <- bind[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +Embarked + Title + Family + FamilyID2, data=train_data2, importance=TRUE, ntree=2000)
summary(bind$FamilyID2)
varImpPlot(fit)
Prediction <- predict(fit, test_data)
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/Arunim Das/Desktop/Data Science/Kaggle/Arunim_Prediction5.csv", row.names = FALSE)
install.packages("party")
library(party)
set.seed(520)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Family + FamilyID, data = train_data2, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test_data, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/Arunim Das/Desktop/Data Science/Kaggle/Arunim_Prediction6.csv", row.names = FALSE)
