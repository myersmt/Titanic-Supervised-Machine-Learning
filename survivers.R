#setup
'"
setwd("C:/R/mini_projects/Titanic/")
getwd()
install.packages("ggplot2")
install.packages("caret")
install.packages("boot")
install.packages("lattice")
install.packages("randomForest")
library(randomForest)
library(caret)
library(boot)
"'
#read in the files
actual_survivors <- read.csv("gender_submission.csv", stringsAsFactors = FALSE) #read the excel sheet to get information
train_na <- read.csv("train.csv", stringsAsFactors = FALSE) #read the excel sheet to get information
test_na <- read.csv("test.csv", stringsAsFactors = FALSE) #read the excel sheet to get information

#checking for NA for Train
table(is.na(train_na)) #has 177 NA
table(is.na(train_na$Fare))#looks good
table(is.na(train_na$Embarked))#looks good
table(is.na(train_na$Parch))#looks good
table(is.na(train_na$Age))#has 177 NA
train_median = median(train_na$Age, na.rm = TRUE)
train_na[is.na(train_na$Age), "Age"] <- train_median
train <- train_na
table(is.na(train)) #alright now it is good to go

#checking for NA for Test
table(is.na(test_na)) #has 87 NA
table(is.na(test_na$Fare))#has 1 NA
test_median_fare = median(test_na$Fare, na.rm = TRUE)
test_na[is.na(test_na$Fare), "Fare"] <- test_median_fare
table(is.na(test_na)) #has 86 NA
table(is.na(test_na$Fare))#has 0 NA now
table(is.na(test_na$Embarked))#looks good
table(is.na(test_na$Parch))#looks good
table(is.na(test_na$Age))#has 86 NA
test_median_age = median(test_na$Age, na.rm = TRUE)
test_na[is.na(test_na$Age), "Age"] <- test_median_age
table(is.na(test_na)) #has 0 NA now
table(is.na(test_na$Age))#has 0 NA now
test <- test_na
table(is.na(test)) #alright now it is good to go

#checking for NA for Actual Survivors
table(is.na(actual_survivors)) #good to go

#removing "" in embarked
table(train$Embarked) #2 ""
table(test$Embarked) #good
train[train$Embarked=='', "Embarked"] <- 'S' #Replacing with the mode
table(train$Embarked) #Good to go

#casting
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Survived <- as.factor(train$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
actual_survivors$Survived <- as.factor(actual_survivors$Survived)

#checking
str(train) #need to remove empty in embarked
str(test) #dont need to remove in embarked

#Survived based on only what I believe are important factors, name i dont think matters and etc.
train_survived <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
train_survived_formula <- as.formula(train_survived)

#randomForest
titanic_forest <- randomForest(formula = train_survived_formula, data = train)

#putting in the people to predict
test_predict_based_on_id <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survival_predict = predict(titanic_forest, newdata = test)

#testing based on passengerid
PassengerId <- test$PassengerId
new_compare <- as.data.frame(PassengerId)
new_compare$Survived <-  survival_predict

#writing new prediction csv file
write.csv(new_compare, file="my_predictions.csv", row.names = FALSE)
my_predictions <- read.csv("my_predictions.csv", stringsAsFactors = FALSE)
my_predictions$Survived <- as.factor(my_predictions$Survived)

#checking success
confusionMatrix(my_predictions$Survived, actual_survivors$Survived)

#Plotting to see better
actual_survivors$predict_full <- my_predictions$Survived==actual_survivors$Survived
ggplot(actual_survivors,aes(x=PassengerId,y=Survived))+
  geom_jitter(aes(col=predict_full))+
  theme_bw()
