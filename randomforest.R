library(randomForest)
library(ggplot2)

#setting work directory
setwd("F:/MiniProject/Restaurant Revenue Prediction")


train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)
origin <- test

#box plot. Helps to find out outliers and extreme values
ggplot(train, aes(Type, revenue)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Type") + ylab("Revenue") + ggtitle("Type vs Revenue")


#setting current date as a variable
current_date <- as.Date('2016-04-19')

#Formatting date
train$opendate <- as.Date(train$Open.Date, format = '%m/%d/%Y', tz="GMT")

test$opendate <- as.Date(test$Open.Date, format = '%m/%d/%Y', tz="GMT")


#finding out days the restaurants have been stayed open
train$days_open <- as.numeric(difftime(current_date,train$opendate, units = "days"))
test$days_open <- as.numeric(difftime(current_date,test$opendate, units = "days"))

#weeks open
train$weeks <- as.numeric(difftime(current_date,train$opendate, units = "weeks"))
test$weeks <- as.numeric(difftime(current_date,test$opendate, units = "weeks"))

#months open
train$months <- as.numeric(difftime(current_date,train$opendate, units = "days")/30)
test$months <- as.numeric(difftime(current_date,test$opendate, units = "days")/30)


#removing unwanted columns
train$opendate <- NULL
train$Open.Date <- NULL
test$opendate <- NULL
test$Open.Date <- NULL

train$City <- NULL
train$City.Group <- NULL
test$City <- NULL
test$City.Group <- NULL


test$revenue <- 1 

#converting type as a factor and eliminating category MB since train doesnt have MB
test$Type[test$Type == "MB"] <- "DT" 
train$Type <- as.factor(train$Type)
test$Type <- as.factor(test$Type)

#removng outliers observed through box plot
train <- train[train$revenue < 15000000,]

#combining test and train
combi <- rbind(train,test)


#random forest model

rf_model1 <- randomForest(revenue ~ ., data = train[,-1], method = "rf", ntree=1000)
rf_model2 <- randomForest(revenue ~ ., data = train[,-1], method = "rf", ntree=1000)
rf_model3 <- randomForest(revenue ~ ., data = train[,-1], method = "rf", ntree=1000)
rf_model4 <- randomForest(revenue ~ ., data = train[,-1], method = "rf", ntree=1000)

#check  parameters

print(rf_model1)
print(rf_model2)
print(rf_model3)
print(rf_model4)

#plotting important variables
varImpPlot(rf_model1)
varImpPlot(rf_model2)
varImpPlot(rf_model3)
varImpPlot(rf_model4)

#prediction
Prediction1 <- data.frame(Prediction1 = predict(rf_model1,combi[,-1]))
Prediction2 <- data.frame(Prediction2 = predict(rf_model2,combi[,-1]))
Prediction3 <- data.frame(Prediction3 = predict(rf_model3,combi[,-1]))
Prediction4 <- data.frame(Prediction4 = predict(rf_model4,combi[,-1]))

#removing train data from prediction
Prediction1 <- Prediction1[-c(1:135),]
Prediction2 <- Prediction2[-c(1:135),]
Prediction3 <- Prediction3[-c(1:135),]
Prediction4 <- Prediction4[-c(1:135),]

#output
id<-test[,1]
submission <- data.frame("Id"= id,"Prediction"= (Prediction1+Prediction2+Prediction3+Prediction4)/4)

write.csv(submission, "randomforest.csv", row.names = FALSE, quote = FALSE)




