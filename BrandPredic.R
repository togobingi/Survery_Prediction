library(caret)
library("readr")
install.packages("doMC")

#pre-processing. Categorize age to ranges
#CatAge <- cut(CompleteResponses$age, breaks = c(19,30,40,50,60,70,81), labels = c("A","B","C","D","E","F"))

#pre-processing. Change dataTypes
CompleteResponses$elevel<-as.numeric(CompleteResponses$elevel) #education Level

CompleteResponses$brand<-as.factor(CompleteResponses$brand) #brand

CompleteResponses$car<-as.numeric(CompleteResponses$car) #car 

CompleteResponses$zipcode<-as.numeric(CompleteResponses$zipcode) #Zipcode


TrainData <-  createDataPartition(y=CompleteResponses$brand, p =.75, list= FALSE) #Split set

#Create Training Set
trainingSet <- CompleteResponses[TrainData,]

#Create Test Set
TestingSet <- CompleteResponses[-TrainData,]

#c.50 code
set.seed(2222)

#Convert as many predictors into vectors to be used in the model
#vars = c("salary", "age")

vars = c("salary", "age", "elevel", "car", "zipcode", "credit")

#C5.0 Model training
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
tree_mod <- train(brand~., data = trainingSet, 
                  method="C5.0", 
                  #mode =C5.0(x = trainingSet[, vars], y = trainingSet$brand), 
                  trControl=fitControl, 
                  tuneLength = 2)


TestingSet$predicted.brand = predict(tree_mod, TestingSet)
postResample(TestingSet$predicted.brand, TestingSet$brand )

#View results of c.50 model
tree_mod


plot(tree_mod)


####################### Random Forest ################

#10 fold cross validation'
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")

#automoatic code
rfGrid <- expand.grid(mtry=c(7))

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest y default)
rfFit1 <- train(brand~., data = trainingSet, method = "rf", trControl=fitControl) # tuneLength = 2, for automatic

#training results
rfFit1

#Actual prediction on Incomplete survey data using the Random Forest model
prediction_results = predict(rfFit1, newdata = SurveyIncomplete)

TestingSet$predicted.brandRF = predict(rfFit1, TestingSet)
postResample(TestingSet$predicted.brandRF, TestingSet$brand )

#Plot different histograms
par(mfrow=c(2,2))
hist(CompleteResponses$salary, breaks = 200)
hist(CompleteResponses$age, breaks = 200)
hist(CompleteResponses$zipcode, breaks = 20)
hist(CompleteResponses$credit, breaks = 200)

#Plot Brand Distribution graph
plot(CompleteResponses$brand, main="Brand Distribution", xlab="Brand", ylab="Frequency")


confusionMatrix(TestingSet$predicted.brandRF, TestingSet$brand)
