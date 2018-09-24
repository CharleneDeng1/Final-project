# Final-project
Final project for term 2
library(ggplot2)
library(VIM)
library(mice)
library(tidyr)
library(purrr)
library(Rcpp)
library(Amelia)
library(caret)
library(pscl)
library(Hmisc)
library(parallel)
library(doParallel)
library(grid)
library(rpart.plot)
library(gridExtra)
library(plyr)
library(readxl)
library(DMwR)

setwd("D:/POC/all data")
getwd()

#-----------StitchPull----------
ABT_13 <- read.csv("Actual6_BallThickness_lot13.csv",header = TRUE)
ABT_14 <- read.csv("Actual6_BallThickness_lot14.csv",header = TRUE)
str(ABT_13)
sum(is.na(ABT_13))
summary(ABT_13$Result)
sapply(ABT_13,function(x) sum(is.na(x)))

sum(complete.cases(ABT_13))
sum(complete.cases(ABT_14))



ABT_13_M <- names(ABT_13) %in% c('X1stBondBaseTime','X1stBondBaseUSGImpedance','X2ndBondBaseForce',
                                 'X2ndBondBaseTime','X2ndBondBaseUSGImpedance',
                                 'FreeAir1stUSGImpedance','FreeAir2ndUSGImpedance')

ABT_13_drop <- ABT_13[!ABT_13_M]
str(ABT_13_drop)
ABT_13_drop %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
#---NO double peak
ABT_13_D <- names(ABT_13_drop) %in% c('X2ndBondUSGCurrent','X1ndBondUSGCurrent')
ABT_13_F <- ABT_13_drop[!ABT_13_D]


str(ABT_14)
sum(is.na(ABT_14))
summary(ABT_14$Result)
sapply(ABT_14,function(x) sum(is.na(x)))
ABT_14_M <- names(ABT_14) %in% c('X1stBondBaseTime','X1stBondBaseUSGImpedance','X2ndBondBaseForce',
                                 'X2ndBondBaseTime','X2ndBondBaseUSGImpedance',
                                 'FreeAir1stUSGImpedance','FreeAir2ndUSGImpedance')
ABT_14_drop <- ABT_14[!ABT_14_M]
str(ABT_14_drop)
ABT_14_drop %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
#---NO double peak
ABT_14_D <- names(ABT_14_drop) %in% c('X2ndBondUSGCurrent','X1ndBondUSGCurrent')
ABT_14_F <- ABT_14_drop[!ABT_14_D]


total_ABT <- rbind(ABT_13_F,ABT_14_F)

Total <- mice(total_ABT,m=5,method = "norm",maxit = 5, 
              diagnostics = FALSE , remove_collinear = FALSE)
summary(Total)

ABT_Total_completedata <- mice::complete(Total)
anyNA(Total)
sum(is.na(ABT_Total_completedata))
sapply(ABT_Total_completedata,function(x) sum(is.na(x)))
densityplot(Total)
summary(ABT_Total_completedata)
str(ABT_Total_completedata)
#--------------------------------Standardization--------------------------------
ABT_Total_completedata[,3:32] <- scale(ABT_Total_completedata[,3:32],center = FALSE, scale = TRUE)
str(ABT_Total_completedata)
sapply(ABT_Total_completedata,function(x) sum(is.na(x)))

library(caret) 
library(e1071) 
set.seed(100)
ModelControl <- trainControl(method = "cv",number = 5)

class(ModelControl)

Model_Formula <- as.formula(OutPut~X1stBaseDepression+X1stBondBaseForce+X1stBondUSGCurrent
                            +X1stContactDepression+X1stContactLevel+X1stContactZDAC
                            +X1stSearchHeight+X1stSearchPosition+X1stSearchSpeed
                            +X2ndBaseDepression+X2ndContactDepression
                            +X2ndContactLevel+X2ndContactZDAC+X2ndSearchHeight+X2ndSearchPosition
                            +EFOCurrent+EFOTime+FireLevel+LoopBaseDistance+LoopTopDistance
                            +LoopWireFeed+Moveto1stSearchHeight)


Model_Formula2 <- as.formula(Result~X1stBaseDepression+X1stBondBaseForce+X1stBondUSGCurrent
                            +X1stContactDepression+X1stContactLevel+X1stContactZDAC
                            +X1stSearchHeight+X1stSearchPosition+X1stSearchSpeed
                            +X2ndBaseDepression+X2ndContactDepression
                            +X2ndContactLevel+X2ndContactZDAC+X2ndSearchHeight+X2ndSearchPosition
                            +EFOCurrent+EFOTime+FireLevel+LoopBaseDistance+LoopTopDistance
                            +LoopWireFeed+Moveto1stSearchHeight)

#---- Training and Test Set ----
smp_size <- floor(0.75 * nrow(ABT_Total_completedata))
set.seed(123)
train_ind <-sample(seq_len(nrow(ABT_Total_completedata)),size = smp_size)

ABT_train <- ABT_Total_completedata[train_ind,]
ABT_test <-ABT_Total_completedata[-train_ind,]
str(ABT_train)
summary(ABT_train)
summary(ABT_test)
str(ABT_test)

#--------------------------------Desicion Tree--------------------------------
library(rpart)
library(rpart.plot)
library(survival)
library(RColorBrewer)
library(rattle)

library(caret)
#set parameter 
cartGrid <- expand.grid(.cp=(1:50)*0.01)
tree_model <- train(Model_Formula, data = ABT_train, method = "rpart",
                    trControl = ModelControl, tuneGrid = cartGrid)
print(tree_model)
#when cp equal to 0.01 rmse lowest
#cp0.01  RMSE0.05285823605  
DecisionTree <- rpart(Model_Formula,method = 'anova',data = ABT_train,control = rpart.control(cp=0.008))
DecisionTree_class <- rpart(Model_Formula,method = 'class',data = ABT_train,control = rpart.control(cp=0.008))

printcp(DecisionTree)
plotcp(DecisionTree)
summary(DecisionTree)
plot(DecisionTree, uniform=TRUE, main="Decision Tree for BallThickness")
text(DecisionTree, use.n=TRUE, all=TRUE, cex=1)


#-------------------------------- Splilit Rule --------------------------------
asRules(DecisionTree)

#--------------------------------prune tree--------------------------------
DecisionTree_prune <- prune(DecisionTree,cp= DecisionTree$cptable
                            [which.min(DecisionTree$cptable[,"xerror"]),"CP"])
plot(DecisionTree_prune, uniform=TRUE, main="Decision Tree after prune")
text(DecisionTree_prune, use.n=TRUE, all=TRUE, cex=.5)
asRules(DecisionTree_prune)
#set.seed(99)
#rpart.plot(DecisionTree_prune)
predicted_DT <- predict(DecisionTree_prune,ABT_test)
postResample(pred = predicted_DT, obs = ABT_train$OutPut)









Act_DT <- factor(ABT_test$Result)
Pre_DT <- factor(predicted_DT)
summary(Pre_DT)
summary(Act_DT)

DTRmse <- RMSE(ABT_test,predicted_DT)
DTRmse
confusionMatrix(predicted_DT,Act_DT)

predicted <- predict(DecisionTree,ABT_test)
predicted1<-predict(DecisionTree_class,ABT_test[,!names(ABT_test)%in%c("Result")])

postResample(pred = predicted, obs = ABT_train$OutPut)
postResample(pred = predicted1, obs = ABT_train$Result)

#Accuracy1 <- accuracy(ABT_train$Result, predicted1, threshold = 0.2)







#--------------------------------Random Forest--------------------------------
library("MASS")
library("randomForest")
library('import')
library('Metrics')

set.seed(415)
rf_model <- train(Model_Formula, data = ABT_train, method = "parRF", 
                  trControl =  ModelControl, prox = TRUE, allowParallel = TRUE)
predicted_RF3 <- predict(rf_model, newdata = ABT_train)
postResample(pred = predicted_RF3, obs = ABT_train$OutPut)

print(rf_model)

RandomForest_BT <- randomForest(Model_Formula,method = 'anova',
                                  data = ABT_train,
                                  importance=TRUE,ntree=1000,nodesize=100,mtry=12)


RandomForest_BT1 <- randomForest(Model_Formula,method = 'anova',
                                data = ABT_train,
                                importance=TRUE,ntree=800,nodesize=200,mtry=12)

summary(RandomForest_BT)
predicted_RF <- predict(RandomForest_BT, newdata = ABT_train)
plot(RandomForest_BT, uniform=TRUE, main="RandomForest for BallThickness")
text(RandomForest_BT, use.n=TRUE, all=TRUE, cex=.5)


summary(RandomForest_BT1)
predicted_RF <- predict(RandomForest_BT1, newdata = ABT_train)
plot(RandomForest_BT1, uniform=FALSE, main="RandomForest for BallThickness")
text(RandomForest_BT1, use.n=TRUE, all=TRUE, cex=.5)
postResample(pred = predicted_RF, obs = ABT_train$OutPut)


importance(x=RandomForest_BT)

#---Need to clear all plots if error msg "Error in plot.new() : figure margins too large"
VI_for_RF <- par(varImpPlot(x=RandomForest_BT, sort=TRUE, n.var=min(5, nrow(RandomForest_BT$importance)),
                            type=1, class=NULL,scale=TRUE,main="Variables Importance",pch = 22,col="blue",
                            pt.cex = 1,cex = 1))

#RFRmse <- rmse(ABT_test$Result,predicted_RF)
#RFRmse
postResample(pred = predicted_RF, obs = ABT_train$OutPut)


#----------------------------Gradient Boosting-----------------------
library(gbm)
library(caret)
GBD <- gbm(Model_Formula,data = ABT_train,shrinkage=0.01,
           distribution='gaussian',interaction.depth=3, cv.folds=5,
           n.trees=1000,verbose=F)
best.iter <- gbm.perf(GBD,method='cv')
print(best.iter)
summary(GBD,best.iter)

fitControl <- trainControl(method = "cv",number = 10,returnResamp = "all")
tune_Grid <-expand.grid(interaction.depth = 2,n.trees = 1000,shrinkage = 0.01,n.minobsinnode = 10)
tune_Grid2 <-expand.grid(interaction.depth = 3,n.trees = 2000,shrinkage = 0.01,n.minobsinnode = 10)

#-----------tune_Grid  bigger MAE
set.seed(825)
GBDacc <- train(Model_Formula,data = ABT_train,method = "gbm",distribution='gaussian',trControl = fitControl,
                verbose = FALSE,tuneGrid = tune_Grid)
GBDacc

#-----------tune_Grid2 less MAE
set.seed(825)
GBDacc2 <- train(Model_Formula,data = ABT_train,method = "gbm",distribution='gaussian',trControl = fitControl,
                verbose = FALSE,tuneGrid = tune_Grid2)
GBDacc2

#-------------GB class tune_Grid 0.9764796 accuracy
set.seed(825)
GBDacc4 <- train(Model_Formula2,data = ABT_train,method = "gbm",distribution='bernoulli',trControl = fitControl,
                 verbose = FALSE,tuneGrid = tune_Grid)
GBDacc4

#-------------GB class tune_Grid2 0.9972505 accuracy
set.seed(825)
GBDacc3 <- train(Model_Formula2,data = ABT_train,method = "gbm",distribution='bernoulli',trControl = fitControl,
                 verbose = FALSE,tuneGrid = tune_Grid2)

GBDacc3

predicted_GBD1 <- predict(GBDacc2,ABT_test)
P_GBD1_table <- table(predicted_GBD1)
summary(P_GBD1_table)
ABT_table <- table(ABT_test$Result)
summary(ABT_table)
#-----------Accuracy
library(SDMTools)
SDMTools::accuracy(ABT_test,predicted_GBD1,threshold=0.5)
#----------set different threshold for classifer


as.factor(P_GBD1_table)
as.factor(ABT_table)

postResample(pred =predicted_GBD1,obs = ABT_test$Result )





#--------------------------------SVM----------------------------------
library(e1071)
library(caret)
ABT_train$Result <- as.numeric(ABT_train$Result)

model_svm <- svm(Model_Formula,
                 kernel = "radial",type="eps-regression",data = ABT_train)


model_svm2 <- svm(Model_Formula2,type="C-classification",data = ABT_train)

summary(model_svm)
pred_svm <- predict(model_svm,ABT_train)
pred_svm2 <- predict(model_svm2,ABT_train)
summary(pred_svm)
svm_table <-table(pred_svm,ABT_train$Result)
SVMRmse <- rmse(ABT_test$Result,pred_svm)
SVMRmse
postResample(pred = pred_svm, obs = ABT_train$OutPut)
postResample(pred = pred_svm2, obs = ABT_train$OutPut)
#--------------------------------NN--------------------------------

Model_ctrl <- trainControl(method = "cv", number = 10)
library(neuralnet)
NN_model <- neuralnet(Model_Formula,data = ABT_train, hidden = c(5,3,2), 
                      threshold = 0.01,stepmax = 1e+05, rep = 1, startweights = NULL, 
                      err.fct = "sse",linear.output = FALSE,learningrate=0.001,
                      algorithm = "rprop+")

NN_model$result.matrix
NN_model$weights
head(NN_model$generalized.weights[[1]])
plot(NN_model)


#matrix.train2 <- model.matrix(~ X1stBaseDepression+X1stBondBaseForce+X1stBondUSGCurrent
#+X1stContactDepression+X1stContactLevel+X1stContactZDAC
#+X1stSearchHeight+X1stSearchPosition+X1stSearchSpeed
#+X2ndBaseDepression+X2ndContactDepression
#+X2ndContactLevel+X2ndContactZDAC+X2ndSearchHeight+X2ndSearchPosition
#+EFOCurrent+EFOTime+FireLevel+LoopBaseDistance+LoopTopDistance
#+LoopWireFeed+Moveto1stSearchHeight,data=ABT_train)


predict_testNN = compute(NN_model, ABT_train[,3:24])

postResample(pred = predict_testNN$net.result, obs = ABT_train$OutPut)
predict_trainNN4$net.result
postResample(pred = predict_trainNN4$net.result, obs = train_total_ball$OutPut)


#-----------------------------------------------------------------------

test_results <- compute(NN_model, test_total_ball[,3:28])

actual_results <- test_total_ball$OutPut

pr.nn_ <- test_results$net.result *(max(total_ballshear$OutPut)-min(total_ballshear$OutPut))+min(total_ballshear$OutPut)
test.r <- (test_total_ball$OutPut)*(max(total_ballshear$OutPut)-min(total_ballshear$OutPut))+min(total_ballshear$OutPut)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_total_ball)
MSE.nn
NNRmse <- sqrt(MSE.nn)
NNRmse

plot(test_total_ball$OutPut,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')



