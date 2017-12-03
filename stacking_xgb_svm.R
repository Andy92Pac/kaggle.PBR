#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(FactoMineR)
library(xgboost)
library(e1071)

#Chargement des données-------------------------------------------------------------
test <- read.csv('data/test.csv')
train.data <- read.csv('data/train.csv')

train <- train.data

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

set.seed(1234)
train$spl = sample.split(train[,1], SplitRatio = 0.85)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

training <- data.matrix(training)
validation <- data.matrix(validation)

training.Dmat <- xgb.DMatrix(training[,predictorNames], label=training[,outcomeName])
valid.Dmat <- xgb.DMatrix(validation[,predictorNames], label=validation[,outcomeName])

watchlist <- list(train = training.Dmat, valid =  valid.Dmat)

cv.data <- xgb.train(data = training.Dmat
                     ,nrounds = 100
                     ,eta = 0.2
                     ,max_depth = 6
                     ,watchlist = watchlist
                     ,eval_metric = "logloss"
                     ,objective = "binary:logistic")

min.logloss <- min(cv.data$evaluation_log$valid_logloss)
min.logloss

min.logloss.index <- which.min(cv.data$evaluation_log$valid_logloss)
min.logloss.index

opti.nrounds = min.logloss.index

xgb.model <- xgb.train(data = training.Dmat
                       ,nrounds = opti.nrounds
                       ,eta = 0.2
                       ,max_depth = 6
                       ,eval_metric = "logloss"
                       ,objective = "binary:logistic")

pred.xgb <- predict(xgb.model, as.matrix(test))

#SVM
remove.cols <- nearZeroVar(train.data[,-1], names = T)

all.cols <- names(train.data)
train.reduc <- train.data[,setdiff(all.cols, remove.cols)]

train <- train.reduc

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

train$spl = sample.split(train[,1], SplitRatio = 0.85)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

training[,outcomeName] <- ifelse(training[,outcomeName]==1,'yes','nope')

svm.model <- svm(training[,outcomeName]~. , data = training[,predictorNames],type="C-classification",probability = TRUE)

pred.svm <- predict(svm.model, test, probability = TRUE)
pred.svm <- attr(pred.svm, "probabilities")


#Ensemble
pred.ens <- (pred.xgb+pred.svm[,1])/2

#Submission
submission <- cbind('MoleculeId'=1:length(pred.ens), "PredictedProbability"=pred.ens)
write.csv(submission, file="submissionEnsembleSVM_XGB.csv" , row.names = F)
