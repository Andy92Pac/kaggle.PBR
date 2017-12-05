#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(FactoMineR)
library(xgboost)
library(tidyverse)

#Chargement des données-------------------------------------------------------------
test <- read.csv('data/test.csv')
train.data <- read.csv('data/train.csv')



#Lavage des données-----------------------------------------------------------------
remove.cols <- nearZeroVar(train.data[,-1], names = T)

all.cols <- names(train.data)
train.reduc <- train.data[,setdiff(all.cols, remove.cols)]

#Testé sur les data réduite et sur le full dataset
#train <- train.reduc
train <- train.data

#Division outcomename et predictornames----------------------------------------------------------

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitage des données----------------------------------------------------------
set.seed(12345)
train$spl = sample.split(train[,1], SplitRatio = 0.80)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

#Transformation 
train.data.mat <- data.matrix(train.data)
training <- data.matrix(training)
validation <- data.matrix(validation)

#xgboost basique
xgb.model <- xgboost(data = training[,predictorNames], label = training[,outcomeName], nrounds = 2)

#xgboost avancé avec cross validation
training.Dmat <- xgb.DMatrix(training[,predictorNames], label=training[,outcomeName])
valid.Dmat <- xgb.DMatrix(validation[,predictorNames], label=validation[,outcomeName])

watchlist <- list(train = training.Dmat, valid =  valid.Dmat)

#gris search
gs <- list(eta = c(0.1,0.3,0.5)
           ,gamma = c(0, 0.1, 0.5)
           ,max_depth = c(3, 6, 10)
           ,max_delta_step = c(0, 2)
)
gs.df <- expand.grid(gs)
gs.df <- cbind(gs.df, 1:nrow(gs.df))

res.gs <- apply(gs.df, 1, function(row) {
  print(row[5])
  cv.data <- 0;
  cv.data <- xgb.train(data = training.Dmat
                       ,nrounds = 100
                       ,eta = row[1]
                       ,gamma = row[2]
                       ,max_depth = row[3]
                       ,max_delta_step = row[4]
                       ,watchlist = watchlist
                       ,eval_metric = "logloss"
                       ,objective = "binary:logistic")
  min.val <- min(cv.data$evaluation_log$valid_logloss)
  min.index <- which.min(cv.data$evaluation_log$valid_logloss)
  c(min.val, min.index)
})

res.gs.t <- t(res.gs)
gs.df <- cbind(gs.df, res.gs.t)

min.logloss.gs <- min(gs.df$`1`)
min.logloss.gs
min.logloss.gs.index <- which.min(gs.df$`1`)
min.logloss.gs.index
min.logloss.gs.nrounds <- gs.df$`2`[10]
min.logloss.gs.nrounds

cv.data <- xgb.train(data = training.Dmat
                     ,nrounds = 35
                     ,eta = gs.df[19,1]
                     ,gamma = gs.df[19,2]
                     ,max_depth = gs.df[19,3]
                     ,max_delta_step = gs.df[19,4]
                     ,watchlist = watchlist
                     ,eval_metric = "logloss"
                     ,objective = "binary:logistic")


#plot(cv.data$evaluation_log$train_logloss, type = 'l')
#lines(cv.data$evaluation_log$valid_logloss)
#segments(min.logloss.index, 0, y1 = min.logloss)

#min.logloss.cv <- min(cv.data$evaluation_log$valid_logloss)
#min.logloss.cv

#min.logloss.index <- which.min(cv.data$evaluation_log$valid_logloss)
#min.logloss.index

#opti.nrounds = min.logloss.index

#train.Dmat <- xgb.DMatrix(train.data.mat[,predictorNames], label=train.data.mat[,outcomeName])

#xgb.model <- xgb.train(data = train.Dmat
#                     ,nrounds = opti.nrounds
#                     ,eta = 0.2
#                     ,max_depth = 6
#                     ,eval_metric = "logloss"
#                     ,objective = "binary:logistic")

xgb.model <- cv.data

test.mat <- data.matrix(test)

predictions <- predict(xgb.model, test.mat)
predictions
res <- cbind('MoleculeId'=1:length(predictions), "PredictedProbability"=predictions)
write.csv(res, file="submissionXGBoostGS.csv" , row.names = F)

