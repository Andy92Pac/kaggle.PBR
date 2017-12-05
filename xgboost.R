#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(FactoMineR)
library(xgboost)

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
train$spl = sample.split(train[,1], SplitRatio = 0.85)
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

cv.data <- xgb.train(data = training.Dmat
                     ,nrounds = 100
                     ,eta = 0.2
                     ,max_depth = 6
                     ,watchlist = watchlist
                     ,eval_metric = "logloss"
                     ,objective = "binary:logistic")

plot(cv.data$evaluation_log$train_logloss, type = 'l')
lines(cv.data$evaluation_log$valid_logloss)
segments(min.logloss.index, 0, y1 = min.logloss)

min.logloss <- min(cv.data$evaluation_log$valid_logloss)
min.logloss

min.logloss.index <- which.min(cv.data$evaluation_log$valid_logloss)
min.logloss.index

opti.nrounds = min.logloss.index

train.Dmat <- xgb.DMatrix(train.data.mat[,predictorNames], label=train.data.mat[,outcomeName])

xgb.model <- xgb.train(data = train.Dmat
                       ,nrounds = opti.nrounds
                       ,eta = 0.2
                       ,max_depth = 6
                       ,eval_metric = "logloss"
                       ,objective = "binary:logistic")

test.mat <- data.matrix(test)

predictions <- predict(xgb.model, test.mat)
predictions
res <- cbind('MoleculeId'=1:length(predictions), "PredictedProbability"=predictions)
write.csv(res, file="submissionXGBoostFullTrain85.csv" , row.names = F)

