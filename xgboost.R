#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(FactoMineR)
library(xgboost)

#Chargement des données-------------------------------------------------------------
test <- read.csv('data/test.csv')
train.data <- read.csv('data/train.csv')

#Présentation des données----------------------------------------------------------
str(train.data)
head(train.data)
dim(train.data)
summary(train.data)

#Lavage des données-----------------------------------------------------------------

remove.cols <- nearZeroVar(train.data[,-1], names = T)

all.cols <- names(train.data)
train.reduc <- train.data[,setdiff(all.cols, remove.cols)]

#Testé sur les data réduite et sur le full dataset
#Le meilleur résultat a été obtenu sur le dataset entier
#train <- train.reduc
train <- train.data

#Division outcomename et predictornames----------------------------------------------------------

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitage des données----------------------------------------------------------
set.seed(1234)
train$spl = sample.split(train[,1], SplitRatio = 0.85)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

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

test.mat <- data.matrix(test)

predictions <- predict(xgb.model, test.mat)
predictions
res <- cbind('MoleculeId'=1:length(predictions), "PredictedProbability"=predictions)
write.csv(res, file="submissionXGBoostNew.csv" , row.names = F)
