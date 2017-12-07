#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(FactoMineR)
library(xgboost)
library(e1071)
library(randomForest)

#Chargement des données-------------------------------------------------------------
test <- read.csv('data/test.csv')
train.data <- read.csv('data/train.csv')

train <- train.data

#Variabilisation des classes--------------------------------------------------------
outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitting des donnees--------------------------------------------------------------
set.seed(1234)
train$spl = sample.split(train[,1], SplitRatio = 0.85)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

#Données mis dans une matrix pour le traitement xgb---------------------------------
training <- data.matrix(training)
validation <- data.matrix(validation)
training.Dmat <- xgb.DMatrix(training[,predictorNames], label=training[,outcomeName])
valid.Dmat <- xgb.DMatrix(validation[,predictorNames], label=validation[,outcomeName])

#Utilisation des matrix créer dans la watchlist qui va ns permettre de créer un modele pour chaque matrix donnée en param
watchlist <- list(train = training.Dmat, valid =  valid.Dmat)

#Il va ns sortir une liste de 100 modele avec des logloss decroissant en training et aleatoire en validation
cv.data <- xgb.train(data = training.Dmat
                     ,nrounds = 100 #Nb de fois qu'il va 
                     ,eta = 0.2 #Par défaut 0.3 évite le sur apprentissage
                     ,max_depth = 6 #Par defaut à 6 hyper param
                     ,watchlist = watchlist #la watchlist
                     ,eval_metric = "logloss" #logloss
                     ,objective = "binary:logistic")

#On récupere le logloss le moins élevé de validation
min.logloss <- min(cv.data$evaluation_log$valid_logloss)
cv.data$evaluation_log$
min.logloss
#On recupere le nb de fois qu'il à du tourner pour avoir le logloss optimisé
min.logloss.index <- which.min(cv.data$evaluation_log$valid_logloss)
min.logloss.index

#On stock le nb de fois pour avoir le logloss optimisé
opti.nrounds = min.logloss.index

#On relance avec le train logloss optimisé
xgb.model <- xgb.train(data = training.Dmat
                       ,nrounds = opti.nrounds
                       ,eta = 0.2
                       ,max_depth = 6
                       ,eval_metric = "logloss"
                       ,objective = "binary:logistic")

#Prediction sur test qu'on converti en matrix 
pred.xgb <- predict(xgb.model, as.matrix(test))
pred.xgb 
#Partie Random Forest ----------------------------------------------------------------------

#Reduction des dimensions 
remove.cols <- nearZeroVar(train.data[,-1], names = T)

all.cols <- names(train.data)
train.reduc <- train.data[,setdiff(all.cols, remove.cols)]

train <- train.reduc

#Variabilisation des classes
outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitage des données 
train$spl = sample.split(train[,1], SplitRatio = 0.85)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

#Factorisation de la colonne outcomena pour pas que l'algo le prenne pour une variable discrete
training[,outcomeName] <- as.factor(training[,outcomeName])

#Modele random forest proba 
rf.modelProba <- randomForest(x = training[,predictorNames], y = training[,outcomeName]
                              ,ytest = as.factor(training[,outcomeName]),xtest = training[,predictorNames]
                              ,keep.forest=TRUE #Utile qd on utilise predict avec rf
)
#Prediction du random forest sur validation
valid.predProbaRF <- predict(rf.modelProba,validation[,predictorNames],type = "prob")

#Prediction du rf sur données tEST
pred.rf <- predict(rf.modelProba, test[,predictorNames], type = "prob")
pred.rf[,2]

#Ensemble du rf et xgb
pred.ens <- (pred.xgb+pred.rf[,2])/2
pred.ens
#Submission
submission <- cbind('MoleculeId'=1:length(pred.ens), "PredictedProbability"=pred.ens)
write.csv(submission, file="submissionEnsembleRF_XGB.csv" , row.names = F)
