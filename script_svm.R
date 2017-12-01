#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(e1071)
library(FactoMineR)
library(randomForest)

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

train <- train.reduc

#Division outcomename et predictornames----------------------------------------------------------

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitage des données----------------------------------------------------------

train$spl = sample.split(train[,1], SplitRatio = 0.7)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))

#Analyse factorielle----------------------------------------------------------
PCA(training[,-1])

training[,outcomeName] <- ifelse(training[,outcomeName]==1,'yes','nope')

#Construction du modèle de prediction SVM--------------------------------------
svm.model <- svm(training[,outcomeName]~. , data = training[,predictorNames],type="C-classification",probability = TRUE)

summary(svm.model)

#SVM sur validation---------------------------------------------------------
valid.pred <- predict(svm.model, validation[,predictorNames],probability = TRUE)
valid.pred <- attr(valid.pred,"probabilities")
valid.compar <- matrix(c(validation[,1], valid.pred[,1]), byrow = F, ncol = 2)
valid.compar

#SVM sur données TEST------------------------------------------------------
test.pred <- predict(svm.model, test[,predictorNames], probability = TRUE)
test.pred <- attr(test.pred,"probabilities")
test.compar <- matrix(test.pred[,1], byrow = F, ncol = 1)
colnames(test.compar)=c("PredictedProbability")
res <- cbind('MoleculeId'=1:length(test.compar), test.compar)
write.csv(res, file="submission.csv" , row.names = F)
