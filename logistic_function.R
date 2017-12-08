#Chargement des librairies----------------------------------------------------------
library(caret)
library(caTools)
library(e1071)
library(FactoMineR)
library(randomForest)
library(Rcmdr)

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

#
fitControl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 1,
  savePredictions = 'final',
  classProbs = T)

training[,outcomeName]
training[,outcomeName] <- as.factor(training[,outcomeName])
#Construction du modèle de prediction : regression logistique ----------------------------------------------------

#Methode glm en mode train 
glm.model <- glm(training[,outcomeName] ~ ., data = training[,predictorNames],family = binomial(link = logit))
summary(glm.model)
res.glm <- stepwise(glm.model,direction="backward",criterion="AIC" )
glm.model <- train(training[,predictorNames],training[,outcomeName],method='glm',trControl=fitControl,tuneLength=1, verbose = FALSE)
glm.model


#glm en mode train sur validation-------------------------------------------------------
valid.predglm <- predict(glm.model,validation[,predictorNames],type = "prob")
valid.predglm

