setwd('/Volumes/MacintoshHD/Users/Andy/Desktop/Kaggle')

#Chargement des librairies
library(caret)
library(caTools)
library(e1071)

#Chargement des données
test <- read.csv('data/test.csv')
train.data <- read.csv('data/train.csv')

#Présentation des données
str(train)
head(train)
dim(train)
summary(train)


#Lavage des données
x <- train.data[,predictorNames]
remove.cols <- nearZeroVar(x, names = T)

all.cols <- names(train.data)
train.reduc <- train.data[,setdiff(all.cols, remove.cols)]

train <- train.reduc

outcomeName <- names(train)[1]
predictorNames <- setdiff(names(train), outcomeName)

#Splitage des données
train$spl = sample.split(train[,1], SplitRatio = 0.7)
training = train[train$spl==1,]
validation = train[train$spl==0,]

training <- subset(training, select = -c(spl))
validation <- subset(validation, select = -c(spl))


#Analyse factorielle
PCA(training[,-1])



#Modèle svm
svm.model <- svm(training[,predictorNames],training[,outcomeName], probability = T)
valid.pred <- predict(svm.model, validation[,predictorNames])
valid.compar <- matrix(c(validation[,1], valid.pred), byrow = F, ncol = 2)
valid.compar

test.pred <- predict(svm.model, test[,predictorNames])
test.pred
