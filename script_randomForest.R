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

#
fitControl <- trainControl(
method = "cv",
number = 5,
savePredictions = 'final',
classProbs = T)

#Construction du modèle de prediction : Random Forest ----------------------------------------------------

#cette méthode vise à corriger les inconvénients de la méthode arbres de décisions
#Pour que rf comprennent qu'on veut faire une classification et non une regression
#Il faut convertir en factor notre variable expliqué
#Selon la doc de rf si on uilise le param formula on obtient que des valeurs donc une matrice de confusion
#Or que si on utilise justement ces paramètres (x,y,xtest,ytest) on obtient des proba
rf.modelProba <- randomForest(x = training[,predictorNames], y = as.factor(training[,outcomeName])
                         ,ytest = as.factor(training[,outcomeName]),xtest = training[,predictorNames]
                         ,keep.forest=TRUE #Utile qd on utilise predict avec rf
                         )

#Mais il est qd mm important d'avoir le resultat en tant que matrice de confusion
#Pour ce rendre compte de la marge d'erreur et comprendre pk il y a des fois des 
#proba élévé pour la classe 0 

rf.model <- randomForest(y = as.factor(training[,outcomeName])
                         ,x = training[,predictorNames]
                        ,keep.forest = T,
                        do.trace = T)

#Methode du knn ------------------------------------------------------------------------
training[,outcomeName]
training[,outcomeName] <- ifelse(training[,outcomeName]==1,'yes','nope')
library(class)
biological.knn <-train(training[,predictorNames],training[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)
biological.knn

#Methode glm
glm.model<-train(training[,predictorNames],training[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)
glm.model
#RF sur validation-------------------------------------------------------------

#RF proba
valid.predProbaRF <- predict(rf.modelProba,validation[,predictorNames],type = "prob")
valid.predProbaRF <- as.data.frame(valid.predProbaRF)
valid.comparRF <-matrix(c(validation[,1], valid.predProbaRF[,2]), byrow = F, ncol = 2)

#RF confusion matrix
predict <- predict(rf.model,validation[,predictorNames])
actuel <- validation[,outcomeName]
matrix.confusion <- table(predict,actuel)
matrix.confusion

#Knn sur validation-------------------------------------------------------

#Knn proba
valid.predknn <- predict(biological.knn,validation[,predictorNames],type = "prob")
valid.predknn

#Knn matrix confusion
predict.knn <- predict(biological.knn,validation[,predictorNames])
actuel <- validation[,outcomeName]
matrix.confusion_knn <- table(predict,actuel)
matrix.confusion_knn

#glm sur validation-------------------------------------------------------
valid.predglm <- predict(glm.model,validation[,predictorNames],type = "prob")
valid.predglm


#RF sur données TEST-------------------------------------------------------

#RF proba
test.predProbaRF <- predict(rf.modelProba, test[,predictorNames], type="prob")
test.predProbaRF <- as.data.frame(test.predProbaRF)
test.compar <- matrix(test.predProbaRF[,2], byrow = F, ncol = 1)
colnames(test.compar)=c("Classe 1")
