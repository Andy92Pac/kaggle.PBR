setwd('/Volumes/MacintoshHD/Users/Andy/Desktop/Kaggle')

install.packages("caret")
install.packages("glmnet")

#Chargement des donnÃ©es
svm_benchmark <- read.csv('data/svm_benchmark.csv')
train <- read.csv('data/train.csv')

head(train)
summary(train)

library(caret)

#iL va reperer les variables qui ont peu de valeurs et va les supprimer
x <- train
remove.cols <- nearZeroVar(x, names = T)

all.cols <- names(x)
train.reduc <- train[,setdiff(all.cols, remove.cols)]


#SÃ©paration en 2 dataset training et validation
library(caTools)
set.seed(123)

#â¥Commentaire pour push
train.reduc$spl = sample.split(train.reduc[,1], SplitRatio = 0.7)
training = train.reduc[train.reduc$spl==1,]
validation = train.reduc[train.reduc$spl==0,]

head(train.reduc,n=1)
str(train.reduc)


#SVM
library(e1071)
svm.model <- svm(training[,-1],training[,1], probability = T)
valid.pred <- predict(svm.model, validation[,-1])
valid.compar <- matrix(c(validation[,1], valid.pred), byrow = F, ncol = 2)

#1er ACP completement illisible normal don't panic i love this game
library(FactoMineR)
#On observe 2 masse d'invidus la masse du bas ceux dont activity est à 0
#Masse du haut dont activity est à 1 
Acp.model <- CA(training[,!colnames(training)=="spl"])
Acp.model$row$contrib
Acp.model$call$marge.row

#Methode lasso
library(glmnet)
  train.lasso <- cv.glmnet(x = as.matrix(training[,-1]),y = training[,1],family="binomial",alpha=0, parallel = T, standardize=T, type.measure = 'auc')
 summary(train.lasso)
 train.lasso

#On se retrouve avec 985 variable on retente un bail de regression linéaire
training_reg <- glm(training[,1] ~ .,family=binomial(link='logit'),training[,-1])
summary(training_reg)

training_reg_back <- step(training_reg,direction="backward",trace=F)
summary(training_reg_back)

#Algo du K nearest Neighbors
#Je tatone pour trouver le k le plus optimal
library(class)
biological.knn <- knn(train=training[,-1], test=validation[,-1],prob=T, cl = training[,1], k = 1)
biological.knn <- knn(train=training[,-1], test=validation[,-1],prob=T, cl = training[,1], k = 5)
biological.knn <- knn(train=training[,-1], test=validation[,-1],prob=T, cl = training[,1], k = 20)
biological.knn
