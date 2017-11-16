setwd('/Volumes/MacintoshHD/Users/Andy/Desktop/Kaggle')

#Chargement des données
svm_benchmark <- read.csv('data/svm_benchmark.csv')
train <- read.csv('data/train.csv')

#Séparation en 2 dataset training et validation
library(caTools)

set.seed(123)

#♥Commentaire pour push
train$spl = sample.split(train[,1], SplitRatio = 0.7)
training = train[train$spl==1,]
validation = train[train$spl==0,]

library(e1071)
svm.model <- svm(training)
