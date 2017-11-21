#setwd('/Volumes/MacintoshHD/Users/Andy/Desktop/Kaggle')
install.packages("caTools")
install.packages("e1071")
library(caTools)
#Chargement des données
svm_benchmark <- read.csv('data/svm_benchmark.csv')
train <- read.csv('data/train.csv')

#Tentative d'un SVM classique 

#Reduction du jeu de données pour le tps de chargement etc
train_vov <- train[1:80,1:81]

# x on met tte les données sauf la var activity ensemble des variable indépendantes donc explicative du phénomène qu'on veut expliquer
x <- subset(train_vov, select = -train_vov$Activity)
# y isole la colonne activity car c'est la variable à explique donc à prédire
y <- train_vov$Activity

# Y en fonction du reste des data de train_vov , type de classification
svm_model_basic <- svm(y ~ ., data=train_vov, type="C-classification")
summary(svm_basic)

#predict() fonction tu lui donnes un modele d'analyse il fait les attribution
# obs et variable à prédire
predict <- predict(svm_model_basic)
actuel <- y
table(predict,actuel)
# Résultat on obtient une matrie de confusion lignes = valeurs préditent
#colonnes = valeurs actuel des classes 
#En gros le modèle prédit qu'il y'a 24 0 et 56 1 
#Pour de vrai dans mon dataset il y a 34 0 et 46 1 donc différence de 10 pas mal dans le sens ou il n y a pas bcp de données



#Tentative d'un SVM plus poussé qui nous permet de trouver le meilleure cost et gamma

#tune est une fonction permettant d'améliorer une méthode en gros de rajouter d'autres paramètres
#Mais surtt cette focntion va ns aider à trouver les cost et gamma les plus performant pour notre modèle
#cost : https://www.youtube.com/watch?v=joTa_FeMZ2s
# gamma : https://www.youtube.com/watch?v=m2a2K4lprQw bien expliqué 
# kernel radial est celui par défaut sa depend de tn dataset pour le notre il va bien
svm_tune <- tune(svm, train.x = x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

summary(svm_tune)
#Donc la on voit qu'il faudrait refaire notre svm avec cost = 1 et gamma = 2 
# Par contre j'ai remarqué que dans mon cas y a que la valeur gamma qui importe crari en cost tpeu mettre nptk sa va marcher jai surement rater un épisode mdr

#On refait un svm avec les cost et gamma optimisé
svm_model_after_tune <- svm(y ~ ., data=train_vov, type="C-classification", kernel="radial",cost=1,gamma=2)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune)
actuel <- y
#La le bail est mignon de fou car il a bien prédit on à bien l'actuel qui est égale au prédictif
table(pred,actuel)

