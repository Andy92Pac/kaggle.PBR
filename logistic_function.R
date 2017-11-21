
#Dans un premier temps on fait une regression logistique binomiale
#Binomiale car notre variable à expliquer est binaire 0 ou 1
train_reg <- glm(y ~.,family=binomial(link='logit'),data=x)
summary(train_reg)

#J'expliquerai mieux demain jsuis fatigué morray mais le mode backward
#Permet d'eliminer au fur est a mesure les variable non influente dans la prediction de notre variable à expliquer
train_reg_back <- step(train_reg,direction="backward",trace=FALSE)
summary(train_reg_back)
