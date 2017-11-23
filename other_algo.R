
#Dans un premier temps on fait une regression logistique binomiale
#Binomiale car notre variable à expliquer est binaire 0 ou 1
training_reg <- glm(train_vov[,1] ~.,family=binomial(link='logit'),data=train_vov[,-1])
summary(training_reg)
training_reg$coefficients
v <- c("D5","D3")
typeof(v)
c<-names(training_reg_back$coefficients)
c
is.vector(c)
p <- train_vov[,c[-1]]
#J'expliquerai mieux demain jsuis fatigué morray mais le mode backward
#Permet d'eliminer au fur est a mesure les variable non influente dans la prediction de notre variable à expliquer
training_reg_back <- step(training_reg,direction="backward",trace=FALSE)
summary(training_reg_back)

p
