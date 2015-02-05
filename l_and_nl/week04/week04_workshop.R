library(faraway)

data(babyfood)
xtabs(disease/(disease+nondisease)~sex+food,babyfood)

mdl<-glm(cbind(disease,nondisease)~sex+food,family=binomial,babyfood)
summary(mdl)

drop1(mdl,test="Chi")
'exp(-.669)'