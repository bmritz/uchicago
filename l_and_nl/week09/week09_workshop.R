library(lme4)

Marketing.Probabilities.Data<-data.frame(Age.20.30=c(.7,.55,.1,.2),Age.30.40=c(.5,.4,.1,.2),Age.40.50=c(.3,.2,.07,.09))
rownames(Marketing.Probabilities.Data)<-c("F.mean","M.mean","F.sd","M.sd")
Marketing.Probabilities.Data

#mean of females and mean of males for 3 different sub groups--also std deviations

library(knitr)

# need 3 cols -- gender group age group and "probabilities"
kable(Marketing.Probabilities.Data,format="markdown")

# simulate the data -- 100 individuals male and female in each age group
Marketing.Probabilities <- data.frame(age=factor(c(rep("Age.20.30",200),rep("Age.30.40", 200),rep("Age.40.50", 200))),
           gender=factor(c(rep("M",100), rep("F", 100), rep("M",100), rep("F", 100), rep("M",100), rep("F",100))),
           probability=c((rnorm(100, .55, .2)),
                         (rnorm(100, .70, .1)),
                         (rnorm(100, .40, .2)),
                         (rnorm(100, .50, .1)),
                         (rnorm(100, .20, .09)),
                         (rnorm(100, .30, .07))))
head(Marketing.Probabilities)
head(cars)

# aggregate up

Mean.Sd.Table <-aggregate(Marketing.Probabilities$probability,
                        list(Marketing.Probabilities$age, Marketing.Probabilities$gender),
                        FUN=function(x) c(m=mean(x), sd= sd(x)))

kable(Mean.Sd.Table)

# fit the linear model then use anova

summary(Model.by.Age <- lm(probability~age, Marketing.Probabilities))
anova(Model.by.Age)

# p value is small so we reject hypothesis that the groups are the same

summary(Model.by.Age.Gender<- lm())

library(lme4)

?lmer
summary(random.effects.age<-lmer(probability~(1|age), data=Marketing.Probabilities))

# fixed effect is the grand mean -- the constant
# random effect --- interested in the variance of the random effect
# shows residual after the variance is absorbed by age is 0.02439 --comparable to residual sd from other models

# compare to residual standard error of lm model above


summary(lmer.by.age.gender <- lmer(probability~(1|age)+(1|gender),data=Marketing.Probabilities))
# still doing separate groupings
# just gender groupings take both groups, and divide them by gender only, or age only, 
#(1|gender) all data, split by gender -- 

#now we move on to interaction between
  
summary(Lmer.by.Age.Over.Gender<-lmer(probability~(1|age/gender),data=Marketing.Probabilities))
# six different means that will be random
#run aic to determine which model is better
# variance characterizes the random effect
# alpha hcanges for each group of ages
# 
AIC(Lmer.by.Age.Over.Gender)




# known gaussian distribution

# same logistic regression -- with additional random effects

library(faraway)
head(ctsib)

data(ctsib)
ctsib$stable <- ifelse(ctsib$CTSIB==1, 1,0)
summary(ctsib)

library(MASS)
gf<-glm(stable~Sex+Age+Height+Weight+Surface+Vision,family=binomial,data=ctsib)
summary(gf)


# add a fixed subject effect
# subject is the fixed effect
gfs<-glm(stable~Sex+Age+Height+Weight+Surface+Vision+factor(Subject),family=binomial,data=ctsib)
anova(gf, gfs, test="Chi")

# res dev is samller for second model -- probability of the equivalence of hte two models is rejected

summary(gg<-glmmPQL(stable~Sex+Age+Height+Weight+Surface+Vision,random=~1|Subject,family=binomial,data=ctsib))
plot(predict(gg,type="response"))
head(cbind(ctsib$stable,predict(gg,type="response")))
