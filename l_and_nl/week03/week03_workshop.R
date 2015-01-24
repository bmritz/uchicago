set.seed(893075)
Variable.X<-rnorm(500,0,1)
Variable.Y<-Variable.X^2
plot(Variable.X,Variable.Y)


cor(Variable.X,Variable.Y)

Variable.X<-rnorm(100,1,1)
Variable.Y<-rnorm(100,1/1.5,1/1.5^2)
Variable.Z<-rexp(100,1.5)

plot(Variable.X,Variable.Y,ylim=c(-1,4))

plot(Variable.X,Variable.Z,ylab="Variable.Y",ylim=c(-1,4))


#empirical copula -- uniform
plot(rank(Variable.X)/100,rank(Variable.Y)/100)
plot(rank(Variable.X)/100,rank(Variable.Z)/100)

# copulas 

library(copula)
par(mfrow=c(2,2))
set.seed(8301735)
#Gaussian Copula, rho=0.9
Gaussian.Copula.0.9<-normalCopula(param=.9,dim=2)
persp(Gaussian.Copula.0.9, dCopula, main="pdf",xlab="u", ylab="v", zlab="c(u,v)")
contour(Gaussian.Copula.0.9,dCopula, main="pdf",xlab="u", ylab="v")
Simulated.Gaussian.Copula.0.9<-rCopula(100,Gaussian.Copula.0.9)
plot(Simulated.Gaussian.Copula.0.9,main="Simulated Copula",xlab="Variable 1",ylab="Variable 2")
plot(apply(Simulated.Gaussian.Copula.0.9,2,rank)/length(Simulated.Gaussian.Copula.0.9[,1]),main="Empirical Copula",xlab="Variable 1",ylab="Variable 2")
title(main="Gaussian Copula, rho=0.9",outer=TRUE,line=-2)



# Simulation of dependent random variables using copulas.
par(mfrow=c(2,2))
matplot(1:100,Simulated.Gaussian.Copula.0.9[,1],pch=19,main="Simulated Uniform Variable",xlab="Count",ylab="Simulated Variable")
matplot(1:100,qnorm(Simulated.Gaussian.Copula.0.9[,1]),pch=19,main="Simulated Normal Variable",xlab="Count",ylab="Simulated Variable")
hist(Simulated.Gaussian.Copula.0.9[,2],main="Histogram of Simulated Variable",xlab="Simulated Variable",ylab="Frequency")
hist(qnorm(Simulated.Gaussian.Copula.0.9[,1]),main="Histogram of qnorm(Simulated Variable)",xlab="Simulated Variable",ylab="Frequency")
 

par(mfrow=c(1,2))
plot(Simulated.Gaussian.Copula.0.9[,1],Simulated.Gaussian.Copula.0.9[,2],xlim=c(0,1),ylim=c(0,1))
plot(rank(qnorm(Simulated.Gaussian.Copula.0.9[,1]))/100,rank(qnorm(Simulated.Gaussian.Copula.0.9[,2]))/100,xlim=c(0,1),ylim=c(0,1))


# fit a copula
Sample.For.Fitting.Gaussian.Copula<-cbind(qnorm(Simulated.Gaussian.Copula.0.9[,1]),
                                          qnorm(Simulated.Gaussian.Copula.0.9[,2],3,5))    
cor(Sample.For.Fitting.Gaussian.Copula)

# doesnt matter what the param is here -- because we will fit it
Gaussian.Copula.Object<-normalCopula(param=0,dim=2)
Gaussian.Copula.fit<-fitCopula(Gaussian.Copula.Object, 
                               pobs(Sample.For.Fitting.Gaussian.Copula,ties.method = "average"), 
                               method = "ml",
                               optim.method = "BFGS", 
                               optim.control = list(maxit=1000))
Gaussian.Copula.fit  # rho.1 will be the correlation coefficient because we have a normalCopula
