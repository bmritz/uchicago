
library(animation)
## set some options first
oopt = ani.options(interval = .5, nmax = 25)
# Create Transition Probabilities Matix
MC.Matrix<-data.frame(rbind(From.Pos1=c(To.Pos1=0.0,To.Pos2=0.9,To.Pos3=0,To.Pos4=.1),
                            From.Pos2=c(To.Pos1=.1,To.Pos2=0,To.Pos3=.9,To.Pos4=0),
                            From.Pos3=c(To.Pos1=0,To.Pos2=.1,To.Pos3=0,To.Pos4=.9),
                            From.Pos4=c(To.Pos1=0.9,To.Pos2=0,To.Pos3=0.1,To.Pos4=0.01)))
Current.State<-2
From.To<-rep(Current.State,2)

# use a loop to create images one by one

my.Data<-as.matrix(t(c(0,2)))
for (i in 1:ani.options("nmax")) {
 From.To[1]<-Current.State
 #    From.To<-c(1,1)
 From.To[2]<-sample(1:4,1,prob=MC.Matrix[From.To[1],])
 Current.State<-From.To[2]
  
 #wait.next<-rexp(1)
 my.Data<-rbind(my.Data,c(my.Data[length(my.Data[,1]),1]+1,From.To[2]))
 plot(my.Data,type="s",xlim=c(1,25),ylim=c(0,4),xlab="Time",ylab="State",lwd=3)
 abline(h=1,col="red")
 abline(h=4,col="red")
 ani.pause(wait.next) 
 ## pause for a while ('interval')
}
# restore the options
ani.options(oopt)


library(depmixS4)
data(speed)
head(speed)
plot(speed$rt,type="l",lty=1)

sp1 <- data.frame(speed[1:168,])
names(sp1) <- c("RT", "ACC","Pacc")
hist(speed$rt)
plot(speed$Pacc)

m1 <- mix(RT~1,nstates=1, data=sp1)
fm1 <- fit(m1)

fm1
summary(fm1)

set.seed(1)
# Create the object with 2 states
m2 <- mix(RT~1,nstates=2, data=sp1,
          respstart=c(rnorm(1,5),1,rnorm(1,6),1))
fm2 <- fit(m2,emcontrol=em.control(rand=F))

summary(fm2)
fm2
library(nor1mix)
Two.State.mix<-norMix(mu=c(5.475281,6.313695),sigma=c(0.1255941,0.3192222),w=c(0.3315286,0.6684714))
plot(Two.State.mix)


# one state model for both variables
m1p <- depmix(list(ACC~Pacc,RT~Pacc), nstates=1,
              data=sp1, family=list(multinomial(),gaussian()))
fm1p <- fit(m1p)
fm1p
summary(fm1p)

set.seed(1)
m2 <- depmix(list(RT~1,ACC~1),nstates=2, data=sp1,
             family=list(gaussian(),multinomial()))
fm2 <- fit(m2)
fm2
summary(fm2)


set.seed(1)
m2a <- depmix(list(RT~1,ACC~1),nstates=2, data=sp1,
              family=list(gaussian(),multinomial()),
              transition=~Pacc)
fm2a <- fit(m2a)
summary(fm2a)


set.seed(1)
m3 <- depmix(list(RT~1,ACC~1),nstates=3, data=sp1,
             family=list(gaussian(),multinomial()))
fm3 <- fit(m3)



# self-organizing maps
library(kohonen)
AssignmentData<-
  read.csv(file="../../statistical_analysis/hw/lecture4/regressionassignmentdata2014.csv",
           header=TRUE,sep=",")
AssignmentData[1:10,2:8]