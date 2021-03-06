Age.Time.Sample<-read.csv(file="Week3_Homework_Project_Data.csv",header=TRUE,sep=",")
Age.Time.Sample<-as.matrix(Age.Time.Sample)
Age.Time.Sample[1:10,]


# there looks like there might be two clusters
plot(Age.Time.Sample)


# Once you rank,the clusters disappear a little bit
plot(rank(Age.Time.Sample[,1])/length(Age.Time.Sample[,1]),rank(Age.Time.Sample[,2])/length(Age.Time.Sample[,1]))


#
c(cor(Age.Time.Sample)[1,2],cor(Age.Time.Sample)[1,2]^2)

hist(Age.Time.Sample[,1])
hist(Age.Time.Sample[,2])


library(mclust)

# use Mclust() to cluster the age component and the time component

Age.Clusters<-Mclust(data=Age.Time.Sample[,1], modelNames="V")
names(Age.Clusters)
Age.Clusters$G

Age.Clusters$parameters
Age.Clusters.Parameters<-rbind(mu=Age.Clusters$parameters$mean,
                               sigma=sqrt(Age.Clusters$parameters$variance$sigmasq),
                               pro=Age.Clusters$parameters$pro)


library(nor1mix)
Classified.Mix.Model.Age <- norMix(Age.Clusters.Parameters["mu",], 
                                   sigma=Age.Clusters.Parameters["sigma",],
                                   w=Age.Clusters.Parameters["pro",])
plot(Classified.Mix.Model.Age,xout=seq(from=10,to=60,by=.25),p.norm=TRUE,p.comp=TRUE)

# Now on to time clusters
Time.Clusters<-Mclust(data=Age.Time.Sample[,2], modelNames="V")
names(Time.Clusters)
Time.Clusters$G

Time.Clusters$parameters

# refit and force the number of clusters to be at least 2
Time.Clusters<-Mclust(data=Age.Time.Sample[,2], G=c(2:9), modelNames="V")
names(Time.Clusters)
Time.Clusters$G

Time.Clusters$parameters
Time.Clusters.Parameters<-rbind(mu=Time.Clusters$parameters$mean,
                                sigma=sqrt(Time.Clusters$parameters$variance$sigmasq),
                                pro=Time.Clusters$parameters$pro)

# make a normix object

Classified.Mix.Model.Time <- norMix(Time.Clusters.Parameters["mu",], 
                                   sigma=Time.Clusters.Parameters["sigma",],
                                   w=Time.Clusters.Parameters["pro",])

plot(Classified.Mix.Model.Time,xout=seq(from=18,to=26,by=.25),p.norm=TRUE,p.comp=TRUE)



# separate samples into clusters and explore their dependencies

#separate samples and explore dependencies
Age.Mixing.Sequence<-Age.Clusters$classification
Age.25.Time.21.Mixing.Sequence<-((Age.Clusters$classification==1)&(Time.Clusters$classification==1))
Age.25.Time.23.Mixing.Sequence<-((Age.Clusters$classification==1)&(Time.Clusters$classification==2))
Age.45.Time.21.Mixing.Sequence<-((Age.Clusters$classification==2)&(Time.Clusters$classification==1))
Age.45.Time.23.Mixing.Sequence<-((Age.Clusters$classification==2)&(Time.Clusters$classification==2))
Grouped.Data.Age.25.Time.21<-
  Grouped.Data.Age.25.Time.23<-
  Grouped.Data.Age.45.Time.21<-
  Grouped.Data.Age.45.Time.23<-
  cbind(Age=rep(NA,200),Time=rep(NA,200))
Grouped.Data.Age.25.Time.21[Age.25.Time.21.Mixing.Sequence,]<-
  Age.Time.Sample[Age.25.Time.21.Mixing.Sequence,]
Grouped.Data.Age.25.Time.23[Age.25.Time.23.Mixing.Sequence,]<-
  Age.Time.Sample[Age.25.Time.23.Mixing.Sequence,]
Grouped.Data.Age.45.Time.21[Age.45.Time.21.Mixing.Sequence,]<-
  Age.Time.Sample[Age.45.Time.21.Mixing.Sequence,]
Grouped.Data.Age.45.Time.23[Age.45.Time.23.Mixing.Sequence,]<-
  Age.Time.Sample[Age.45.Time.23.Mixing.Sequence,]
matplot(Age.Time.Sample[,1],cbind(Grouped.Data.Age.25.Time.21[,2],
                                  Grouped.Data.Age.25.Time.23[,2],
                                  Grouped.Data.Age.45.Time.21[,2],
                                  Grouped.Data.Age.45.Time.23[,2]),
        pch=16,xlab="Age",ylab="Time",
        col=c('black','red', 'blue', 'green'))
legend('topleft', c("Age.25.Time.21","Age.25.Time.23","Age.45.Time.21","Age.45.Time.23") , 
       lty=1,lwd=3, col=c('black','red', 'blue', 'green'), bty='n', cex=.75)

# Group the samples by age and by time and explore the dependencies within groups.
#Group by age
Grouped.Data.Age.25<-cbind(Age=rep(NA,200),Time=rep(NA,200))
Grouped.Data.Age.25[Age.Clusters$classification==1,]<-Age.Time.Sample[Age.Clusters$classification==1,]
Grouped.Data.Age.45<-cbind(Age=rep(NA,200),Time=rep(NA,200))
Grouped.Data.Age.45[Age.Clusters$classification==2,]<-Age.Time.Sample[Age.Clusters$classification==2,]
plot(rank(na.omit(Grouped.Data.Age.25[,1]))/length(na.omit(Grouped.Data.Age.25[,1])),
     rank(na.omit(Grouped.Data.Age.25[,2]))/length(na.omit(Grouped.Data.Age.25[,2])),
     xlab="Age 25 Group: Age",ylab="Age 25 Group: Time")

cor(na.omit(Grouped.Data.Age.25),method="spearman")[1,2]

plot(rank(na.omit(Grouped.Data.Age.45[,1]))/length(na.omit(Grouped.Data.Age.45[,1])),
     rank(na.omit(Grouped.Data.Age.45[,2]))/length(na.omit(Grouped.Data.Age.45[,2])),
     xlab="Age 45 Group: Age",ylab="Age 45 Group: Time")

cor(na.omit(Grouped.Data.Age.45),method="spearman")[1,2]

#Group by Time
Grouped.Data.Time.21<-cbind(Age=rep(NA,200),Time=rep(NA,200))
Grouped.Data.Time.21[Time.Clusters$classification==1,]<-
  Age.Time.Sample[Time.Clusters$classification==1,]
Grouped.Data.Time.23<-cbind(Age=rep(NA,200),Time=rep(NA,200))
Grouped.Data.Time.23[Time.Clusters$classification==2,]<-
  Age.Time.Sample[Time.Clusters$classification==2,]
plot(rank(na.omit(Grouped.Data.Time.21[,1]))/length(na.omit(Grouped.Data.Time.21[,1])),
     rank(na.omit(Grouped.Data.Time.21[,2]))/length(na.omit(Grouped.Data.Time.21[,2])),
     xlab="Time 21 Group: Age",ylab="Time 21 Group: Time")

cor(na.omit(Grouped.Data.Time.21),method="spearman")[1,2]

plot(rank(na.omit(Grouped.Data.Time.23[,1]))/length(na.omit(Grouped.Data.Time.23[,1])),
     rank(na.omit(Grouped.Data.Time.23[,2]))/length(na.omit(Grouped.Data.Time.23[,2])),
     xlab="Time 23 Group: Age",ylab="Time 23 Group: Time")

cor(na.omit(Grouped.Data.Time.23),method="spearman")[1,2]

#Group by Age and Time
#Grouped.Data.Age.25.Time.21
plot(rank(na.omit(Grouped.Data.Age.25.Time.21[,1]))/length(na.omit(Grouped.Data.Age.25.Time.21[,1])),
     rank(na.omit(Grouped.Data.Age.25.Time.21[,2]))/length(na.omit(Grouped.Data.Age.25.Time.21[,2])),
     xlab="Time 21 Group: Age",ylab="Time 21 Group: Time")

cor(na.omit(Grouped.Data.Age.25.Time.21),method="spearman")[1,2]

#Grouped.Data.Age.25.Time.23
plot(rank(na.omit(Grouped.Data.Age.25.Time.23[,1]))/length(na.omit(Grouped.Data.Age.25.Time.23[,1])),
     rank(na.omit(Grouped.Data.Age.25.Time.23[,2]))/length(na.omit(Grouped.Data.Age.25.Time.23[,2])),
     xlab="Time 21 Group: Age",ylab="Time 21 Group: Time")

cor(na.omit(Grouped.Data.Age.25.Time.23),method="spearman")[1,2]



#Grouped.Data.Age.45.Time.21
plot(rank(na.omit(Grouped.Data.Age.45.Time.21[,1]))/length(na.omit(Grouped.Data.Age.45.Time.21[,1])),
     rank(na.omit(Grouped.Data.Age.45.Time.21[,2]))/length(na.omit(Grouped.Data.Age.45.Time.21[,2])),
     xlab="Time 21 Group: Age",ylab="Time 21 Group: Time")

cor(na.omit(Grouped.Data.Age.45.Time.21),method="spearman")[1,2]

#Grouped.Data.Age.45.Time.23
plot(rank(na.omit(Grouped.Data.Age.45.Time.23[,1]))/length(na.omit(Grouped.Data.Age.45.Time.23[,1])),
     rank(na.omit(Grouped.Data.Age.45.Time.23[,2]))/length(na.omit(Grouped.Data.Age.45.Time.23[,2])),
     xlab="Time 21 Group: Age",ylab="Time 21 Group: Time")

cor(na.omit(Grouped.Data.Age.45.Time.23),method="spearman")[1,2]


library(copula)

data.to.copula<- pobs(na.omit(Grouped.Data.Age.25.Time.23), ties.method = "average")
Gaussian.Copula.Age.25.Time.23.fit<-fitCopula(normalCopula(), data.to.copula, method="ml")
pobs(na.omit(Grouped.Data.Age.25.Time.23), ties.method = "average")


data.to.copula<- pobs(na.omit(Grouped.Data.Age.45.Time.21), ties.method = "average")
Gaussian.Copula.Age.45.Time.21.fit<-fitCopula(normalCopula(), data.to.copula, method="ml")
Gaussian.Copula.Age.45.Time.21.fit


