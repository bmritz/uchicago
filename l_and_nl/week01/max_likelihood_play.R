require(rgl)

manual.normal.dist <- function(x, mu=0, sd=1){
first.term <- 1 / sqrt(2*pi*(sd^2))
second.term <- exp(-((x - mu)^2)/(2*sd^2))
return(first.term*second.term)
}

x.vect <- seq(from=-10, to=10, length.out=10000)
plot(sapply(x.vect, manual.normal.dist))

y1.vect<-seq(from=-10, to=10, length.out=100)
y2.vect<-seq(from=-10, to=10, length.out=100)

all.vect <- expand.grid(y1.vect, y2.vect)
y1.vect <- all.vect[,1]
y1.vect <- all.vect[,2]
# y1.norm<- manual.normal.dist(y1.vect)
# y2.norm<- manual.normal.dist(y2.vect)

y1.norm<- sapply(y1.vect, manual.normal.dist)
y2.norm<- sapply(y2.vect, manual.normal.dist)
# creates matrix of points -- combinations of each y1.norm with each y2.norm

joint.pdf <- y1.norm*y2.norm
joint.pdf <- outer(y1.norm, y2.norm, "*")

plot3d(y1.vect, y2.vect, joint.pdf)



# Now -- switch around our thinking and use mu and sd on the axes:

# we can create a join.pdf.function as a function of y1.vect and y2.vect:


# NOTE: LOOK OUT FOR UNDERFLOW IF WE EXPAND BEYOND y1 and y2
joint.pdf.func <- function(y1, y2, mu=0, sd=1){
  y1.normed <- manual.normal.dist(y1, mu=mu, sd=sd)
  y2.normed <- manual.normal.dist(y2, mu=mu, sd=sd)
  return((y1.normed*y2.normed))
}

#check that it works like the last way we did

joint.pdf<- mapply(joint.pdf.func, y1.vect, y2.vect)
# joint.pdf <- joint.pdf / sum(joint.pdf)
plot3d(y1.vect, y2.vect, joint.pdf)

# just above, we flexed y1 and y2 and kept mu and sd constant, 
# now, we we will flex mu and sd and keep y1 and y2 constant

# fix y1 and y2
y1.fixed = 4
y2.fixed = 8

# vectors of mu and sd
mu.vect <- seq(from= -10, to=10, length.out=100)
sd.vect <- seq(from= -10, to=10, length.out=100)

allmusd <- expand.grid(mu.vect, sd.vect)
mu.vect <- allmusd[,1]
sd.vect <- allmusd[,2]
# joint.pdf.mu.sd <- outer(mu.vect, sd.vect, function(mu1, sd1){joint.pdf.func(y1=y1.fixed, y2=y2.fixed, mu1, sd1)})

joint.pdf.mu.sd <- mapply(function(mu1, sd1){joint.pdf.func(y1=y1.fixed, y2=y2.fixed, mu=mu1, sd=sd1)}, 
                          mu.vect, sd.vect)
# joint.pdf.mu.sd <- joint.pdf.mu.sd / sum(joint.pdf.mu.sd)


plot3d(mu.vect, sd.vect, joint.pdf.mu.sd)
# you can see the max on the mu axis is at the mean of the y1, y2s

persp3D(mu.vect, sd.vect, t(joint.pdf.mu.sd))

mu.norm<- sapply(mu.vect, function(x){manual.normal.dist(y1.fixed, mu=x)})
