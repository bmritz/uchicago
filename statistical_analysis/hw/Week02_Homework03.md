Week 2 Homework 3 - Simulate Linear Model
========================================================
Brian Ritz
--------------------------------------------------------
MSCA 31007 Autumn 2014
--------------------------------------------------------

We will simulate the linear model of the form:

Y = aX + b + Epsilon, a=1, b=2.5

Simulate {X1, X2, ., XN}, N=100 and Xi are independent and identically distributed normal random variables with mu=3, sigma=2.5. Epsiloni are normal with mu=0, sigma=1.5.



```r
a<-1
b<-2.5
obs <- 100
set.seed(123)

produceY <- function(a,b,X,Epsilon){
  return(a*X + b + Epsilon)
}
```

We will produce vectors of X and Epsilon, then apply our function to the vectors using mapply.


```r
# xs are normally distributed
norm.mu <- 3
norm.sigma <- 2.5
X1 <- rnorm(obs, norm.mu, norm.sigma)
Epsilons <- rnorm(obs, 0, 1.5)

Y1 <- produceY(a, b, X1, Epsilons)
plot(X1, Y1)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

You can see the normal distribution in the x-axis, there are more points near the middle of the x-axis (near the mean of 3), and as we move away from x=3 there are less and less observations. You can also see the linear pattern, as x increases, so does y.


Xi are exponential independent and identically distributed variables with lambda=0.5. Epsilons are normal with mu=0, sigma=1.5


```r
# xs are normally distributed
lambda <- 0.5

X2 <- rexp(obs, lambda)
Epsilons <- rnorm(obs, 0, 1.5)

Y2 <- produceY(a, b, X2, Epsilons)
plot(X2, Y2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
