Week 2 Homework 2 - Calculate Maximum by Monte Carlo Method
========================================================
Brian Ritz
--------------------------------------------------------
MSCA 31007 Autumn 2014
--------------------------------------------------------

We were given the following three functions and tasked with finding:
F(InputPoint)=MAX(function.1(InputPoint),function.2(InputPoint),function.3(InputPoint))


```r
# input parameters 
a <- -50
b <- 50
n <- 10000
```


```r
function.1 <- function(InputPoint) {
  return(-5*InputPoint+2)
}
function.2 <- function(InputPoint) {
  return(7*InputPoint+3)
}
function.3 <- function(InputPoint) {
  return(-2*InputPoint^2+0.5*InputPoint+120)
}
```

So, we create the function we are supposed to find:


```r
target.fxn <- function(input.point){
  out<-max(function.1(input.point), function.2(input.point), function.3(input.point))
  return(out)
}
```

Now, to use the function, apply it to the X vector that we generate from draws from a random distribution.


```r
# n draws
X <- runif(n, a, b)

# we must sort the Xs if we want to use a line chart--otherwise the lines will be drawn across the figure
X<-sort(X)

# could also use: 
#X <- seq(a, b, 1)
Y <- sapply(X, target.fxn)
plot(X, Y)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

