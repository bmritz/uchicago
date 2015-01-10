# ---------------------------------------------------------
# Section 0: Websites.
# ---------------------------------------------------------

# Download and intall R.
www.r-project.org # The base R with a console.
# Download and install RStudio (recommended).
www.rstudio.com # An IDE for R.


# Useful websites.
http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
www.rdocumentation.org # R documentation.
www.r-fiddle.org # Online interaction R console.
crantastic.org # Reviews of R packages.
www.statmethods.net # Tutorial by Quick-R.
www.cookbook-r.com # Tutorial.
www.r-bloggers.com # Blog.
www.inside-r.org # Blog and documentation by Revolution Analytics.


# ---------------------------------------------------------
# Section 1: Using the R Console as a calculator.
# ---------------------------------------------------------

# Number arithmetic.
1 + 0.2 * (3e2 - 4) / 5e-1 # Integer, decimal, and exponential number.
sqrt(3 ** 2 + 4 ^ 2) # Both ** and ^ are exponent.
                     # See also: abs, exp, log, cos, sin


# Boolean expression.
1 < 2 # TRUE or FALSE value.
      # See also: >, >=, <=, ==, !, !=, &, |
(5 >= 4) & (1 != 2)


# Storing value into variable.
price = 2
quantity <- 3 # Both <- and = are OK.  But <- is the convention.
total <- price * quantity
answer <- total <= 10


# Text.
question <- 'What are you doing?'
response <- "I'm learning R." # Both ' and " are OK.


# Naming of variable.
a, b, x, y # Don't use "c" or "data"; they are R's keyword.
ice.cream, ice_cream, IceCream, # All are OK.  The dot style is the convention.


# Current workspace i.e. stuff in the current R Console session.
ls() # A list of existing variable names.
rm(answer) # Remove a variable.
# Go to File -> Save Workspace... to save all variables to a .RData file.


# Help.
?c # A useful way to check if a word is R's built-in keyword or function.


# ---------------------------------------------------------
# Section 2a: Vector.
# ---------------------------------------------------------

# A vector is a sequence of items of the same type.
numbers <- c(1, -2, 3) # Here "c" is a built-in function to combine items.
phones <- c("Apple", "Samsung", "Nokia", "HTC")
numbers <- numeric(5) # Vector of 0's.
x <- rep(1, 5)
y <- 2:7 # Increment by 1.  Note that 7:2 is descending.
         # See also: seq
numbers <- c(x, y) # Concatenate two (or more) vectors together.


# Selecting items from a vector.
phones[1] # Indexing by positive integers.
phones[2:3]
phones[c(4,1)]
phones[-2] # Negative integers means "except".
phones[-c(2,4)]
phones[c(TRUE, FALSE, TRUE, FALSE)] # Indexing by TRUE boolean values.


# Basic operations on vector.
length(numbers)
phones <- sort(phones) # See also: order, rank


# Element-wise computation i.e. vectorization.
x <- 1:5
y <- 6:10
x + y # Also for other binary operators e.g. -, *, /, &, |
x ^ 2 # Same as x ^ rep(2, length(x)).


# Example.
x[x > 1 & y < 10] # Vectorized > and < and &, then boolean indexing.
phones[phones < "M"]


# ---------------------------------------------------------
# Section 2b: Matrix.
# ---------------------------------------------------------

# A matrix of items of the same type.
m1 <- matrix(0, 3, 3) # Create a matrix of 0's.
m2 <- matrix(1:9, 3, 3, byrow=TRUE) # Fill a matrix.
m3 <- cbind(m1, m2) # See also: rbind


# Selecting items from a matrix.
m <- matrix(1:16, 4, 4)
m[1,2] # Integer indexing.
m[1,] # Omit means to select along the whole dimension.
m[,2:4]
m[2:3,-c(1,4)]
m[,m[1,] >= 6] # Boolean indexing.


# Basic operations on matrix.
dim(m) # See also: nrow, ncol
dim(m) <- c(8,2) # Reshape the matrix.
rowSums(m) # See also: colSums, colMeans, rowMeans


# Matrix operations.
m <- matrix(1:6, 3, 2)
t(m) # Transpose.
m * m # Element-wise multiplication.
t(m) %*% m # Matrix multiplication.
m <- diag(3) # Create a diagonal matrix of 1's.
diag(m) <- 2 # Modify its diagonal
solve(m) # Matrix inverse.
# See also: eigen, svd, det


# ---------------------------------------------------------
# Section 2c: List.
# ---------------------------------------------------------

# A list is a sequence of stuff of possibly different types.
x <- list(TRUE, 2, "Three")
movie <- list(title="Toy Story", year=1995,
    production=c("Walt Disney", "Pixar")) # Each item has a name tag.


# Select one item from the list.
x[[2]] # Indexing by integer.  Note the use of double brackets.
movie[[3]]
movie[["production"]] # Indexing by name tag.
movie$production # By dollar sign and name tag.


# Select a sub-list of the list.
x[2] # A list with one item.
x[2:3]
movie[c("title", "year")] # A list with two items.


# Adding items to a list.
x[4] <- 4.0 # By index.
x$five <- c("F", "i", "v", "e") # By name tag.


# Removing items from a list.
movie$year <- NULL
movie[[2]] # All the following items shift its position forward.
x[2:3] <- NULL # Remove a chunk of items.


# Basic operation on list.
length(movie)
names(movie) # The name tags.


# Combining two lists.
y <- c(x, movie) # A longer list consists of items from both lists.


# ---------------------------------------------------------
# Section 2d: Factor.
# ---------------------------------------------------------

blood.type <- c("O", "A", "B", "A", "O", "AB", "A", "B")
f <- factor(blood.type) # Convert a vector to factor with levels.
levels(f)


# What exactly is the difference between blood.type and f?  Use str() to find
# out.  The str() function is a very useful tool to peek into the structure of
# a variable.
str(blood.type) # Just a vector of texts.
str(f) # It knows the 4 levels, and each item is encoded by a number.


# Why do we need factor?
#   Many statistical analyses in R understand the meaning of a factor and
#   handle it correctly.  For example, in linear regression, if you convert a
#   variable to a factor, then you don't need to create dummy 0-1 variables
#   yourself, and the ANOVA also groups all levels of a factor together
#   properly.


# ---------------------------------------------------------
# Section 3: Import and export data.  Use of data frame.
# ---------------------------------------------------------

# Current directory to import data from.
getwd()


# Importing CSV file into a data frame.
price <- read.csv("price.csv") # See also: read.table
price # Your screen is probably not large enough to display it all.
head(price) # Show the first few rows.
            # See also: tail


# Overview of the data frame.
class(price)
str(price) # By default, read.csv() treats a column of text as a factor.
           # If you want to keep text as text (such as for dates), then use:
           #   read.csv(...,stringsAsFactors=FALSE).
colnames(price)
summary(price) # Five-number summary of each column.


# What is a data frame?
# 1. It behaves like a matrix.  But the columns can have different data types.
dim(price) # Also: nrow, ncol
price[1,]
price[1:10,5]
# 2. It is actually a list of vectors.  Each column is one vector.
typeof(price)
length(price) # Same as the number of columns.
names(price) # Titles of the columns are the name tags of the list.
price$AAPL[1:5]


# Select column(s) by name tag.
price[1:5, c("EBAY", "AMZN")]


# Creating a new data frame.
volume <- read.csv("volume.csv")
groupon <- data.frame(Date=price$Date, Price=price$GRPN, Volume=volume$GRPN)

yelp <- as.data.frame(cbind(price$YELP, volume$YELP))
colnames(yelp) <- c("Price", "Volume")
rownames(yelp) <- price$Date


# Exporting a data frame to a CSV file.
write.csv(groupon, file="groupon.csv", row.names=FALSE)
    # See ?write.csv for details about the parameters.


# Saving (any kind of) stuff to a .RData file.
save(groupon, file="groupon.RData") # File size is much smaller than CSV file.


# Loading data from a .RData file.
rm(groupon)
load("groupon") # Much faster to load than importing CSV file.


# ---------------------------------------------------------
# Section 4: Plotting and statistical analyses.
# ---------------------------------------------------------

# (Finally, we have learned enough of the annoying technical stuff to perform
# some useful statistical analyses.)
price <- read.csv("price.csv")
volume <- read.csv("volume.csv")


# Useful websites for plotting.
www.statmethods.net/graphs # Various kinds of graphs.
www.statmethods.net/advgraphs # Fine-tuning and other tools.


# Task 1: Plotting Facebook stock price.

# Plot as a sequence of numbers.
plot(price$FB, main="Stock Price of Facebook (Year 2014)", xlab="Day",
     ylab="Price", type="l", col="blue")

# Plot with Date object.
stock.date <- as.Date(price$Date) # Convert the characters to Date objects.
class(stock.date)
plot(stock.date, price$FB, main="Stock Price of Facebook", xlab="Year 2014",
     ylab="Price", type="o", pch=20, col="blue")
  # Because x is a Date object, the x-axis labels are more display-friendly.

# Plot as a ts object.
library(ts) # Load the "ts" package that comes with R.
fb.price.ts <- ts(price$FB)
plot(fb.price.ts, main="Stock Price of Facebook (Year 2014)", ylab="Price",
     type="l", col="blue")
  # Because y is a ts object, the x-axis label is called "Time" automatically.
  # But it is not the best to use a ts object here because the stock price is
  # not a regular time series.

# Plot as a xts object.
library(xts) # Need to install the xts() package first.
fb.price.xts <- xts(price$FB, order.by=as.Date(price$Date))
plot(fb.price.xts, main="Stock Price of Facebook", ylab="Price")
  # Because y is a xts object, the plot automatically behaves differently.

stock.date.xts <- index(fb.price.xts)
plot(stock.date.xts, fb.price.xts, main="Stock Price of Facebook",
     xlab="Year 2014", ylab="Price", type="o", pch=20, col="blue")

is.higher <- diff(fb.price.xts) > 0
is.higher[1] <- TRUE # By default, diff() function applied to xts() object gives NA.
plot(stock.date.xts, fb.price.xts, main="Stock Price of Facebook",
     xlab="Year 2014", ylab="Price", type="o", pch=20, col="grey")
points(stock.date.xts[is.higher], fb.price.xts[is.higher], pch=20,
       col="green")
points(stock.date.xts[!is.higher], fb.price.xts[!is.higher], pch=20,
       col="red")


# Task 2: Plotting Facebook and Twitter stock prices in one plot.
fb.price.xts <- xts(price$FB, order.by=as.Date(price$Date))
twtr.price.xts <- xts(price$TWTR, order.by=as.Date(price$Date))

xrange <- range(index(fb.price.xts))
yrange <- range(c(fb.price.xts, twtr.price.xts))
plot(xrange, yrange, # Set the x and y range of the plot.
     main="Stock Prices",
     xlab="Year 2014",
     ylab="Price",
     type="n") # But don't show anything yet.
lines(index(fb.price.xts), fb.price.xts, type="l", col="blue") # Add a line.
lines(index(twtr.price.xts), twtr.price.xts, type="l", col="red")
legend(x="topleft",
       legend=c("Facebook", "Twitter"),
       col=c("blue", "red"),
       lty=1,
       bty="n")


# Task 3: Show Microsoft stock price and volume in separate plots in one window.
# Multiple graphs in one figure.
par(mfrow=c(2,1)) # Divide the figure into 2 rows and 1 columns, filled by row.
                  # See also: par(mfcol=...)

stock.date <- as.Date(price$Date)
plot(stock.date, price$MSFT, main="Microsoft", xlab="", ylab="Stock Price",
     type="l", col="blue")
plot(stock.date, volume$MSFT, xlab="Year 2014", ylab="Volume", type="l",
     col="darkgrey")
par(mfrow=c(1,1)) # Reset it back to single plot.


# Task 4: Exporting figure.
# Method 1: Go to Export -> Save as PDF.
# Method 2:
pdf("figure.pdf", width=8, height=5) # Create a PDF file.  Width and height in inches.
plot(ts(price$NFLX), main="Stock Price of Netflix (Year 2014)", ylab="Price",
     type="l", col="blue") # Draw stuff to the file.
dev.off() # Close the file.


# Task 5: Plotting the log-return of Yahoo stock price.
yahoo.price <- price$YHOO
plot(yahoo.price, main="Stock Price of Yahoo",
     xlab="Trading Day in Year 2014", ylab="Price", type="o", pch=20,
     col="purple")

yahoo.log.return <- diff(log(yahoo.price))
plot(yahoo.log.return, main="Log-Return on Yahoo Stock",
     xlab="Trading Day in Year 2014", ylab="Log-Return", type="o", pch=20,
     col="purple")
abline(a=0, b=0, lty=2) # Add a horizontal line.


# Task 6: Scatterplot of Yahoo log-return and volume.
dot.color <- rgb(82,6,180,70,maxColorValue=255) # Partial transparency.
plot(yahoo.log.return, volume$YHOO[-1], main="Yahoo Stock", xlab="Log-Return",
     ylab="Trading Volume", pch=16, col=dot.color)
abline(v=0, lty=2) # Add a vertical line.
grid(10, 10) # Add a grid.


# Task 7: Are the log-returns normally distributed?
summary(yahoo.log.return)
par(mfrow=c(1,2))

# Histogram of log-return.
hist(yahoo.log.return, breaks=20, freq=F, main="Histogram of Yahoo Log-Return",
     xlab="Log-Return", border="darkgrey")
# Add a kernel density smoothing line.
lines(density(yahoo.log.return), col="purple", lwd=2)
# Add the density of the normal distribution with matching mean and standard
# deviation.
tick <- seq(from=min(yahoo.log.return), to=max(yahoo.log.return),
            length.out=100)
lines(tick, dnorm(tick, mean(yahoo.log.return), sd(yahoo.log.return)),
      col="orange", lwd=2)

# Simulate a random sample from the normal distribution.
set.seed(1)
x <- rnorm(length(yahoo.log.return), mean(yahoo.log.return),
           sd(yahoo.log.return))
hist(x, breaks=20, freq=F, main="Histogram of Simulated Log-Return",
     xlab="Log-Return", border="darkgrey")
tick <- seq(from=min(x), to=max(x), length.out=100)
lines(tick, dnorm(tick, mean(x), sd(x)),
      col="orange", lwd=2)

# Q-Q plot.
par(mfrow=c(1,1))
qqnorm(yahoo.log.return)
qqline(yahoo.log.return, col="red")


# ---------------------------------------------------------
# Section 5: Basic programming.
# ---------------------------------------------------------

# IF ELSE statement.
x <- -3
if (x > 0) {
  answer <- "Positive"
} else if (x == 0)  {
  answer <- "Zero"
} else {
  answer <- "Negative"
}
answer


# FOR loop.
stock.symbol <- colnames(price)[-1]
# Method 1: Loop through an index.
total <- 0
for (i in 1:length(stock.symbol)) {
  print(i)
  total <- total + nchar(stock.symbol[i])
}
# Method 2: Loop through the items themselves.
total <- 0
for (symbol in stock.symbol) {
  print(symbol)
  total <- total + nchar(symbol)
}
# (But the simplest way is sum(nchar(stock.symbol)).  Think vectorization!)


# WHILE loop.
x <- 5
while(x > 0) {
  x <- x - 1
  print(paste("x:", x)) # See also: paste0
}


# Simple Function.
hitchhiker <- function(x) {
  if (x == 42) {
    return(x)
  } else {
    return(42)
  }
}

hitchhiker(2)


# Function with parameter that has default value.
random.password <- function(n, choice=c(letters, LETTERS)) { # See ?letters.
  if (n <= 0) {
    return(NULL) # NULL represents the empty object in R.
  }
  password <- paste0(sample(choice, n), collapse="") # See ?sample.
  return(password)
}

random.password(10) # Use default value.
random.password(10, 0:9) # Use 0 to 9 instead.


# Functin that returns more than one stuff.
five.number.summary <- function(x) {
  m1 <- min(x)
  q1 <- quantile(x, 0.25, names=FALSE)
  q2 <- median(x)
  q3 <- quantile(x, 0.75, names=FALSE)
  m2 <- max(x)
  return(list(minimum=m1, first.quartile=q1, median=q2, third.quartile=q3,
              maximum=m2)) # Put all the stuff in a list.  Then return one list.
}

x <- c(0.6, 1.7, 0.4, -0.3, -0.3, 0.2, -0.2, -1.5, 1.6, 1.5)
result <- five.number.summary(x)
result
result$first.quartile


# Super-assignment operator.
x <- 1

want.to.change.x <- function() {
  x <- 2 # Create a new variable called "x" and assign to it the value 2.
  print(paste("Here x =", x))
}

want.to.change.x()
x # What is the value of x here?

really.change.x <- function() {
  x <<- 3 # Find the variable called "x" outside of the function.
}

really.change.x()
x


# ---------------------------------------------------------
# Section 6: Apply.
# ---------------------------------------------------------

# A whole family of them:
#   apply, lapply, sapply, vapply, rapply, tapply, mapply, eapply
# and their relatives:
#   replicate, ave, aggregate, by, sweep
# See:
#   https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#   http://rollingyours.wordpress.com/category/r-programming-apply-lapply-tapply/


# Example: apply
x <- matrix(rnorm(25), nrow=5, ncol=5); x

# Compute the average of each row.
apply(x, 1, mean) # Same as rowMeans(x)

# Count the number of negative entries in each column.
count.negative <- function(values) {
  return(sum(values < 0))
}
apply(x, 2, count.negative)
# (But colSums(x < 0) is faster.)


# Example: lapply and sapply
avg.log.return <- function(stock.price) {
  return(mean(diff(log(stock.price))))
}
lapply(price[,-1], avg.log.return)
sapply(price[,-1], avg.log.return) # Do lapply() and try to simplify.


# Split-apply-combine (SAC) operation.
apple.price <- price$AAPL
apple.volume <- volume$AAPL

# A naive approach.
is.higher <- c(TRUE, diff(apple.price) > 0)
mean(apple.volume[is.higher])
mean(apple.volume[!is.higher])

# Using split-lapply-unlist.
# 1. Create a "factor" to indicate if today's Apple stock price is higher than
# yesterday's stock price or not.
apple.price <- price$AAPL
direction <- factor(c(TRUE, diff(apple.price) > 0))
levels(direction)
levels(direction) <- c("Lower", "Higher")
# 2. Split the trading volume into two sets for "higher" or "lower".
apple.volume <- volume$AAPL
high.low.set <- split(apple.volume, direction)
# 3. Use lapply().
avg.volume <- lapply(high.low.set, mean)
# 4. Convert to a vector.
avg.volume <- unlist(avg.volume)

# Using tapply
tapply(apple.volume, direction, mean)
