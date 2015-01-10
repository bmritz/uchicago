
# ---------------------------------------------------------
# Section 0: Introduction to R
# ---------------------------------------------------------

# Download and intall R.
www.r-project.org # The base R with a console.
# Download and instal RStudio (optional).
www.rstudio.com # An IDE for R.


# Useful websites.
www.rdocumentation.org # R documentation.
www.r-fiddle.org # Online interaction R console.
crantastic.org # Reviews of R packages.
www.statmethods.net # Tutorial by Quick-R.
www.cookbook-r.com # Tutorial.
www.r-bloggers.com # Blog.
www.inside-r.org # Blog and documentation by Revolution Analytics.


# Comparison of R with other statistical software:
# 1. SAS
#      Powerful.  Commercial support.  Not free.
#      Ugly old syntax.
# 2. STATA, SPSS
#      Easy-to-use mouse click.  Popular in social science.
#      Not a programming language.
# 3. MATLAB
#      Powerful.  Nice syntax for matrix computation.  Good documentation.
#      Popular in engineering.  Not as complete for statistical analysis.
# 4. Python + NumPy + Scipy + Panda + IPython
#      General-purpose programming language.  Free.
#      More complicated to install.  Harder to learn.
# 5. R
#      Lots of statistical tools.  A programming language.  Free
#      Cutting-edge 3rd-party packages.  Documentation can be confusing.


# What does this R Workshop teach?
#   1. Use the R Console as a calculator
#   2. Data type: number, vector, matrix, list
#   3. Import and export data.  Data frame.
#   4. Basic statistics e.g. summary, contingency table, t-test
#   5. Plotting graphs
#   6. Using packages
#   7. Basic programming syntax e.g. IF ELSE, FOR, WHILE loop, function


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
phone[-c(2,4)]
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
hflights <- read.csv("hflights.csv") # See also: read.table
hflights # Your screen is probably not large enough to display it all.
head(hflights) # Show the first few rows.
               # See also: tail


# Overview of the data frame.
str(hflights) # By default, read.csv() treats a column of text as a factor.
summary(hflights) # Five-number summary of each column.


# What is a data frame?
# 1. It behaves like a matrix.  But the columns can have different data types.
dim(hflights)
hflights[1,]
hflights[1:10,5]
# 2. It is actually a list of vectors.  Each column is one vector.
length(hflights) # Same as the number of columns.
names(hflights) # Titles of the columns are the name tags of the list.
hflights$DepTime[1:10]
# (Now you understand why we learn vector, matrix, and list.)


# Select column(s) by name tag.
hflights[1:10, c("DepTime", "ArrTime")]


# Exporting a data frame to a CSV file.
hflights.sunday <- hflights[hflights$DayOfWeek == 1,]
write.csv(hflights.sunday, file="hflight_sunday.csv", row.names=FALSE)
    # See ?write.csv for details about the parameters.


# Saving (any kind of) stuff to a .RData file.
save(hflights, file="hflights.RData") # File size is much smaller than CSV file.


# Loading data from a .RData file.
rm(hflights)
load("hflights.RData") # Much faster to load than importing CSV file.


# ---------------------------------------------------------
# Section 4: Basic statistics.
# ---------------------------------------------------------

# (Finally, we have learned enough of the boring technical stuff to perform
# some useful statistical analyses.)
hflights <- read.csv("hflights.csv")


# Task 1: Arrival delay.
arrival.delay <- hflights$ArrDelay
summary(arrival.delay) # See also: min, max, mean, median, range, quantile
sd(arrival.delay) # Fail because of missing values i.e. NA.
                  # See also: var
sd(arrival.delay, na.rm=TRUE) # Ignore missing values.

idx <- which.max(arrival.delay) # See also: which.min
hflights[idx,]


# Task 2: Cleaning missing data.
x <- c(1, 2, NA, 4)
is.na(x)

any(is.na(arrival.delay)) # See also: all
sum(is.na(arrival.delay)) # How many NA's are there?  TRUE is 1 and FALSE is 0.
mean(is.na(arrival.delay)) # Fraction of NA's.

is.complete <- complete.cases(hflights) # Vector of TRUE or FALSE.
                                        # See also: na.omit
hflights.clean <- hflights[is.complete,] # Indexing by boolean values.
c(nrow(hflights), nrow(hflights.clean))


# Task 3: Re-ordering the data.
idx <- order(hflights.clean$Distance, hflights.clean$AirTime) # See also: rank
hflights.clean <- hflights.clean[idx,]


# Task 4: Number of flights per airline.
(num.carrier <- length(levels(hflights$UniqueCarrier)))
    # Use outermost (...) to display the result after the computation.
(num.flight <- table(hflights$UniqueCarrier)) # Count the number of items for
                                              # each factor level.
rank(-num.flight, ties.method="first") # (Trivia: Who is WN?)

table(hflights$UniqueCarrier, hflights$Origin) # Two-way contingency table.


# Task 5: Number of delay incidents per airport.
delay <- hflights.clean$DepDelay
airport <- hflights.clean$Origin

has.delay <- factor(delay > 0)
levels(has.delay) <- c("On Time", "Delay") # Rename the levels.
result <- table(airport, has.delay)
prop.table(result, margin=1) # Proportion over each row.
                             # See also: margin.table
# (So HOU has higher chance of delay than IAH.)


# Task 6: Length of (positive) delay per airport.
pos.delay.per.airport <- split(delay[delay > 0], airport[delay > 0])
    # What happened above?
    #   1. Select those items from the "delay" and "aiport" vectors that
    #      correspond to a positive amount of delay.
    #   2. Split the (positive) "delay" vector into a list of two vectors; one
    #      for each airport.
str(pos.delay.per.airport)
lapply(pos.delay.per.airport, summary)
    # What happened above?
    #   Apply the summary() function to each airport.  See ?lapply for details.
    #   It is a very useful and powerful tool.
hou.pos.delay <- pos.delay.per.airport$HOU
iah.pos.delay <- pos.delay.per.airport$IAH
t.test(hou.pos.delay, iah.pos.delay) # Two-sample t-test.
                                     # See also: binom.test, chisq.test, etc
# (So given that there is a delay, the length of the delay is similar for both
#  HOU and IAH.)


# ---------------------------------------------------------
# Section 5: Plotting.
# ---------------------------------------------------------

# Useful websites.
www.statmethods.net/graphs # Various kinds of graphs.
www.statmethods.net/advgraphs # Fine-tuning and other tools.


hflights <- read.csv("hflights.csv")
hflights.clean <- na.omit(hflights)


# Simple bar plot.
num.flight <- table(hflights$UniqueCarrier)
barplot(num.flight,
    main="Flight Distribution at HOU and IAH in Year 2011",
    xlab="Airline",
    ylab="Number of flights")


# Scatterplot.
n <- 500 # Fewer data points just to speed it up.
x <- hflights.clean$Distance[1:n]
y <- hflights.clean$AirTime[1:n]
plot(x, y,
    main="Flight Time vs Flight Distance",
    xlab="Flight distance (in miles)",
    ylab="Flight time (in minutes)",
    pch=19, col="blue") # Shape and color of the dots.


# Multiple lines plot.
windows() # Open a new figure window.  Mac uses quartz() and Unix uses X11().
hou.month <- table(hflights$Month[hflights$Origin == "HOU"])
iah.month <- table(hflights$Month[hflights$Origin == "IAH"])
hou.month <- hou.month / hou.month[1]
iah.month <- iah.month / iah.month[1]
month <- 1:12
xrange <- range(month)
yrange <- range(c(hou.month, iah.month))
plot(xrange, yrange, # Set the x and y range of the plot.
    main="Number of Flights per Month (HOU: blue, IAH: red)",
    xlab="Month",
    ylab="Number of flights (normalized by Jaunary)",
    type="n") # But don't show the dots.
lines(month, hou.month, type="b", col="blue", pch=0) # Add a color curve with dot.
lines(month, iah.month, type="b", col="red", pch=1)
abline(1, 0, col="darkgray", lty=2) # Add a straight line in dash style.


# Histogram.
arrival.delay <- hflights.clean$ArrDelay
arrival.delay <- arrival.delay[arrival.delay <= 200] # Just to shorten right tail.
hist(arrival.delay,
    main="Histogram of Arrival Delay (<= 200 only)",
    xlab="Arrival delay (in minutes)",
    ylab="Density",
    breaks=50, freq=FALSE, border="darkgrey") # Number of bins and bar color.
lines(density(arrival.delay), col="red", lwd=1.5) # Add a curve for the density smoothing.


# Multiple graphs in one figure.
par(mfrow=c(2,2)) # Divide the figure into 2 rows and 2 columns, filled by row.
                  # See also: par(mfcol=...)
plot(...)
plot(...)
plot(...)
plot(...)


# Exporting figure.
# Method 1: Go to File -> Save As.
# Method 2:
pdf("figure.pdf", width=8, height=5) # Create a PDF file.  Width and height in inches.
barplot(num.flight,
    main="Flight Distribution at HOU and IAH in Year 2011",
    xlab="Airline",
    ylab="Number of flights") # Draw stuff to the file.
dev.off() # Close the file.


# Example.
# Step 1: Prepare the data points.
n <- 500 # Fewer data points just to speed it up.
x <- hflights.clean$Distance[1:n]
y <- hflights.clean$AirTime[1:n]
# Step 2: Choose some colors.
dot.color <- rgb(0,128,255,50,maxColorValue=255)
lowess.color <- "#707070"
lm.color <- "orange"
# Step 3: Scatterplot of the data points.
plot(jitter(x, amount=20), y,
    main="Flight Time vs Flight Distance",
    xlab="Flight distance (in miles)",
    ylab="Flight time (in minutes)",
    pch=16, col=dot.color, cex=1.5)
# Step 4: Add a smoothing curve.
lines(lowess(x, y, f=0.5), lty=2, col=lowess.color, lwd=1.5)
# Step 5: Add a linear regression line.
abline(lm(y ~ x), col=lm.color, lwd=1.5)
# Step 6: Add a legend.
legend(min(x), max(y),
    c("Flight", "Lowess smoothing", "Linear regression"),
    col=c(rgb(0,128,255,200,maxColorValue=255), lowess.color, lm.color),
    pch=c(16,NA,NA), lty=c(NA,2,1), lwd=c(NA,2,2),
    bty="n", cex=0.8, pt.cex=1.2)
# Step 7: Compute the average distance and flight time to each destination.
dest <- factor(hflights.clean$Dest[1:n]) # Re-factor to remove empty destinations.
dest.x <- unlist(lapply(split(x, dest), mean))
dest.y <- unlist(lapply(split(y, dest), mean))
# Step 8: Show destination names on the graph.
text(jitter(dest.x, amount=100), jitter(dest.y, amount=20),
    levels(dest), cex=0.6, col="red")
# (Trivia: Where are AUS, SAT, and HNL?)


# ---------------------------------------------------------
# Section 6: Using packages.
# ---------------------------------------------------------

# How to install additional R packages?
#   1. Go to Packages -> Install package(s)...
#   2. Choose a CRAN mirror server.
#   3. Choose the package from the list, and click OK.


# Task 1: Use "dplyr" package to do split-apply-combine.
library(dplyr) # Load a package that has already been installed.
hflights.clean <- na.omit(read.csv("hflights.csv"))
planes <- tbl_df(hflights.clean) %.% # Special syntax for "dplyr".
    group_by(TailNum) %.% # Group data by airplane.  Similar to split().
    summarize( # Compute some statistics for each airplane.  Similar to lapply(...).
        count = n(),  
        dist = mean(Distance),
        delay = mean(ArrDelay)) %.%
    filter(count > 20, dist < 2000) # Select those rows that satisfy these criterions.
planes

# Task 2: Use "ggplot2" package to plot.
library(ggplot2)
ggplot(planes, aes(dist, delay)) + 
  geom_point(aes(size = count), alpha = 1/2) + 
  geom_smooth() + 
  scale_size_area()


# Other popular packages.  See www.rdocumentation.org for more.
library(data.table) # Very fast alternative to data.frame.  Its fread() reads
                    # data file super fast.
library(reshape2)
library(sqldf) # Manipulate data frame using SQL syntax.
library(xts) # Time series.


# ---------------------------------------------------------
# Section 7: Basic programming syntax.
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
phones <- c("Apple", "Samsung", "Nokia", "HTC")
# Method 1: Loop through an index.
total <- 0
for (i in 1:length(phones)) {
    print(i)
    total <- total + nchar(phones[i])
}
# Method 2: Loop through the items themselves.
total <- 0
for (phone in phones) {
    print(phone)
    total <- total + nchar(phone)
}
# (But the simplest way is sum(nchar(phones)).  Think vectorization!)


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

