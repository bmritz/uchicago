#R Code Workshop#
#################

install.packages("httr")
#require(httr)

install.packages("jsonlite")
#require(jsonlite)

install.packages("reshape2")
#require(reshape2)

## Fetch rate data from priceonomics website in JSON format
## Read into dataframe
fxdata <- as.data.frame(fromJSON("http://fx.priceonomics.com/v1/rates/"))

#Read JSON Format in a pretty way:
toJSON(fxdata, pretty = TRUE)

#Example:
ipa <- as.data.frame(fromJSON("http://ip.jsontest.com/"))
toJSON(ipa)

(NOTES)

#print(fxdata)

## Separate currency labels
  #Get 1st Country Label
  fromcurr <- substr(names(fxdata),1,3)
  
  #Get 2nd Country Label
  tocurr <- substr(names(fxdata),5,7)

## Transposing fxdata to fetch rates
tfxdata <- t(fxdata)

## I'm identifying the column that has the rates from transposed matrix in from-to currency format
#[row, column]
rates <- as.numeric(tfxdata[,1])
#Read data easier:
data.frame(rates)

## Combining currency and rates into a dataframe
rateframe <- data.frame(fromcurr,tocurr,rates)

## Reshaping currency data into matrix form for operations
#melt converts an object into molten data frame. This is essentially saying that I'm combining 'fromcurr' and 'tocurr' variables to the rates column automatically.
#In coding, you would write a loop function and logic, but this is complex. The 'melt' function simplifies and does the operation for you.
#'melt' is a one-line process to write a for loop.
mfxdata <- melt(rateframe,id.vars=c("fromcurr","tocurr"))

# acast function gives output a matrix of my data
ratematrix <- acast(mfxdata,fromcurr ~ tocurr)
currCodes <- colnames(ratematrix)

#You want matrices because:
  1. Fast
  2. Lots of Linear Algebra equations calculate matrices (manage, manipulate, process)
  3. Time-series 

#print(currCodes)