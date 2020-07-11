## DATA SCIENCE ONLINE TRAINING - HARVARD edX

#################################################################################
## CHAPTER 1. R BASICS.
#################################################################################

setwd("~/Documents/Trainings/DataScienceTraining")

library(tidyverse)
library(dslabs)

data(murders)
murders %>% 
  ggplot(aes(population,total,label=abb,color=region)) +
  geom_label()
  
## VARIABLES ##

a <- 2
b <- -1
c <- -4

## OBJECTS ##

solution_1 <- (-b + sqrt(b^2 -4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 -4*a*c))/(2*a)

## FORMULAS ##

n <- 1000
n*(n+1)/2
x <- seq(1,n)
sum(x)

x <- 5
log(10^x)

## FUNCTIONS LEARNED ##

str(murders)
head(murders) #first 6
names(murders) #provides list of columns of a dataset
murders$region #access elements/values of the column/variable
a <- murders$abb
b <- murders[["abb"]]
identical(a,b)
class(a)
# class can be character, numeric, logical and factor. Factor are categories
# levels function only work with factor class

###############################
## VECTORS ##

codes <- c(italy=380,canada=124,egypt=818)
codes <- c(380,124,818)
countries <- c("italy","canada","egypt")
names(codes) <- countries
seq(1,14,2)
1:14

# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

###############################
## VECTOR COERCION ##

x <- 1:5
y <- as.character(x)
z <- as.numeric(y)

# SORTING

sort(murders$total)
order(murders$total)
index <- order(murders$total)
murders$state[index]

max(murders$total)
is <- which.max(murders$total) 
murders$state[is]

original <- c(31,4,15,92,65)
rank(original)
sort(original)
order(original) #it sorts and then gives original index/position

which.min(murdes$populaiton) # gives you the index

states <- murders$state
ranks <- rank(murders$population)
my_df <- data.frame(name=states,rank=ranks)
my_df
ind <- order(murders$population)
my_df <- data.frame(states=states[ind],ranks=ranks[ind])
my_df

###############################
## NA EXAMPLE ##

# Using new dataset 
library(dslabs)
data(na_example)
str(na_example)
# Find out the mean of the entire dataset 
mean(na_example)
# Use is.na to create a logical index ind that tells which entries are NA
ind <- is.na(na_example)
# Determine how many NA ind has using the sum function
sum(ind)

###############################
## REMOVING NAs

# Note what we can do with the ! operator
x <- c(1, 8, 3)
ind <- c(FALSE, TRUE, FALSE)
a <- x[!ind]
mean(a)
# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)
# We saw that this gives an NA
mean(na_example)
# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])

###############################
## RATIOS ##

# The name of the state with the maximum population is found by doing the following
murders$state[which.max(murders$population)]
# how to obtain the murder rate
murder_rate <- murders$total / murders$population * 100000
# ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing=TRUE)]

###############################
## INDEXING VECTORS ##

# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)
# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

###############################
## INDEXING FUNCTIONS ##

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE
# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]
# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]
x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x
# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state
# Store the 5 abbreviations in abbs. (remember that they are character vectors)

abbs <- c("MA", "ME", "MI", "MO", "MU") 
# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!abbs %in% murders$abb)
# Names of abbreviations in `ind`
abbs[ind]

###############################
## DATA WRANGLING ##

# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)
# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)
# subsetting with filter
filter(murders, rate <= 0.71)
# selecting columns with select
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)
# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)
# Defining rate
rate <-  murders$total/ murders$population * 100000
# Redefine murders to include a column named rank with the ranks of rate from highest to lowest
murders <- mutate(murders, rank = rank(-rate))

# Use filter to create a new data frame no_south
no_south <- filter(murders,region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders,region %in% c("Northeast","West"))
# Number of states (rows) in this category 
nrow(murders_nw)
# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders,region %in% c("Northeast","West") & rate < 1)
# Use select to show only the state name, the murder rate and the rank
select(my_states,state,rate,rank)

###############################
## CREATING DATA FRAMES ##

# creating a data frame with stringAsFactors = FALSE

grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                    exam_1 = c(95, 80, 90, 85), 
                    exam_2 = c(90, 85, 85, 90),
                    stringsAsFactors = FALSE)

my_states <- data.frame(murders) %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) 
%>% filter(region %in% c("Northeast","West") & rate < 1) %>% select(state,rate,rank)

## BASIC PLOTS

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y, col = "blue")
murders_state_index <- which.max(murders$population) # assigns index to variable/object
murders$state[which.max(murders$rate)]
# a histogram of murder rates
hist(murders$rate)
# boxplots of murder rates by region
boxplot(rate~region, data = murders)

ind <- order(murders$rate,decreasing=TRUE)
st <- murders$state
pop <- murders$population
tot <- murders$total
rk <- murders$rank
new_order <- data.frame(rk=rk[ind],st=st[ind],pop=pop[ind],tot=tot[ind])
new_order

ggplot(murders,aes(x,y,label=abb,color=region)) + geom_label()

###############################

x <- c("a", "a", "b", "b", "b", "c")
table(x)
table(murders$region)

library(dslabs)
data(heights)
options(digits = 3)

mean(heights$height)
ind <- (heights$height >= mean(heights$height)) & (heights$sex == "Female")
sum(ind) # individuals above avg and female
heights[ind]

mean(heights$sex=="Female") # proportion of female individuals
max(heights$height) # minimum height in table
xx <- match(min(heights$height),heights$height) # determine the index of the individual with the minimum height
heights$sex[xx] # sex of the lowest height index

min1 = min(heights$height)
max1 = max(heights$height)
x <- c(min(heights$height),max(heights$height))

# vector x that includes the integers between the minimum and maximum heights.
x <- min(heights$height):max(heights$height) 
x[which(x %in% heights$height)]
ind <- which(x %in% heights$height)
x[ind]

sum(!x %in% heights$height) # integers not in heights database

heights2 <- mutate(heights,ht_cm=height*2.54)
heights2$ht_cm[18]
mean(heights2$ht_cm)
str(heights2)

female <- filter(heights2,sex=="Female")
str(female)
mean(female$ht_cm)

library(dslabs)
data(olive)
head(olive)
str(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)

###############################
## PROGRAMMING BASICS ##
###############################

## BASIC CONDITIONALS ##

# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}
# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}
# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}
# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)
# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))
# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

###############################
## BASIC FUNCTIONS ##

# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))
# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s
# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}
# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

###############################
## BASIC FOR LOOPS ##

# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
# a very simple for-loop
for(i in 1:5){
  print(i)
}
# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
# creating a plot for our summation function
n <- 1:m
plot(n, s_n)
# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))
# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)
# other loop functions
apply()
sapply()
tapply(vector, index, function)
mapply(function, ...)
# other useful functions
split()
cut()
quantile()
Reduce()
identical()
unique()
# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Report the value of the sum when n=10
compute_s_n(10)

new_names <-  ifelse(nchar(murders$state)>8,murders$abb,murders$state)

x <- 5
y <- 10
altman_plot <- function(x,y){
  plot(y-x,x+y)
}
log_plot <- function(x, y){
  plot(log10(x), log10(y))
}
altman_plot(5,10)


# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

# Check that s_n is identical to the formula given in the instructions.

identical(s_n,(n*(n+1)*(2*n+1))/6)
