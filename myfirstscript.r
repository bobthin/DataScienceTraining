## DATA SCIENCE ONLINE TRAINING - HARVARD edX

#################################################################################
## CHAPTER 1. R BASICS.
#################################################################################


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

#################################################################################
## CHAPTER 2. DATA VISUALIZATION
#################################################################################

library(dslabs)
data(murders)

x <- heights$height
length(unique(x))
tab <- table(x)
sum(tab==1)

# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))

y <- prop.table(table(murders$region))
x <- murders$region
barplot(y,col="red")
murders %>%
  ggplot(aes(region,total)) + geom_col(aes(color=region))

# CDF
a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

###############################
## NORMAL DISTRIBUTION ##

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))
# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)
# calculate standard units
z <- scale(x)
# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

###############################
## THE NORMAL CDF AND PNORM ##

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#We can estimate the probability that a male is taller than 70.5 inches with:
1 - pnorm(70.5, mean(x), sd(x))
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

###############################
## QUANTILES ##

library(dslabs)
data(heights)
summary(heights$height) # summary() function returns the minimum, quartiles and maximum of a vector.
str(heights$height)
p <- seq(0.01, 0.99, 0.01) #crea una secuencia para generar los percentiles
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
quantile(heights$height,0.75)
# finding quantiles with qnorm
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)

###############################
## Quantile-Quantile Plots ##

# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
# proportion of data below 69.5
mean(x <= 69.5)
# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

###############################
## personal exercises ##

library(lattice)
str(murders)
hist(murder_rate,col="blue")
histogram(murder_rate)
densityplot(murder_rate)
qqnorm(murder_rate);qqline(murder_rate,col=1)
summary(murder_rate)

mycolors <- ifelse(levels(heights$sex)=="Male","green","red")
boxplot(height~sex, data = heights, col = mycolors)

library(dslabs)
data(heights)
summary(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2))
df <- data.frame(female=female_percentiles,male=male_percentiles)
print(df)

library(HistData)
data(Galton)
x <- Galton$child
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
error_avg <- function(k){
  x[1] <- k
  mean(x)
}
error_avg(10000)
error_avg(-10000)

###############################
##  ggplot2 ##

library(tidyverse)
library(dslabs)
data(murders)

ggplot(data = murders)
murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot (geom_label is a textbox, geom_text is plain text)
p + geom_point(aes(population/10^6, total)) +
  geom_label(aes(population/10^6, total, label = abb))

# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# move text labels slightly to the right ==> nudge_x / nurge_y moves the label to the right or up of the point
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 50)

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

#Code: Add labels and title
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

#Code: Change color of the points
# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)

#Code: Add a line with average murder rate
# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

#Code: Change legend title
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title

# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()

# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

#Code: Putting it all together to assemble the plot
# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

p <- ggplot(murders,aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region")

p + theme_economist()


library()
data(package = "dslabs")

###############################
## Other Examples ##

#Code: Histograms in ggplot2
# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

#Code: Smooth density plots in ggplot2
p + geom_density()
p + geom_density(fill = "blue")

#Code: Quantile-quantile plots in ggplot2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

#Code: Grids of plots with the gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")


# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

#################################################################################
## SUMMARIZING DATA WITH dplyr ##

library(tidyverse)
library(dslabs)
data(heights)

# compute average and standard deviation for males
s <- heights %>%
  filter(sex == "Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# access average and standard deviation from summary table
s$average
s$standard_deviation

# compute median, min and max
heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))
# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))

# generates an error: summarize can only take functions that return a single value
heights %>%
  filter(sex == "Male") %>%
  summarize(range = quantile(height, c(0, 0.5, 1)))

###############################
## THE DOT PLACEHOLDER ##

library(tidyverse)
library(dslabs)
data(murders)

murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

# calculate US murder rate, generating a data frame
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

# extract the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate

# calculate and extract the murder rate with one pipe
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate

###############################
## group_by

# libraries and data
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute separate average and standard deviation for male/female heights
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# compute median murder rate in 4 regions of country
murders <- murders %>%
  mutate(murder_rate = total/population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))

###############################
## SORTING DATA TABLES ##

# libraries and data
library(tidyverse)
library(dslabs)
data(murders)

# set up murders object
murders <- murders %>%
  mutate(murder_rate = total/population * 100000)

# arrange by population column, smallest to largest
murders %>% arrange(population) %>% head()

# arrange by murder rate, smallest to largest
murders %>% arrange(murder_rate) %>% head()

# arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

# arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

# show the top 10 states with highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

# show the top 10 states with highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)
murders %>% arrange(region, desc(murder_rate)) %>% top_n(20)


###############################
## EXERCISES

## Practice Exercise. National Center for Health Statistics
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE) # calculate mean removing "NA" values
sd(na_example, na.rm = TRUE) # na.rm is an argument of the mean/d functions

###############################
## SECTION 4

# load and inspect gapminder data
library(dslabs)
data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>%
  filter(year == 2014 & country %in% c("Mexico", "United States")) %>%
  select(country, infant_mortality)
str(gapminder)
unique(gapminder$country)

# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

# add color as continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() + scale_color_discrete(name = "Continent")

###############################
## FACETING

# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

###############################
## TIME SERIES PLOTS

#Code: Single time series
# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

#Code: Multiple time series
# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

#Code: Adding text labels to a plot
# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

###############################
## TRANSFORMATIONS

# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

###############################
## STRATIFY AND BOXPLOTS

Code: Boxplot of GDP by region
# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Code: The reorder function
# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)
#

#Code: Enhanced boxplot ordered by median income, scaled, and showing data
# reorder by median income and color by continent
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

###############################
## COMPARING DISTRIBUTIONS

#Code: Histogram of income in West versus developing world, 1970 and 2010
# add dollars per day variable and define past year
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Code: Income distribution of West versus developing world, only countries with data 
# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Code: Boxplots of income in West versus developing world, 1970 and 2010
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))


###############################
## DENSITY PLOTS

#Code: Faceted smooth density plots
# see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

#Code: Add new region groups with case_when
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#Code: Stacked density plot
# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

#Code: Weighted stacked density plot
# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)


###############################
## ECOLOGIAL FALLACY

# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

###############################
## ASSESSMENT EXERCISES

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(continent == "Africa" & year == 2012) %>%
  ggplot(aes(fertility, life_expectancy )) +
  geom_point(aes(color=region))

#creating a table with selected columns and filtered values
gapminder %>% filter(continent == "Africa", year == 2012,+
                       fertility <= 3, life_expectancy >= 70) %>% 
  select(region, country, fertility, life_expectancy) 

countries <- c("United States", "Vietnam")
labels <- data.frame(country = countries, x = c(1970, 1965), y = c(75, 62))
gapminder %>% filter(year <= 2010 & country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() + geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

unique(gapminder$year)

###############################
## NORE EXAMPLES ON VISUALIZATION PRINCIPLES

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(fill=sex),alpha = 0.1)

# note the aes on histogeram to plot density instead of count
heights %>% ggplot(aes(height)) + geom_histogram(aes(y=stat(density)),binwidth=1,col="black",fill="blue") +
  facet_grid(rows=heights$sex,scales="free_y")
# plot is similar than above but with chart trellis on top versus on the side
heights %>% ggplot(aes(height)) + geom_histogram(aes(y=stat(density)),binwidth=1,col="black",fill="blue") +
  facet_wrap(.~sex, scales="free_y", nrow = 2)

# using color palette for the coplor blind
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)


mycolors <- ifelse(levels(heights$sex)=="Male","green","red")
boxplot(height~sex, data = heights, col = mycolors)

str(gapminder)

# calculates population mean by continent
gapminder %>% filter(!is.na(population)) %>% group_by(continent) %>% 
  summarize(medi = median(population/10^6))


# recreation of plot using reorder, modified scale and other aesthethics
gapminder %>% filter(!is.na(population),year == 2015) %>% 
  mutate(continent = reorder(continent, population, FUN = median)) %>%
  ggplot(aes(continent,(population/10^6))) + geom_point(alpha = 0.4) +
  scale_y_continuous("Populations in Millions", trans = "log", breaks = c(1,10,100,1000), n.breaks = 6) +
  geom_boxplot(alpha = 0.1) + xlab("Continent")


###############################
## ADJACENT COMPARED VISUALS

gapminder <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365)

pyears <- c(1970,2010)

gapminder %>% filter(year %in% pyears & !is.na(gdp)) %>%
  ggplot(aes(continent, dollars_per_day)) + 
  geom_boxplot(aes(fill = factor(year))) +
  scale_y_continuous("Income in $ per day", trans = "log", breaks = c(1,8,64), n.breaks = 4) +
  xlab("Continent") +
  scale_fill_discrete(name = "Year")


###############################
## SLOPE CHARTS

# Code: Slope chart
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#Code: Bland-Altman plot
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

###############################
## ENCODING MULTIPLE VARIABLES

library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")
#ColorBrewer package offers several color palettes. 
#Sequential color palettes are best suited for data that span from high to low. 
#Diverging color palettes are best suited for data that are centered and diverge towards high or low values.

Code: Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

Code: Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


#Build As an example, here the per 10,000 disease rates for California across five decades.
options(digits = 3)

diseases <- c("Measless", "Pertussis", "Polio")
promedios <- us_contagious_diseases$population / us_contagious_diseases$count

us_contagious_diseases %>% filter(state == "California", year, disease %in% diseases) %>% 
  select(state, year, disease, ) %>% group_by(state, year, disease) %>%
  summarize(rate = mean(promedios))

###############################
## ASSIGNMENT #3

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

# build desnsity plots by age and sex to understand dsitributions
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

str(titanic)

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, , y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")  

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)

# build qq-plots of age and add an abline

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
p <- titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age))
p + geom_qq(dparams = params) +
  geom_abline()

#  make barplots of the Survived and Sex variables using geom_bar(). 
#Try plotting one variable and filling by the other variable. 
# You may want to try the default plot, 
#then try adding position = position_dodge() to geom_bar() to make separate bars for each group

titanic %>% ggplot(aes(Survived, y = ..count.., fill = Sex)) +
  geom_bar(stat = "count", position =  position_dodge(), width=0.9) +
  scale_y_continuous(n.breaks = 6, limits = c(0,600))

#Make a density plot of age filled by survival status. 
#Change the y-axis to count and set alpha = 0.2.

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, Y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, position = "stack") +
  scale_x_continuous(n.breaks = 10)

#Filter the data to remove individuals who paid a fare of 0. 
#Make a boxplot of fare grouped by survival status. 
#Try a log2 transformation of fares. Add the data points with jitter and alpha blending

titanic %>% filter(Fare != 0) %>% 
  ggplot(aes(Survived, Fare)) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(fill = Survived), alpha = 0.2, ) +
  scale_y_continuous(trans = "log2")
           
#The Pclass variable corresponds to the passenger class. Make three barplots. 
#For the first, make a basic barplot of passenger class filled by survival. 
#For the second, make the same barplot but use the argument 
#position = position_fill() to show relative proportions in each group instead of counts. 
#For the third, make a barplot of survival filled by passenger class using position = position_fill()

titanic %>% ggplot(aes(Pclass, y = ..count.., fill = Survived)) +
  geom_bar(width = 0.8)
titanic %>% ggplot(aes(Pclass, y = ..count.., fill = Survived)) +
  geom_bar(width = 0.8, position = position_fill())
titanic %>% ggplot(aes(Survived, y = ..count.., fill = Pclass)) +
  geom_bar(width = 0.8, position = position_fill()) 


#Create a grid of density plots for age, filled by survival status, 
#with count on the y-axis, faceted by sex and passenger class.

titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(na.rm = TRUE, alpha = 0.6, position = "stack") +
  facet_grid(Sex ~ Pclass)
  

###############################
## OTHER DATASETS TO LEARN FROM

library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)



###############################
##

###############################
##
