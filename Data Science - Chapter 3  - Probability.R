## DATA SCIENCE ONLINE TRAINING - HARVARD edX

#################################################################################
## CHAPTER 3. PROBABILITY
#################################################################################

setwd("~/Documents/Trainings/R for Analytics")

###############################
## MONTE CARLO SIMULATIONS

Monte Carlo simulations model the probability of different outcomes by repeating 
a random process a large enough number of times that the results are similar to 
what would be observed if the process were repeated forever.

options(digits = )   # report 3 significant digits
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 100000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times with replacement
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

B <-  100000
events <-  sample(beads, B, replace = TRUE)
tab <- table(events)
#tab
prop.table(tab)

set.seed(1, sample.kind="Rounding") 
set.seed(1986)

mean(beads == "red") # calculates de probability of ocurrences from a vector
p <- sample(beads,5)
p[2:5]

cyan <- 3
magenta <- 5
yellow <- 7
p_1 <- cyan / (cyan + magenta + yellow)
p_2 <- (magenta + yellow) / ((cyan + magenta + yellow)-1)
p_2 * p_1

###############################
## COMBINATIONS & PERMUTATIONS

Code: Introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
deck
# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

Code: Permutations and combinations
Correction: The code shown does not generate all 7 digit phone numbers because phone numbers can have repeated digits. 
It generates all possible 7 digit numbers without repeats.

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
mean(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
facecard

hands <- combinations(52, 2, v=deck) # all possible hands
hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard) #opt1
mean(hands[,1] %in% facecard & hands[,2] %in% aces) #opt2

# probability of a natural 21 checking for both ace first and ace second - Addition Rule p(A)+p(B)-P(AB)
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))


Code: Monte Carlo simulation of natural 21 in blackjack
Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

Code: The birthday problem. Probability of 2 people having the same birthday in a group of 50 people
# checking for duplicated bdays in one 50 person group
# calculates proportion of groups with duplicated bdays

###############################
## SAPPLY

Code: Function for birthday problem Monte Carlo simulations
Note that the function body of compute_prob() is the code that we wrote in the previous video. If we write this code as a function, we can use sapply() to apply this function to several values of n.

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n <- seq(1, 60)

Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob) 
grid(lty="dotted")

Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

###############################
## HOW MANY NUMBER OF MONTE CARLO EXPERIMENTS ARE ENOUGH ?

Using the birthday example with n individuals

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 30){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
compute_prob(3)
prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates


###############################
## THE MONTY HALL PROBLEM

Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

# probando
doors <- as.character(1:3)
prize <- sample(c("car","goat","goat"))    # puts prizes in random order
prize_door <- doors[prize == "car"]    # note which door has prize
my_pick  <- sample(doors, 1)    # note which door is chosen
show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
stick <- my_pick    # stick with original door
stick == prize_door    # test whether the original door has the prize

doors
prize
prize_door
my_pick
show
stick

switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
switch
switch == prize_door    # test whether the switched door has the prize


###############################
## ASSESSMENT

EXERCISE 1. The Cavs and the Warriors

Two teams, say the Cavs and the Warriors, are playing a seven game championship series. 
The first to win four games wins the series. 
The teams are equally good, so they each have a 50-50 chance of winning each game.
If the Cavs lose the first game, what is the probability that they win the series?

# Assign a variable 'n' as the number of remaining games.
n <- 6
# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 
# 1 indicates a win for the Cavs.
outcomes <- c(0,1)
# Assign a variable `l` to a list of all possible outcomes in all remaining games. 
# Use the `rep` function on `list(outcomes)` to create list of length `n`. 
l <- rep(list(outcomes), n)
# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)
# Create a vector named 'results' that indicates whether each row in the 
# data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)>=4
# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

Confirm the results of the previous question with a Monte Carlo simulation to estimate 
the probability of the Cavs winning the series after losing the first game.

# The variable `B` specifies the number of times we want the simulation to run. 
# Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Create an object called `results` that replicates for `B` iterations a simulated series and 
# determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  cavs_wins <- sample(c(0,1), 6, replace = TRUE)
  sum(cavs_wins)>=4 
})
# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. 
# Print your answer to the console. 
mean(results)

###############################

EXERCISE 2
A and B play a series - part 1. Two teams, A and B, are playing a seven series game series. 
Team A is better than team B and has a p>0.5 chance of winning each game.

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)
# Given a value 'p', the probability of winning the series for the underdog team B can be computed 
# with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
# Apply the 'prob_win' function across the vector of probabilities that team A 
# will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)
# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 
and compute the probability for different series lengths. 
For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.

# Given a value 'p', the probability of winning the series for the underdog team $B$ can be computed 
# with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1, 25, 2)
# Apply the 'prob_win' function across the vector of series lengths to determine the probability 
# that team B will win. Call this object `Pr`.
Pr<- sapply(N, prob_win)
# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)


EXERCISE 3
In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). 
In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries
Using Monte Carlo, calculate the probability that all the runners are from Jamaica.

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
b = 10000
runs <- replicate(b,sample(runners,3))
mean(runs[1,]=="Jamaica" & runs[2,]=="Jamaica" & runs[3,]=="Jamaica")


EXERCISE 4
A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day 
of the year. He does not think his current special actually allows that number of choices, but wants to change 
his special if needed to allow at least 365 choices.
A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. 
He currently offers a choice of 1 entree from a list of 6 options, 
a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

# Write a function that takes a number of entree choices and returns the number of meal combinations 
# possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.

myfun <- function(e){
  e*(length(combinations(6,2))/2)*3
}
x <- 1:12
sapply(x,myfun)

# Write a function that takes a number of side choices and returns the number of meal combinations 
# possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.

myfun <- function(s){
  6*(length(combinations(s,2))/2)*3
}
x <- 2:12
sapply(x,myfun)

EXERCISE 5
Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. 
The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer 
(cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully 
matched on a variety of demographic and medical characteristics. 
The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases 
and controls grouped by age range (agegp).

library(tidyverse)
str(esoph)
head(esoph)

# How many groups are in the study?
nrow(esoph)

# How many cases are there?
all_cases <- sum(esoph$ncases)
sum(esoph[,4])

# How many controls are there?
all_controls <- sum(esoph$ncontrols)
sum(esoph[,5])

# What is the probability that a subject in the highest (or lowest) alcohol consumption group is a cancer case?
table <- esoph %>% group_by(alcgp) %>% 
  summarize(ncases = sum(ncases),ncontrols = sum(ncontrols),n())
grupo <- table$alcgp[1]
sum(esoph[which(esoph[,2] == grupo),4]) / (sum(esoph[which(esoph[,2] == grupo),4]) + sum(esoph[which(esoph[,2] == grupo),5]))

#Answer:
  esoph %>%
    #filter(alcgp == "120+") %>% 
    filter(alcgp == "0-39g/day") %>%
    summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
    mutate(p_case = ncases / (ncases + ncontrols)) %>%
    pull(p_case)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
table <-  esoph %>% 
  group_by(tobgp) %>% 
  summarize(ncases = sum(ncases),ncontrols = sum(ncontrols),n())
grupo <- table$tobgp[1]
prob <- sum(esoph[which(esoph[,3] == grupo),4]) / all_cases
1 - prob

#Answer:
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()
tob_cases/all_cases


# Given that a person is a control, what is the probability that they smoke 10g or more a day?
table <-  esoph %>% 
  group_by(tobgp) %>% 
  summarize(ncases = sum(ncases),ncontrols = sum(ncontrols),n())
grupo <- table$tobgp[1]
prob <- sum(esoph[which(esoph[,3] == grupo),5]) / all_controls
1 - prob

#Answer:
tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
tob_controls/all_controls

# For cases, what is the probability of being in the highest alcohol group?
table <- esoph %>% group_by(alcgp) %>% 
  summarize(ncases = sum(ncases),ncontrols = sum(ncontrols),n())
grupo <- table$aclgp[4]
sum(esoph[which(esoph[,2] == grupo),4]) / all_cases

alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
alc_cases / all_cases
alc_cases

tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
tob_cases / all_cases
tob_cases

# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
high_cases <-  esoph %>%
  filter(alcgp == "120+",tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
high_cases / all_cases
high_cases


# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
(alc_cases + tob_cases - high_cases) / all_cases 


alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
alc_cases / all_controls
tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
tob_cases / all_controls
high_cases <-  esoph %>%
  filter(alcgp == "120+",tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
high_cases / all_controls
(alc_cases + tob_cases - high_cases) / all_controls

###############################
## CONTINUOUS PROBABILITY

Code: Cumulative distribution function
Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x <- heights %>% filter(sex=="Male") %>% .$height

Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

Code: Using pnorm() to calculate probabilities
Given male heights x, We can estimate the probability that a male is taller than 70.5 inches using:
  
1 - pnorm(70.5, mean(x), sd(x))

Code: Discretization and the normal approximation
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

Plotting the probability density for the normal distribution
We can use dnorm() to plot the density curve for the normal distribution. dnorm(z) gives the probability density  𝑓(𝑧)
of a certain z-score, so we can draw a curve by calculating the density over a range of possible values of z.

First, we generate a series of z-scores covering the typical range of the normal distribution. 
Since we know 99.7% of observations will be within  −3≤𝑧≤3 , we can use a value of  𝑧  slightly larger than 
3 and this will cover most likely values of the normal distribution. Then, we calculate  𝑓(𝑧) 
, which is dnorm() of the series of z-scores. Last, we plot  𝑧  against  𝑓(𝑧) .

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()


Here is the resulting plot:
  
Plot of the normal distribution generated using the dnorm function.
Note that dnorm() gives densities for the standard normal distribution by default. 
Probabilities for alternative normal distributions with mean mu and standard deviation sigma can be evaluated with:
  
dnorm(z, mu, sigma)

Key points
rnorm(n, avg, s) generates n random numbers from the normal distribution with average avg and standard deviation s.
By generating random numbers from the normal distribution, we can simulate height data with similar properties to our dataset. 
Here we generate simulated height data using the normal distribution.

Code: Generating normally distributed random numbers

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

Code: Monte Carlo simulation of tallest person over 7 feet

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

# OTHER DISTRIBUTIONS

Key points

You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) 
for many of these distributions.
Each distribution has a matching abbreviation (for example, norm() or t()) that is paired with the related function 
abbreviations (d, p, q, r) to create appropriate functions.
For example, use rt() to generate random numbers for a Monte Carlo simulation using the Student t distribution.


Code: Plotting the normal distribution with dnorm
Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution 
(abbreviation norm()):
  
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

Exercise 7. Distribution of IQ scores
The distribution of IQ scores is approximately normally distributed. 
The average is 100 and the standard deviation is 15. 
Suppose you want to know the distribution of the person with the highest IQ in your school district, 
where 10,000 people are born each year.
Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)
# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
})
# Make a histogram of the highest IQ scores.
hist(highestIQ)


# ACT EXERCISE. ASSESSMENT
#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 
#and standard deviation of 5.7. Save these values as act_scores. 
#You'll be using this dataset throughout these four multi-part questions.

# What is the mean  and sd of act_scores?
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)

# A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)

# In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <= 10)

# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the 
# probability density function over x given a mean of 20.9 and standard deviation of 5.7; 
# save the result as f_x. Plot x against f_x.
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

#Convert act_scores to Z-scores. Recall from Data Visualization (the second course in this series) that to 
#standardize values (convert values into Z-scores, that is, values distributed with a mean of 0 and 
#standard deviation of 1), you must subtract the mean and then divide by the standard deviation. 
#Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.

# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
z_scores <- scale(act_scores)
mean(z_scores > 2)

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores) + mean(act_scores)

# A Z-score of 2 corresponds roughly to the 97.5th percentile.
# Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and 
# standard deviation observed in act_scores.

# What is the 97.5th percentile of act_scores?
qnorm(.975, mean(act_scores), sd(act_scores))

# Write a function that takes a value and produces the probability of an ACT score less than or 
# equal to that value (the CDF). Apply this function to the range 1 to 36.
# What is the minimum integer score such that the probability of that score or lower is at least .95?
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

# Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving 
# that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.

# What is the expected 95th percentile of ACT scores?
qnorm(.95, 20.9, 5.7)

# As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of 
# the act_scores data. Save these as sample_quantiles.

# In what percentile is a score of 26?
# Your answer should be an integer (i.e. 60), not a percent or fraction. 
#Note that a score between the 98th and 99th percentile should be considered the 98th percentile, 
#for example, and that quantile numbers are used as names for the vector sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) 
# with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. 
# Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()


###############################
## RANDOM VARIABLES

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


###############################
## SAMPLING MODELS

Monte Carlo simulation: Chance of casino losing money on roulette
We build a sampling model for the random variable  𝑆  that represents the cainos total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.
BINOMIAL DISTRIBUTION >> (S + n)/2

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

###############################
## CENTRAL LIMIT THEOREM

B <- 10^6
X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))
mean(X)

mu <- n * (20-18)/38
se <- sqrt(n) * 2 * sqrt(90)/19
S <- pnorm(0,mu,se)
mean(S)
sd(S)

###############################
## EXERCISES

# Exercise 1. American Roulette probabilities
# An American roulette wheel has 18 red, 18 black, and 2 green pockets. 
# Each red and black pocket is associated with a number from 1 to 36. 
# The two remaining green slots feature "0" and "00". 
# Players place bets on which pocket they think a ball will land in after the wheel is spun. 
# Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).

# What are the chances that the ball lands in a green pocket?
  
green <- 2
black <- 18
red <- 18
p_green <- green / (green+black+red)

#*******************************

# Exercise 2. American Roulette payout
# In American roulette, the payout for winning on green is $17. 
# This means that if you bet $1 and it lands on green, you get $17 as a prize.

# Create a model to predict your winnings from betting on green one time.

p_not_green <- 1-p_green
X <- sample(c(17,-1), 1, prob = c(p_green, p_not_green))
mean(X)

#*******************************

# Exercise 3 & 4. American Roulette expected value
# In American roulette, the payout for winning on green is $17. 
# This means that if you bet $1 and it lands on green, you get $17 as a prize.
# In the previous exercise, you created a model to predict your winnings from betting on green.

# Now, compute the expected value of X and standard error, the random variable you generated previously.

p_not_green <- 1-p_green
# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
Ex <- 17*p_green + -1*p_not_green
SEx <- abs((17 - -1))*sqrt(p_green*p_not_green)
Ex
SEx

#*******************************

Exercise 5 & 6 & 7. American Roulette sum of winnings
You modeled the outcome of a single spin of the roulette wheel, X, in exercise 2.

Now create a random variable S that sums your winnings after betting on green 1,000 times.

n <- 1000
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
S <- sum(X)
S

Es <- n * 17*p_green + -1*p_not_green
SEs <- sqrt(n) * abs((17 - -1))*sqrt(p_green*p_not_green)
Es
SEs

#*******************************

Exercise 1. American Roulette probability of winning money
The exercises in the previous chapter explored winnings in American roulette. 
In this chapter of exercises, we will continue with the roulette example and add in the Central Limit Theorem.

In the previous chapter of exercises, you created a random variable S that is the sum of your winnings 
after betting on green a number of times in American Roulette.

What is the probability that you end up winning money if you bet on green 100 times?
  
p_green <- 2 / 38
p_not_green <- 1-p_green
n <- 100 # number of bets
# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green 
# and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green) # Compute 'se', the standard error of the sum of 100 outcomes
1 - pnorm(0, avg,se) # probability that you win money betting on green 100 times

#*******************************

Exercise 2. American Roulette Monte Carlo simulation
Create a Monte Carlo simulation that generates 10,000 outcomes of S, the sum of 100 bets.

Compute the average and standard deviation of the resulting list and compare them to the expected value (-5.263158) 
and standard error (40.19344) for S that you calculated previously.

p_green <- 2 / 38 # probability of the ball landing in a green pocket
p_not_green <- 1-p_green # probability of the ball not landing in a green pocket
n <- 100 # number of bets
B <- 10000 # times to run Monte Carlo simulatiokn
set.seed(1)
S <- replicate(B,{  
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  sum(X)
}) # Monte Carlo simulation
mean(S) # Compute the average value for 'S'
sd(S) # Calculate the standard deviation of 'S'
mean(S>0) # probability of winning money from the Monte Carlo simulation

#*******************************

Exercise 5. American Roulette average winnings per bet
Now create a random variable Y that contains your average winnings per bet after betting on green 10,000 times.

n <- 10000 # Define the number of bets using the variable 'n'
p_green <- 2 / 38 # probability of the ball landing in a green pocket
p_not_green <- 1 - p_green # probability of the ball not landing in a green pocket
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green)) # contains the outcomes of `n` bets
Y <- mean(X) # mean outcome per bet
mean(Y)

#*******************************

Exercise 6. American Roulette per bet expected value
What is the expected value of Y, the average outcome per bet after betting on green 10,000 times?
  
p_green <- 2 / 38 # probability of the ball landing in a green pocket
p_not_green <- 1 - p_green # probability of the ball not landing in a green pocket
avg <- 17*p_green + -1*p_not_green # expected outcome of `Y`, the mean outcome per bet in 10,000 bets
se <- abs((17 - -1))*sqrt(p_green*p_not_green) / sqrt(n) # standard error of Y
1 - pnorm(0, avg, se) # probability that your winnings are positive after betting on green 10,000 times?

#*******************************

Exercise 9. American Roulette Monte Carlo again
Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average outcome from 10,000 bets on green.
Compute the average and standard deviation of the resulting list to confirm the results 
from previous exercises using the Central Limit Theorem.

n <- 10000 # number of independent bets on green
B <- 10000  # number of times we want the simulation to run
S <- replicate(B,{  
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  mean(X)
}) # vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
mean(S)
sd(S)
mean(S>0) # proportion of outcomes in the vector 'S' where you won more than $0

#*******************************

SAT TESTING

An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer 
and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions 
each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

# What is the probability of guessing correctly for one question?
p <-  1/5

# What is the expected value of points for guessing on one question?
a <- 1
b <- -0.25
mu <- a*p + b*(1-p)

# What is the standard error of guessing on all 44 questions?
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))

# Use the CLT to determine the probability that a guessing student scores 8 points or higher on the test.
1 - pnorm(8,mu,sigma)

# What is the probability that a guessing student scores 8 points or higher?

set.seed(21)
B <- 10000
n <- 44
p <- 0.2
tests <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
mean(tests >= 8)

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})
min(p[which(exp_val > 0.8)])

# Exercise wining on 6 numbers

pwin <- 5/38
plose <- 1 - pwin
a <- 500*((6*pwin) + (-1*plose))
b <- abs(-1-6)*sqrt(pwin*plose)*sqrt(500)
wins <- sample(c(6,-1), size = 500, replace = TRUE, prob = c(pwin, plose))
mean(wins)
sum(wins)
pnorm(0,a,b)

###############################
## THE BIG SHORT CASE

# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Code: Calculating interest rates for expected value of 0
# We can calculate the amount  𝑥  to add to each loan so that the expected value is 0 using the
# equation  𝑙𝑝+𝑥(1−𝑝)=0 . Note that this equation is the definition of expected value given a 
# loss per foreclosure  𝑙  with foreclosure probability  𝑝  and profit  𝑥  if there is
# no forecloure (probability  1−𝑝 ).

# We solve for  𝑥=−𝑙𝑝1−𝑝  and calculate  𝑥 :
x = - loss_per_foreclosure*p/(1-p)
x

# On a $180,000 loan, this equals an interest rate of:
x/180000

# Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Code: Monte Carlo simulation for 1% probability of losing money
# Note that your results will vary from the video because the seed is not set.
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

# Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Code: Calculating number of loans for desired probability of losing money
# The number of loans required is:
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Code: Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of default  𝑝=0.04 .
# Note that your results will differ from the video because the seed is not set.
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#Code: Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown probability of default  0.03≤𝑝≤0.05 ,
# modeling the situation where an event changes the probability of default for all borrowers simultaneously. 
# Note that your results will differ from the video because the seed is not set.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

#*******************************

# Exercise 1. Bank earnings
# Say you manage a bank that gives out 10,000 loans. 
# The default rate is 0.03 and you lose $200,000 in each foreclosure.

# Create a random variable S that contains the earnings of your bank. 
# Calculate the total amount of money lost in this scenario.
n <- 10000
loss_per_foreclosure <- -200000
p_default <- 0.03
defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE)
S <- sum(defaults * loss_per_foreclosure)

# Exercise 2. Bank earnings Monte Carlo
# Run a Monte Carlo simulation with 10,000 outcomes for S, 
# the sum of losses over 10,000 loans. Make a histogram of the results.

B <- 10000
S <- replicate(B, {
  defaults <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
library(tidyverse)
data.frame(losses_in_millions = S/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")
hist(S)

# Exercise 3 & 4. Bank earnings expected value
# What is the expected value of S, the sum of losses over 10,000 loans? and standard error
# For now, assume a bank makes no money if the loan is paid.
n <- 10000
n*(p_default*loss_per_foreclosure + (1-p_default)*0)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))

# Exercise 5. Bank earnings interest rate - 1
# So far, we've been assuming that we make no money when people pay their loans and we lose a lot of money 
# when people default on their loans. Assume we give out loans for $180,000. 
# How much money do we need to make when people pay their loans so that our net loss is $0?

# In other words, what interest rate do we need to charge in order to not lose money?
x = - loss_per_foreclosure*p_default/(1-p_default)
x/180000

# Exercise 6. Bank earnings interest rate - 2
# With the interest rate calculated in the last example, we still lose money 50% of the time. 
# What should the interest rate be so that the chance of losing money is 1 in 20?
# In math notation, what should the interest rate be so that Pr(S<0)=0.05?
#Remember that we can add a constant to both sides of the equation to get:
#   Pr(S−E[S]SE[S]<−E[S]SE[S])
# which is
#   Pr(Z<−[lp+x(1−p)]n(x−l)np(1−p)‾‾‾‾‾‾‾‾‾√)=0.05
# Let z = qnorm(0.05) give us the value of z for which:
#   Pr(Z≤z)=0.05
z <- qnorm(1/20)
x <- -loss_per_foreclosure*(n*p_default - z*sqrt(n*p_default*(1-p_default))) / (n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))
x/180000

#Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))


#*******************************
#Exercise 1. Confidence interval for p
#For the following exercises, we will use actual poll data from the 2016 election. 
#The exercises will contain pre-loaded data from the dslabs package.
library(dslabs)
data("polls_us_election_2016")
#We will use all the national polls that ended within a few weeks before the election.
#Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p.

# Load the data
data("polls_us_election_2016")
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# For the first poll in `polls`, convert the percentage to a proportion of Clinton voters and assign it to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)

#*******************************
#Exercise 2. Pollster results for p
#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, 
#the proportion of voters who declared a vote for Clinton, the standard error of this estimate, 
#and the lower and upper bounds of the confidence interval for the estimate.

# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)

#*******************************
#Exercise 3. Comparing to actual results - p
#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. 
#Add a column called hit to pollster_results that states if the confidence interval included the 
#true proportion p=0.482 or not. What proportion of confidence intervals included p?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = lower<=0.482 & upper>=0.482) %>% summarize(mean(hit))


#*******************************
#Exercise 5. Confidence interval for d
#A much smaller proportion of the polls than expected produce confidence intervals containing p. Notice that most polls that fail to include p are underestimating. The rationale for this is that undecided voters historically divide evenly between the two main candidates on election day.
#In this case, it is more informative to estimate the spread or the difference between the proportion of two candidates d, or 0.482−0.461=0.021 for this election.
#Assume that there are only two parties and that d=2p−1. Construct a 95% confidence interval for difference in proportions on election night.

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)

#*******************************
#Exercise 6. Pollster results for d
#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the difference in the proportion of voters who declared a vote either, and the lower and upper bounds of the confidence interval for the estimate.

# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <-  polls %>% mutate(X_hat = (d_hat+1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = d_hat - qnorm(0.975)*se_hat, upper = d_hat + qnorm(0.975)*se_hat) %>% select(pollster, enddate, d_hat, lower, upper)

#*******************************
#Exercise 7. Comparing to actual results - d
#What proportion of confidence intervals for the difference between the proportion of voters included d, the actual difference in election day?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = lower<=0.021 & upper>=0.021) %>% summarize(mean(hit))
  
#*******************************
#Exercise 8. Comparing to actual results by pollster
#Although the proportion of confidence intervals that include the actual difference between the proportion of voters increases substantially, it is still lower that 0.95. In the next chapter, we learn the reason for this.
#To motivate our next exercises, calculate the difference between each poll's estimate d¯ and the actual d=0.021. Stratify this difference, or error, by pollster in a plot.

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% 
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#*******************************
#Exercise 9. Comparing to actual results by pollster - multiple polls
#Remake the plot you made for the previous exercise, but only for pollsters that took five or more polls.
#You can use dplyr tools group_by and n to group data by a variable of interest and then count the number of observations in the groups. The function filter filters data piped into it by your specified condition.
#For example:
data %>% group_by(variable_for_grouping) 
%>% filter(n() >= 5)

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###############################
##  POLL AGGREGATORS

#Code: Simulating polls
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Code: Calculating the spread of combined polls
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

###############################
## POLL DATA & POLLSTER BIAS

#Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

#Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))


###############################
## DATA-DRIVEN MODELS

#Code 
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

#*******************************
#Exercise 1 - Heights Revisited
#We have been using urn models to motivate the use of probability models. However, most data science applications are not related to data obtained from urns. More common are data that come from individuals. Probability plays a role because the data come from a random sample. The random sample is taken from a population and the urn serves as an analogy for the population.
#Let's revisit the heights dataset. For now, consider x to be the heights of all males in the data set. Mathematically speaking, x is our population. Using the urn analogy, we have an urn with the values of x in it.
#What are the population average and standard deviation of our population?

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Calculate the population average. Print this value to the console.
mean(x)
# Calculate the population standard deviation. Print this value to the console.
sd(x)

#*******************************
#Exercise 2 - Sample the population of heights
#Call the population average computed above μ and the standard deviation σ. Now take a sample of size 50, with replacement, and construct an estimate for μ and σ.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#*******************************
#Exercise 4 - Confidence Interval Calculation
#We will use X¯ as our estimate of the heights in the population from our sample size N. We know from previous exercises that the standard estimate of our error X¯−μ is σ/N‾‾√.
#Construct a 95% confidence interval for μ.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)

#*******************************
#Exercise 5 - Monte Carlo Simulation for Heights
#Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. What proportion of these intervals include μ?

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

#*******************************
#Exercise 6 - Visualizing Polling Bias
#In this section, we used visualization to motivate the presence of pollster bias in election polls. Here we will examine that bias more rigorously. Lets consider two pollsters that conducted daily polls and look at national polls for the month before the election.
#Is there a poll bias? Make a plot of the spreads for each poll.

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) + 
  geom_boxplot() + 
  geom_point()


###############################
## BAYES' THEOREM

#Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

#*******************************
#Exercise 6 - Back to Election Polls
#Florida is one of the most closely watched states in the U.S. election because it has many electoral votes and the election is generally close. Create a table with the poll spread results from Florida taken during the last days before the election using the sample code.

#The CLT tells us that the average of these spreads is approximately normal. Calculate a spread average and provide an estimate of the standard error.

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg = mean(spread),  se = sd(spread)/sqrt(n()))
results


###############################
## ELECTION FORECASTING

#Code: Definition of results object
#This code from previous videos defines the results object used for empirical Bayes election forecasting.

library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#Code: Computing the posterior mean, standard error, credible interval and probability
#Note that to compute an exact 95% credible interval, we would use qnorm(.975) instead of 1.96.

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)


###############################
## MATHEMATICAL REPRESENTATION OF MODELS

#Code: Simulated data with  𝑋𝑗=𝑑+𝜖𝑗 
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#Code: Simulated data with  𝑋𝑖,𝑗=𝑑+𝜖𝑖,𝑗 
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Code: Simulated data with  𝑋𝑖,𝑗=𝑑+ℎ𝑖+𝜖𝑖,𝑗 
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Code: Calculating probability of  𝑑>0  with general bias
#Note that sigma now includes an estimate of the variability due to general bias  𝜎𝑏=.025 .

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

###############################
## PREDICTING THE ELECTORAL COLLEGE

#Code: Top 5 states ranked by electoral votes
#The results_us_election_2016 object is defined in the dslabs package:
  
  library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#Code: Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#Code: Calculating the posterior mean and posterior standard error
#Note there is a small error in the video code: B should be defined as sigma^2/(sigma^2 + tau^2).

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#Code: Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

###############################
## FORECASTING

#Code: Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#Code: Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#Code: Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))




#*******************************
#Exercise 1 - Confidence Intervals of Polling Data
#For each poll in the polling data set, use the CLT to create a 95% confidence interval for the spread. Create a new table called cis that contains columns for the lower and upper limits of the confidence intervals.

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that columns for the lower and upper confidence intervals. Select the columns indicated in the instructions.
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

#*******************************
#Exercise 2 - Compare to Actual Results
#You can add the final result to the cis table you just created using the left_join function as shown in the sample code.
#Now determine how often the 95% confidence interval includes the actual result.

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))
p_hits

#*******************************
#Exercise 3 - Stratify by Pollster and Grade
#Now find the proportion of hits for each pollster. Show only pollsters with at least 5 polls and order them from best to worst. Show the number of polls conducted by each pollster and the FiveThirtyEight grade of each pollster.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls. 
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))
p_hits


#*******************************
#Exercise 4 - Stratify by State
#Repeat the previous exercise, but instead of pollster, stratify by state. Here we can't show grades.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls. 
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits)) 
p_hits

#*******************************
#Exercise 5- Plotting Prediction Results
#Make a barplot based on the result from the previous exercise.

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()


#*******************************
#Exercise 6 - Predicting the Winner
#Even if a forecaster's confidence interval is incorrect, the overall predictions will do better if they correctly called the right winner.
#Add two columns to the cis table by computing, for each poll, the difference between the predicted spread and the actual spread, and define a column hit that is true if the signs are the same.

# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)

#*******************************
#Exercise 7 - Plotting Prediction Results
#Create an object called p_hits that contains the proportion of instances when the sign of the actual spread matches the predicted spread for states with 5 or more polls.
#Make a barplot based on the result from the previous exercise that shows the proportion of times the sign of the spread matched the actual result for the data in p_hits.

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>%  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())

# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()

#*******************************
#Exercise 8 - Plotting the Errors
#In the previous graph, we see that most states' polls predicted the correct winner 100% of the time. Only a few states polls' were incorrect more than 25% of the time. Wisconsin got every single poll wrong. In Pennsylvania and Michigan, more than 90% of the polls had the signs wrong.
#Make a histogram of the errors. What is the median of these errors?

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

#*******************************
#Exercise 9- Plot Bias by State
#We see that, at the state level, the median error was slightly in favor of Clinton. The distribution is not centered at 0, but at 0.037. This value represents the general bias we described in an earlier section.
#Create a boxplot to examine if the bias was general to all states or if it affected some states differently. Filter the data to include only pollsters with grades B+ or higher.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()

#*******************************
#Exercise 10 - Filter Error Plot
#Some of these states only have a few polls. Repeat the previous exercise to plot the errors for each state, but only include states with five good polls or more.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()


###############################
## t-DISTRIBUTION

#Code: Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)



#*******************************
#Exercise 1 - Using the t-Distribution
#We know that, with a normal distribution, only 5% of values are more than 2 standard deviations away from the mean.
#Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when the degrees of freedom are 3.

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'. 
1 - pt(2, 3) + pt(-2, 3)

#*******************************
#Exercise 2 - Plotting the t-distribution
#Now use sapply to compute the same probability for degrees of freedom from 3 to 50.
#Make a plot and notice when this probability converges to the normal distribution'3s 5%.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(x) 1 - pt(2, x) + pt(-2, x)

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

#*******************************
#Exercise 3 - Sampling From the Normal Distribution
#In a previous section, we repeatedly took random samples of 50 heights from a distribution of heights. We noticed that about 95% of the samples had confidence intervals spanning the true population mean.
#Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens to the proportion of hits.

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#*******************************
#Exercise 4 - Sampling from the t-Distribution
#N=15 is not that big. We know that heights are normally distributed, so the t-distribution should apply. Repeat the previous Monte Carlo simulation using the t-distribution instead of using the normal distribution to construct the confidence intervals.
#What are the proportion of 95% confidence intervals that span the actual mean height now?

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qt(0.975, N-1)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

###############################
## ASSOCIATION TESTS

#Code: Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#Code: Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")


###############################
## CHI-SQUARE TESTS

Code: Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  nbsp;   chisq.test()
chisq_test$p.value
Code: Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women
Code: p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()



#*******************************
#Exercise 1 - Comparing Proportions of Hits
#In a previous exercise, we determined whether or not each poll predicted the correct winner for their state in the 2016 U.S. presidential election. Each poll was also assigned a grade by the poll aggregator. Now we're going to determine if polls rated A- made better predictions than polls rated C-.
#In this exercise, filter the errors data for just polls with grades A- and C-. Calculate the proportion of times each grade of poll predicted the correct winner.

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade,hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)

# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])

#*******************************
#Exercise 2 - Chi-squared Test
#We found that the A- polls predicted the correct winner about 80% of the time in their states and C- polls predicted the correct winner about 86% of the time.
#Use a chi-squared test to determine if these proportions are different.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% 
  select(-hit) %>%
  chisq.test()
chisq_test

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

#*******************************
#Exercise 3 - Odds Ratio Calculation
#It doesn't look like the grade A- polls performed significantly differently than the grade C- polls in their states.
#Calculate the odds ratio to determine the magnitude of the difference in performance between these two grades of polls.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

