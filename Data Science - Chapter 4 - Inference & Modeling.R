## DATA SCIENCE ONLINE TRAINING - HARVARD edX

#################################################################################
## CHAPTER 4. INFERENCE & MODELING
#################################################################################

setwd("~/Documents/Trainings/DataScienceTraining")

###############################
## Sampling Model Parameters and Estimates

library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)    # draw 25 beads

#*******************************
# Exercise 5. se versus p
# Write a line of code that calculates the standard error se of a sample average when you poll 25 people 
# in the population. Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) 
# to 1 (all Democrats).

# Plot se versus p for the 100 different proportions.
N <- 25 # `N` represents the number of people polled
p <- seq(0, 1, length = 100) # variable `p` that contains 100 proportions ranging from 0 to 1 
se <- sqrt(p*(1-p)/N) # Create a variable `se` that contains the standard error of each sample average
plot(p, se) # Plot `p` on the x-axis and `se` on the y-axis

#*******************************
# Exercise 6. Multiple plots of se versus p
# Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se 
# when the sample sizes equal N=25, N=100, and N=1000.
p <- seq(0, 1, length = 100) # variable `p` that contains 100 proportions ranging from 0 to 1 
sample_sizes <- c(25, 100, 1000) # The vector `sample_sizes` contains the three sample sizes
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
} # for-loop that calculates the standard error `se` for every value of `p` for each sample. Then plots using limits

#*******************************
# Exercise 9. Standard error of the spread
# Say the actual proportion of Democratic voters is p=0.45. 
# In this case, the Republican party is winning by a relatively large margin of d=−0.1, or a 10% margin of victory. 
# What is the standard error of the spread 2X¯−1 in this case?
N <- 25 # `N` represents the number of people polled
p <- 0.45 # `p` represents the proportion of Democratic voters
2*sqrt(p*(1-p)/N) # Calculate the standard error of the spread

###############################
## CENTRAL LIMIT THEOREM

# Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000
# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

#Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#*******************************

#Exercise 1. Sample average
#Write function called take_sample that takes the proportion of Democrats p and the sample size N as 
#arguments and returns the sample average of Democrats (1) and Republicans (0).

#Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.

# Write a function called `take_sample` that takes `p` and `N` as arguements 
# and returns the average value of a randomly sampled population.
take_sample <- function(p, N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1 - p, p))
  mean(X)
}
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

#*******************************
# Exercise 2. Distribution of errors - 1
# Assume the proportion of Democrats in the population p equals 0.45 and that your sample size N is 100 polled voters. 
# The take_sample function you defined previously generates our estimate, X¯.

# Replicate the random sampling 10,000 times and calculate p−X¯ for each random sample. 
# Save these differences as a vector called errors. Find the average of errors and plot a histogram of the distribution.

p <- 0.45 # Define `p` as the proportion of Democrats in the population being polled
N <- 100 # Define `N` as the number of people polled
B <- 10000 # The variable `B` specifies the number of times we want the sample to be replicated
set.seed(1)
errors <- replicate(B, p - take_sample(p, N)) # replicates subtracting the result of the `take_sample` function from `p` for `B` replications
mean(errors) # Calculate the mean of the errors. Print this value to the console.

#*******************************
# Exercise 4. Average size of error
# The error p−X¯ is a random variable. In practice, the error is not observed because we do not know the 
# actual proportion of Democratic voters, p. However, we can describe the size of the error by constructing a simulation.

# What is the average size of the error if we define the size by taking the absolute value ∣p−X¯∣ ?

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))

#*******************************
The standard error is related to the typical size of the error we make when predicting. We say size because, as we just saw, the errors are centered around 0. In that sense, the typical error is 0. For mathematical reasons related to the central limit theorem, we actually use the standard deviation of errors rather than the average of the absolute values.

As we have discussed, the standard error is the square root of the average squared distance (X¯−p)2. The standard deviation is defined as the square root of the distance squared.

Calculate the standard deviation of the spread.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

#*******************************
Exercise 6. Estimating the standard error
The theory we just learned tells us what this standard deviation is going to be because it is the standard error of X¯.

Estimate the standard error given an expected value of 0.45 and a sample size of 100.

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p*(1-p)/N)

#*******************************
# Exercise 7. Standard error of the estimate
# In practice, we don't know p, so we construct an estimate of the theoretical prediction based 
# by plugging in X¯ for p. Calculate the standard error of the estimate: SE^(X¯)

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

#*******************************
Exercise 8. Plotting the standard error
The standard error estimates obtained from the Monte Carlo simulation, the theoretical prediction, and the estimate of the theoretical prediction are all very close, which tells us that the theory is working. This gives us a practical approach to knowing the typical error we will make if we predict p with X̂ . The theoretical result gives us an idea of how large a sample size is required to obtain the precision we need. Earlier we learned that the largest standard errors occur for p=0.5.

Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, how large does the sample size have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(se)

#*******************************
Exercise 11. Plotting the errors
Make a qq-plot of the errors you generated previously to see if they follow a normal distribution.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)

#*******************************
Exercise 12. Estimating the probability of a specific value of X-bar
If p=0.45 and N=100, use the central limit theorem to estimate the probability that X¯>0.5.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))

#*******************************
Exercise 13. Estimating the probability of a specific error size
# Assume you are in a practical situation and you don't know p. 
# Take a sample of size N=100 and obtain a sample average of X¯=0.51.

# What is the CLT approximation for the probability that your error size is equal or larger than 0.01?
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)


###############################
## CONFIDENCE INTERVALS

#Code: geom_smooth confidence interval example
#The shaded area around the curve is related to the concept of confidence intervals.

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")
#geom_smooth() using method = loess

#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

#Code: Solving for  𝑧  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

#Code: Monte Carlo simulation
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#Code: Confidence interval for the spread with sample size of 25
#Note that to compute the exact 95% confidence interval, we would use c(-qnorm(.975), qnorm(.975)) instead of 1.96.

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)



###############################
##

###############################
##



#*******************************

#*******************************

#*******************************

#*******************************