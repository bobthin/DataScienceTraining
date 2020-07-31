## DATA SCIENCE ONLINE TRAINING - HARVARD edX

#################################################################################
## CHAPTER 7. LINEAR REGRESSION
#################################################################################

###############################
## MONEYBALL CASE

# Code: Scatterplot of the relationship between HRs and wins

library(Lahman)
library(tidyverse)
library(dslabs)
library(ggplot2)
ds_theme_set()

str(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Code: Scatterplot of the relationship between stolen bases and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Code: Scatterplot of the relationship between bases on balls and runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Other plots

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  ggplot(aes(AB, R)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_line()

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point()

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(wins_per_game = W/G) %>%
  ggplot(aes(wins_per_game, E)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  ggplot(aes(X3B, X2B)) + 
  geom_point(alpha = 0.5)

###############################
## CORRELATION

# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


#*******************************
# Exercises

# What is the correlation coefficient between number of runs per game and number of at bats per game?
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(R/G, AB/G))
library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)

# What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(W/G, E/G))
cor(Teams_small$W/Teams_small$G, Teams_small$E/Teams_small$G)

# What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(X2B/G, X3B/G))
cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)

###############################
## Anscombe's Quartet/Stratification

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point(alpha = 0.5)

O;# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# example
r <- galton_heights %>% summarize(r = cor(father, son)) %>% .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# calculate values to plot regression line on original data - IMPORTANT
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# y = b + mx
# When two variables follow a bivariate normal distribution, the variation explained can be calculated as  𝜌2×100 .

vignette("ggplot2-specs") # help on aesthetics of ggplot

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

# regression lines both ways

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
# add regression line to plot
galton_heights %>%
  ggplot(aes(son, father)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_1, slope = m_1)


# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_2, slope = m_2)


#*******************************
# Assessment

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mum <- mean(female_heights$mother)
sdm <- sd(female_heights$mother)
mud <- mean(female_heights$daughter)
sdd <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * sdd/sdm
b <-  mud - m*mum 

# Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. 
#Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?

#Slope of regression line predicting daughters' height from mothers' heights
r * sdd/sdm

#Intercept of regression line predicting daughters' height from mothers' heights
mud - (r * sdd/sdm)*mum

#Change in daughter's height in inches given a 1 inch increase in the mother's height
r * sdd/sdm

#What percent of the variability in daughter heights is explained by the mother's height?
r^2

#A mother has a height of 60 inches. What is the conditional expected value of her daughter's height given the mother's height?
x <- 60
y = b + m*x
y


###############################
## LINEAR MODELS

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

###############################
## STRATIFICATION ANN MULTIVARIATE REGRESSION


# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

###############################
## LSE - LEAST SQUARES ESTIMATES

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))


# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#Although interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated, 
#which can be seen using this code:
  
lse %>% summarize(cor(beta_0, beta_1))

#However, the correlation depends on how the predictors are defined or transformed.
#Here we standardize the father heights, which changes  𝑥𝑖  to  𝑥𝑖−𝑥¯ .

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

#Observe what happens to the correlation in this case:
  
cor(lse[1,], lse[2,]) 

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#*******************************
#Exercises

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
#Run a linear model in R predicting the number of runs per game based on both the number of bases on balls per game and the number of home runs per game.
# Q. What is the coefficient for bases on balls?

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs = R/G, baseb = BB/G, homer = HR/G) %>%
  lm(runs ~ baseb + homer, data = .)

dat
summary(dat)


B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#*******************************
# Assesment

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights using daughters' heights.
# Q. What are the slope amd intercept of the model?
lm(mother ~ daughter, data = female_heights)

# Predict mothers' heights using the model.
# Q. What is the predicted height of the first mother in the dataset?
predict(fit)[1]

# Q. What is the actual height of the first mother in the dataset?
female_heights$mother[1]

#*******************************

# We have shown how BB and singles have similar predictive power for scoring runs. 
# Another way to compare the usefulness of these baseball metrics is by assessing how stable they are across the years. Because we have to pick players based on their previous performances, we will prefer metrics that are more stable. In these exercises, we will compare the stability of singles and BBs.
# Before we get started, we want to generate two tables: one for 2002 and another for the average of 1999-2001 seasons. We want to define per plate appearance statistics, keeping only players with more than 100 plate appearances. Here is how we create the 2002 table:
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


# Now compute a similar table but with rates computed over 1999-2001. 
# Keep only rows from 1999-2001 where players have 100 or more plate appearances, 
# calculate each player's single rate and BB rate per season, 
# then calculate the average single rate (mean_singles) and average BB rate (mean_bb) per player over those three seasons.
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

# Q. How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 1999-2001?
sum(bat_99_01$mean_singles > 0.2)          

# Q. How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
sum(bat_99_01$mean_bb > 0.2)

# Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you 
# created in the previous question.
dat <- inner_join(bat_02, bat_99_01)

# Q. What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(dat$singles, dat$mean_singles)

# Q. What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
cor(dat$bb, dat$mean_bb)

dat %>% ggplot(aes(mean_singles, singles)) +
  geom_point() +
  geom_smooth(method = "lm")

dat %>% ggplot(aes(mean_bb, bb)) +
  geom_point() +
  geom_smooth(method = "lm")

# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
# Q. What is the coefficient of mean_singles, the slope of the fit?
lm(singles ~ mean_singles, data = dat)

# Q. Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
lm(bb ~ mean_bb, data = dat)

library(car)
scatterplot(mean_singles ~ singles, data = dat)

#*******************************
#*******************************
#*******************************
###############################
##
###############################
##