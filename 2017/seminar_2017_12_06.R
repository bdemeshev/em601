library(tidyverse)

# simulate just one draw from Bin(n=2, p=0.4)
rbinom(1, size = 2, prob = 0.4)

# simulate 10000 draw from Bin(n=2, p=0.4)
x <- rbinom(10000, size = 2, prob = 0.4)
head(x) # the beginning of the vector x
tail(x) # the end of the vector x

# let us set random seed (initial state for random draws)
set.seed(25)
x <- rbinom(10000, size = 2, prob = 0.4)
head(x) # the beginning of the vector x

# access specific elements of a long vector
# x number 27 ?
x[27]
# x ranging from 20 to 50 ?
x[20:50]
# last 7 elements
tail(x, 7)
# first 10 elements
head(x, 10)


# visualise
hist(x)

mytable <- tibble(x = x)
qplot(data = mytable, x = x)

# calculate sample characteristics
mean(x) # x bar (average of x_1, x_2, ..., x_n)
sum(x)
var(x) # sample variance

# cumulative distribution function
# F(t) = P(X <= t)
pbinom(1, size = 2, prob = 0.4)
# F(1) = P(X <= 1) = 0.84

# let's solve some problems related to normal distribution
# X ~ N(exp value = 160, variance = 100)
# P(X in [150, 180]) = F(180) - F(150)
pnorm(180, mean = 160, sd = 10) - pnorm(150, mean = 160, sd = 10)

# start with the same seed on all computers
set.seed(26)
# generate 1000 draws from N(160, 100)
x <- rnorm(1000, mean = 160, sd = 10)
# look at x
head(x)

# simple table
xy_data <- tibble(x = x, y = 2 * x + 10)

# histogram for y
qplot(data = xy_data, x = y)

# extract a column from two dimensional data table
xy_data$y

# calculate sample means
mean(xy_data$x)
mean(xy_data$y)

# calculate sample variances
var(xy_data$x)
var(xy_data$y)

# calculate sample covariance
cov(xy_data$x, xy_data$y)
