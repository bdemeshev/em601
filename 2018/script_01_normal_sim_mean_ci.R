# exer 1
# Z ~ N(0,1)
# a) P(Z > 0.7)
# cumulative distribution function
# F(t) = P(Z < t) = pnorm(t)
1 - pnorm(0.7)
# b) P(0.7 < Z < 0.9)
pnorm(0.9) - pnorm(0.7)
# c) c? P(Z > c) = 0.97
qnorm(0.03)
qnorm(1 - 0.97)
# d) d? P(-d < Z < d) = 0.90
qnorm(0.95)
- qnorm(0.05)

# exer 2
# Z ~ N(1,4), so mean = 1, and standard deviation /sd/ = 2
# a) P(Z > 0.7)
# cumulative distribution function
# F(t) = P(Z < t) = pnorm(t)
1 - pnorm(0.7, mean = 1, sd = 2)
# b) P(0.7 < Z < 0.9)
pnorm(0.9, mean = 1, sd = 2) - pnorm(0.7, mean = 1, sd = 2)
# c) c? P(Z > c) = 0.97
qnorm(0.03, mean = 1, sd = 2)

library(tidyverse) # attach the package tidyverse
# if error (no package called)
# then Tools - Install packages - tidyverse - OK

head(diamonds)
?diamonds
glimpse(diamonds)

qplot(data = diamonds, x = price)
qplot(data = diamonds, x = log(price))

qplot(data = diamonds, x = log(carat))
qplot(data = diamonds, x = carat, 
      y = price, color = cut)

# sample mean of price
mean(diamonds$price)
mu_hat = mean(diamonds$price)
# sample standard deviation
sd(diamonds$price)
sigma_hat = sd(diamonds$price)
# number of rows = number of observations
n = nrow(diamonds)
n
# se(mu_hat)
se_mu_hat = sigma_hat / sqrt(n)
# for 90% Confidence Interval:
z_crit = 1.65
left_ci = mu_hat - z_crit * se_mu_hat
right_ci = mu_hat + z_crit * se_mu_hat
left_ci
right_ci

# create a new variable in the table
d2 = mutate(diamonds, ln_price = log(price))
glimpse(d2)

# filter observations
d3 = filter(d2, price < mean(price))
glimpse(d3)
