library(rio)
library(tidyverse)
library(broom)

# download data from https://goo.gl/QjPF8q

# if I know exact location
flats <- import("~/Downloads/flats_moscow.txt")

# another way
# go to Session - Set working directory - Choose
setwd("~/Downloads")
flats <- import("flats_moscow.txt")

# look at our dataset
glimpse(flats)

model_a <- lm(data = flats, 
    price ~ totsp + livesp + kitsp + brick)
summary(model_a)
tidy(model_a)

new_flats <- tibble(totsp = c(60, 60),
                    livesp = c(40, 40),
                    kitsp = c(10, 10),
                    brick = c(0, 1))
new_flats

new_flats_plus <- augment(model_a, newdata = new_flats)
new_flats_plus

n_obs <- nrow(flats)
n_obs

k <- 5 # five coefficients: intercept, totsp, livsp, kitsp, brick
t_crit <- qt(0.975, df = n_obs - k)

new_flats_plus2 <- mutate(new_flats_plus, 
            left_ci = .fitted - t_crit * .se.fit,
            right_ci = .fitted + t_crit * .se.fit)
new_flats_plus2

# built-in command for confidence interval for average of y
predict(model_a, newdata = new_flats, interval = "confidence")

# built-in command for predictive interval for particular value of y
predict(model_a, newdata = new_flats, interval = "prediction")

# a lot of junk in a model :)
glance(model_a)

# confidence intervals for coefficients:
confint(model_a, level = 0.8)


