library(tidyverse) # data manipulations + plots
library(mfx) # marginal effects

# let's use old data frame with diamond prices
# let's create binary variable :) :)
df2 <- mutate(diamonds, 
    ideal = ifelse(cut == "Ideal", 1, 0))
glimpse(df2)

effects <- logitmfx(data = df2, 
          ideal ~ carat + price)
effects # marginal effects for average diamond

effects_av <- logitmfx(data = df2, atmean = FALSE,
                    ideal ~ carat + price)
effects_av # marginal effects averaged over all diamonds

summary(effects$fit) # beta hat and short summary

# let's create a small new data frame
new_diamonds <- tibble(carat = c(1, 0.5), price = c(1500, 800))
new_diamonds

# let's predict probabilities:
predict(effects$fit, newdata = new_diamonds, type = "response")

# confidence interval for beta:
confint(effects$fit)


