library(tidyverse) # data manipulations + plots
library(lmtest) # tests: White, Goldfeld-Quandt etc
library(sandwich) # se_HC corrections
library(mfx) # logistic regression and marginal effects

# clear heteroskedasticity:
# range of price grows with carat
qplot(data = diamonds,
      y = price, 
      x = carat)

model <- lm(data = diamonds, 
            price ~ carat + x + y + z)

summary(model) # a lot of info

coeftest(model) # under homoskedasticity, 
# se(hat beta) are not corrected by default


coeftest(model, vcov. = vcovHC) # under heteroskedasticity, 
# se(hat beta) are corrected!

# Goldfeld-Quandt test
gqtest(data = diamonds, price ~ carat + x + y + z, 
       fraction = 0.2, order.by = ~ carat)
# H0: homoskedasticity is rejected
# as p_value < alpha


# version of White test
bptest(data = diamonds, price ~ carat + x + y + z,
       varformula = price ~ carat * x * y * z)
# H0: homoskedasticity is rejected
# as p_value < alpha


# comparing two nested models
formula_r <- price ~ carat
formula_ur <- price ~ carat + x + y + z

model_r <- lm(data = diamonds, formula_r)
model_ur <- lm(data = diamonds, formula_ur)

# old F-test is good under homoskedasticity
waldtest(model_r, model_ur)
# H0: (model_r is correct) is rejected
# Fobs = 585

# Wald test corrected for heteroskedasticity
waldtest(model_r, model_ur, vcov = vcovHC)
# H0: (model_r is correct) is rejected
# Fobs = 55

# logistic regression
glimpse(diamonds)

# add 0/1 dummy variable
d <- mutate(diamonds, 
        ideal = ifelse(cut == "Ideal", 1, 0))

report <- logitmfx(data = d, 
            ideal ~ price + carat + x + y + z)
summary(report$fit) # beta hat and se(beta hat)
report # marginal effect for an average observation
