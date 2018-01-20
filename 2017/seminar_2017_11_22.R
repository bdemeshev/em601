library(tidyverse)
library(rio)

su <- import("~/Downloads/demo.xlsx")
su

model_a <- lm(data = su, macro ~ 0 + cake)
summary(model_a)

model_b <- lm(data = su, macro ~ cake)
summary(model_b)
