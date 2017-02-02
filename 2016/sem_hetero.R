library(tidyverse)
library(readr)
# скопировано из File - Import dataset - CSV
# разделитель: tab
flats <- read_delim(
        "~/Downloads/flats_moscow.txt", 
        "\t", escape_double = FALSE, 
        trim_ws = TRUE)

glimpse(flats)

qplot(data = flats, 
      x = totsp,
      y = price)

m1 <- lm(data = flats,
         price ~ totsp)
summary(m1)

m2 <- lm(data = flats,
  price ~ totsp + kitsp + livesp + brick + floor)
summary(m2)

flats2 <- mutate(flats, 
          hallsp = totsp - kitsp - livesp)

m3 <- lm(data = flats2, 
         price ~ hallsp + kitsp + livesp + brick + floor)
summary(m3)

# боремся с ГСК
library(lmtest) # всякие тесты
library(sandwich) # se_HC(beta_hat)

coeftest(m3) # обычные se(beta_hat)
coeftest(m3, vcov = vcovHC) # робастные se_HC(beta_hat)

library(modelr)
flats3 <- add_residuals(data = flats2, m3)
glimpse(flats3)
