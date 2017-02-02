library(tidyverse) # манипуляции с данными + графики
library(lmtest) # тесты для линейных моделей (на ГСК и пр)
library(sandwich) # для робастных к ГСК оценок дисперсиии
library(psych) # описательные статистики


# File - Import dataset
library(readr)

flats <- read_delim("~/Downloads/flats_moscow.txt", 
                    "\t", escape_double = FALSE, col_types = cols(kitsp = col_double()), 
                    trim_ws = TRUE)
glimpse(flats)
flats2 <- mutate(flats, 
                 othersp = totsp - kitsp - livesp)

mod_a <- lm(data = flats2, 
    price ~ othersp + kitsp + livesp + dist + brick)
mod_b <- lm(data = flats2,
    log(price) ~ log(othersp) + log(kitsp) + log(livesp) + 
      dist + brick)
summary(mod_a)
summary(mod_b)

qplot(data = flats2, x = totsp, y = price)
qplot(data = flats2, x = log(totsp), y = log(price))


# смысл: +, * и ^ в формуле
mod_x <- lm(data = flats2, 
            price ~ othersp * kitsp) # x1, x2, x1 * x2
mod_x <- lm(data = flats2, 
            price ~ othersp : kitsp) # только x1 * x2
mod_x <- lm(data = flats2,
            price ~ (othersp + kitsp + livesp)^2)
summary(mod_x)

# как же всё-таки добавить квадрат?
# 1. - создать такую переменную до регрессии
# 2. - написать квадрат внутри I(...)

# способ 1 
flats3 <- mutate(flats2, 
                 othersp2 = othersp^2,
                 kitsp2 = kitsp^2,
                 livesp2 = livesp^2,
                 dist2 = dist^2)
# способ 2
mod_x <- lm(data = flats2,
            price ~ othersp + kitsp + livesp +
              I(livesp^2))
summary(mod_x)

# итак, тест Бройша Пагана (Уайта) на гетероскедастичность
# вариант один: BP-test 
# во вспомогательную регрессию включают те же регрессоры, 
# что в исходную
bptest(mod_a)
# p-value < 0.05 => H0 о гомоскедастичности отвергается
# вариант два: White-test
aux <- ~ (othersp + kitsp + livesp + dist + brick)^2 +
  I(othersp^2) + I(kitsp^2) + I(livesp^2) + I(dist^2) 
bptest(mod_a, varformula = aux, data = flats2)
# p-value < 0.05 => H0 о гомоскедастичности отвергается


# тест Голдфельда-Квандта
gqtest(mod_a, fraction = 0.2, order.by = flats2$totsp)
# p-value < 0.05 => H0 о гомоскедастичности отвергается

# поиск ошибки чтения:
# describe(flats2)
# which(is.na(flats2$kitsp))
# flats2[1180, ]

# LASSO
library(glmnet)

big_model <- lm(data = flats2, 
      price ~ (kitsp + livesp + othersp + 
                 dist + brick + floor + walk)^3)
summary(big_model)

y <- flats2$price
X <- model.matrix(data = flats2,
        price ~ (kitsp + livesp + othersp + 
                  dist + brick + floor + walk)^3)
y
X
mod_lasso <- glmnet(X, y)
mod_lasso

# plot(mod_lasso, "dev")

coef(mod_lasso, s = 13.71)
predict(mod_lasso, newdata = ..., s = 13.71)


