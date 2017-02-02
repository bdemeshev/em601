x <- 9
# коммент по русски
# bad style:
# x<-9
# x = 9

# простая арифметика
vladlena <- c(5, -7, 3.9)
vladlena
y <- vladlena^2
y

log(vladlena)
v <- c(5, NA, NaN, 10)

0/0

v + c(1, 2, 3, 4)
mean(vladlena)
mean(v)
mean(v, na.rm = TRUE)

library("tidyverse")
library("psych")
Pokemon <- read_csv("~/Downloads/Pokemon.csv")
glimpse(Pokemon)

head(Pokemon)
tail(Pokemon)

help(pie)

qplot(data = Pokemon,
      x = Attack)
qplot(data = Pokemon,
  x = Attack, y = Defense)
qplot(data = Pokemon,
  x = Attack, y = Defense,
  col = Generation)
qplot(data = Pokemon,
      x = Attack, y = Defense,
      col = Generation,
      size = Speed) + theme_bw()
library("ggthemes")

describe(Pokemon)

model <- lm(data = Pokemon,
            Speed ~ Attack + Defense)
summary(model)

new_data <- data_frame(Attack = c(50, 100),
                       Defense = c(30, 70))
new_data
predict(model, new_data)
