library("dplyr")
library("lmtest")
library("ggplot2")

# устанавливаем рабочую папку
# вар 1: командой
setwd("/Users/boris/Downloads/")
# папку указывыйте свою :)
# вар 2: кликаньем
# Session - Set Working Directory - Choose

flats <- read.table("flats_moscow.txt",
      header = TRUE,
      sep = "\t", dec = ".")
glimpse(flats)

# restricted model
model_r <- lm(data = flats,
              price ~ totsp)
rss_r <- deviance(model_r)
rss_r

flats_b <- filter(flats, brick == 1)
glimpse(flats_b)

flats_nb <- filter(flats, brick == 0)

model_b <- lm(data = flats_b,
              price ~ totsp)

model_nb <- lm(data = flats_nb,
              price ~ totsp)
summary(model_nb)

rss_b <- deviance(model_b)
rss_nb <- deviance(model_nb)
rss_ur <- rss_b + rss_nb
rss_ur

n <- nrow(flats)
n

F_obs <- ((rss_r - rss_ur) / 2) /
         (rss_ur / (n - 4))
F_obs
F_crit <- qf(0.95, df1 = 2, df2 = n - 4)
F_crit

# другой способ
flats2 <- mutate(flats,
        db = brick, dnb = 1 - brick,
        db_totsp = db * totsp,
        dnb_totsp = dnb * totsp)
model_ur <- lm(data = flats2,
      price ~ 0 + db + db_totsp +
        dnb + dnb_totsp)
deviance(model_ur)
anova(model_r, model_ur)
# H0 is rejected
# кирпичные и некирпичные отличаются

# упражнение два
# H0: price = b1 + b2 livesp + b3 kitsp + b4 othersp + eps_i
# Ha:
# for brick:
# price = b1 + b2 livesp + b3b kitsp + b4b othersp + eps_i
# for non-brick:
# price = b1 + b2 livesp + b3nb kitsp + b4nb othersp + eps_i


flats3 <- mutate(flats2,
      othersp = totsp - livesp - kitsp)
model_r <- lm(data = flats3,
      price ~ livesp + kitsp + othersp)
summary(model_r)

flats4 <- mutate(flats3, db_kitsp = db * kitsp,
                 dnb_kitsp = dnb * kitsp,
                 db_othersp = db * othersp,
                 dnb_othersp = dnb * othersp)
model_ur <- lm(data = flats4,
      price ~ livesp + db_kitsp + dnb_kitsp +
        db_othersp + dnb_othersp)

anova(model_r, model_ur)
# H0 is rejected
# кирпичные и некирпичные отличаются хотя бы одним из коэффициентов b3 или b4
