library("AER")
library("dplyr")
library("ggplot2")

data("CigarettesSW")
help("CigarettesSW")

dplyr::glimpse(CigarettesSW)
cig <- dplyr::filter(
  CigarettesSW,
      year == 1995)
qplot(data = cig,
      x = price, y = packs)
cig2 <- mutate(cig,
      lprice = log(price),
      lquant = log(packs),
      tdiff = taxs - tax)

model_1 <- lm(data = cig2,
        lquant ~ lprice)
summary(model_1)

model_2 <- ivreg(data = cig2,
        lquant ~ lprice | tdiff)
summary(model_2)


