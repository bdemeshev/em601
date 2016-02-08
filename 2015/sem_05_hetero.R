library("ggplot2")
library("lmtest")
library("sandwich")
library("dplyr")


setwd("~/Downloads")
flats <- read.table(
  "flats_moscow.txt",
  header = TRUE, sep = "\t", dec = ".")

qplot(data = flats, x = totsp, y = price)

model <- lm(data = flats,
            price ~ totsp + brick)
summary(model)

#### до учёта гетероскедастичности:
coeftest(model) # проверка коэффициентов на значимость
confint(model) # доверительные интервал

# "Обычная" оценка ковариационной матрицы
# оценка Var(beta_hat)
vcov(model)

#### учитываем гетероскедастичность
# Оценка ковариационной матрицы устойчивая
# к гетероскедастичности
vcovHC(model)
?vcovHC

coeftest(model, vcov. = vcovHC) # проверка коэффициентов на значимость

# руками считаем доверительный интервал для коэффициентов
conf_level <- 0.9
z_cr <- qnorm((1 + conf_level)/2)
z_cr

beta_test <- coeftest(model, vcov. = vcovHC)

beta_ci <- data_frame(
  estimate = beta_test[, 1],
  seHC = beta_test[, 2],
  left = estimate - z_cr * seHC,
  right = estimate + z_cr * seHC
)
beta_ci
View(beta_ci)

# как в условиях гетероскедастичности сравнить две модели

# старый способ сравнения двух моделей
model_A <- lm(data = flats,
        price ~ totsp)
model_B <- lm(data = flats,
        price ~ totsp + brick + floor)

# ! неверно в условиях гетероскедастичности
waldtest(model_A, model_B)
# H_0 отвергается
# тк P-value < alpha

# скорректированная версия теста
waldtest(model_A, model_B, vcov = vcovHC)
# H_0 отвергается
# тк P-value < alpha

#### проверка гипотезы о незначимости регрессии
# оцениваем тривиальную модель
# тривиальная --- без регрессоров
trivial_model <- lm(data = flats, price ~ 1)

# сравниваем тривиальную модель с нашей:
waldtest(trivial_model, model, vcov = vcovHC)
# H_0 (о незначимости регрессии в целом) отвергается
# хотя бы один из коэффициентов в model значим


# тест Бройша-Пагана
# одни и те же регрессоры
# на первом и втором шаге
bptest(model)

# тест Бройша-Пагана
# с явным указанием регрессоров
# второго шага
bptest(model,
       varformula = ~totsp,
       data = flats)

