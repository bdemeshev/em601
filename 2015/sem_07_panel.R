library("plm")

help("Grunfeld")
data("Grunfeld")
g <- pdata.frame(Grunfeld,
    index = c("firm", "year"))

model_p <- plm(data = g,
            inv ~ value + capital,
            model = "pooling")
summary(model_p)

model_fe <- plm(data = g,
               inv ~ value + capital,
               model = "within")
summary(model_fe)
fixef(model_fe)

model_fe2 <- plm(data = g, effect = "time",
                inv ~ value + capital,
                model = "within")
summary(model_fe2)
fixef(model_fe2)


model_re <- plm(data = g,
                inv ~ value + capital,
                model = "random")
summary(model_re)

# сравниваем какая лучше
# pooled vs fe
pFtest(model_fe, model_p)
# fe лучше pooled

# fe vs re
phtest(model_re, model_fe)
# FE и RE модели дают состоятельные оценки

# re vs pooled
plmtest(model_re, type = "bp")
# re лучше pooled






