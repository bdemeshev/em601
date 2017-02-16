library(forecast)
library(sophisthse)

naselenie <- sophisthse("POPNUM_Y")
naselenie

# играемся с искусственными рядами
n <- 100 
# y_t = e_t
eps <- rnorm(n, 
             mean = 0, 
             sd = 18)
eps
tsdisplay(eps)

# y_t = e_t + 2e_{t-1} + 7
eps[5] # пятый элемент
eps[-5] # все элементы кроме пятого
y_a <- eps[-1] + 2 * eps[-100] + 7
tsdisplay(y_a)

# y_t = t * e_t + 8
time <- 1:n
time

y_b <- time * eps + 8
tsdisplay(y_b)

# y_t = t/2 + e_1 + ... + e_t
y_d <- time / 2 + cumsum(eps)
tsdisplay(y_d)

# медленно убывающая ACF (левый график)
# признак того, что
# y_t - нестационарный
# а dy_t = y_t - y_{t-1} - стационарный

# перейдем к разнице dy_t
dy_d <- diff(y_d)
tsdisplay(dy_d)

# фактический ряд
tsdisplay(naselenie)

dnas <- diff(naselenie)
tsdisplay(dnas)

library(quantmod)
library(rusquant)

# установка rusquant нестандартная 
# install.packages("rusquant", repos="http://R-Forge.R-project.org")

getSymbols(Symbols = "GOOG", 
           from = "2014-10-01",
           to = "2016-12-25")
GOOG

getSymbols(Symbols = "GAZP", src = "Finam",
           from = "2014-10-01",
           to = "2016-12-25")
GAZP
