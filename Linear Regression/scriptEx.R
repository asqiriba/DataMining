set.seed(1)
x = rnorm(100)

eps = rnorm(100, 0, sqrt(0.25))

y = -1 + 0.5 * x + eps

plot(x, y)

lm.fit = lm(y ~ x)
summary(lm.fit)

plot(x, y)
abline(lm.fit, lwd = 3, col = 2)
abline(-1, 0.5, lwd = 3, col = 3)

legend(-1, legend = c("B0", "B1"), col = 2:3, lwd = 3)

lm.fitPow = lm(y ~ x + I(x ^ 2))
summary(lm.fitPow)

set.seed(1)
epsUno = rnorm(100, 0, 0.125)
xUno = rnorm(100)
yUno = -1 + 0.5 * xUno + epsUno
plot(xUno, yUno)
lm.fitUno = lm(yUno ~ xUno)
summary(lm.fitUno)
abline(lm.fitUno, lwd = 3, col = 2)
abline(-1, 0.5, lwd = 3, col = 3)
legend(-1, legend = c("B0", "B1"), col = 2:3, lwd = 3)

set.seed(1)
epsDos = rnorm(100, 0, 0.5)
xDos = rnorm(100)
yDos = -1 + 0.5 * xDos + epsDos
plot(xDos, yDos)
lm.fitDos = lm(yDos ~ xDos)
summary(lm.fitDos)
abline(lm.fitDos, lwd = 3, col = 2)
abline(-1, 0.5, lwd = 3, col = 3)
legend(-1, legend = c("B0", "B1"), col = 2:3, lwd = 3)

confint((lm.fit))
confint((lm.fitUno))
confint((lm.fitDos))
