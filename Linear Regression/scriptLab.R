library(MASS)
library(ISLR)

attach(Boston)
summary(lm(medv~lstat*age, data = Boston))
lm(formula = medv ~ lstat * age, data = Boston)

lm.fit2 = lm(medv~lstat + I(lstat^2))
summary(lm.fit2)
lm(formula = medv ~ lstat + I(lstat^2))

lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)
summary(lm(medv~log(rm), data = Boston))

fix(Carseats)
names(Carseats)

lm.fit = lm(Sales~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("Libraries loaded :D")
}
LoadLibraries
LoadLibraries()
