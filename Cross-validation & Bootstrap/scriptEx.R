library(boot)
library(MASS)

#(a) Generate a simulated data set.
set.seed(1)
x = rnorm(100)
y = x - 2 * x ^ 2 + rnorm(100)

#In this data set, what is n and what is p?
# n = 100, p = 2

#Write out the model used to generate the data in equation form.
# y = x - 2x^2 + sigma.



#(b) Create a scatterplot of X against Y. Comment on what you find.
plot(x, y)
# A normal distribution-like curve is how x and y are related.



#(c) Set a random seed, and then compute the LOOCV errors that result from 
# fitting the following four models using least squares.
set.seed(4)
Datos = data.frame(x, y)

#i
fit.glm.1 = glm(y ~ x)
cv.glm(Datos, fit.glm.1) $delta[1]  #7.288162

#ii
fit.glm.2 = glm(y ~ poly(x, 2))
cv.glm(Datos, fit.glm.2) $delta[1]  #0.9374236

#iii
fit.glm.3 = glm(y ~ poly(x, 3))
cv.glm(Datos, fit.glm.3) $delta[1]  #0.9566218

#iv
fit.glm.4 = glm(y ~ poly(x, 4))
cv.glm(Datos, fit.glm.4) $delta[1]  #0.9539049

#(d) Repeat (c) using another random seed, and report your results.
set.seed(22)

#i.b
fit.glm.1 = glm(y ~ x)
cv.glm(Datos, fit.glm.1) $delta[1]  #7.288162

#ii.b
fit.glm.2 = glm(y ~ poly(x, 2))
cv.glm(Datos, fit.glm.2) $delta[1]  #0.9374236

#iii.b
fit.glm.3 = glm(y ~ poly(x, 3))
cv.glm(Datos, fit.glm.3) $delta[1]  #0.9566218

#iv.b
fit.glm.4 = glm(y ~ poly(x, 4))
cv.glm(Datos, fit.glm.4) $delta[1]  #0.9539049

#Are your results the same as what you got in (c)? Why?
# Results are the same. LOOCV works without the use of the seed.



#(e) Which of the models in (c) had the smallest LOOCV error?
# ii's, 0.9374236 was the smallest.

#Is this what you expected? Explain your answer.
# Yes, their relatinship is one-to-one.



#(f) Comment on the statistical significance of the coefficient estimates
#that results from fitting each of the models in (c) using least squares.

summary(fit.glm.4)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.55002    0.09591 -16.162  < 2e-16 ***
#  poly(x, 4)1   6.18883    0.95905   6.453 4.59e-09 ***
#  poly(x, 4)2 -23.94830    0.95905 -24.971  < 2e-16 ***
#  poly(x, 4)3   0.26411    0.95905   0.275    0.784    
#poly(x, 4)4   1.25710    0.95905   1.311    0.193    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for gaussian family taken to be 0.9197797)

#Do these results agree with the conclusions drawn based on the cross-validation
#results?
# Yes, it is according to the model.



#(g) We will now consider the Boston housing data set, from the MASS library.

#(g.1) Based on this data set, provide an estimate for the population mean
#of medv. Call this estimate u.
attach(Boston)
miu.hat = mean(medv)
miu.hat #22.53281

#(g.2) Provide an estimate of the standard error of u. Interpret this result.
se.hat = sd(medv) / sqrt(dim(Boston)[1])
se.hat #0.4088611

#(g.3) Now estimate the standard error of u using the bootstrap.
set.seed(1)
bootst.fn = function(data, index){
  miu = mean(data[index])
  return (miu)
}
boot(medv, bootst.fn, 1000)
#ORDINARY NONPARAMETRIC BOOTSTRAP
#
#
#Call:
#  boot(data = medv, statistic = bootst.fn, R = 1000)
#
#
#Bootstrap Statistics :
#  original      bias    std. error
#t1* 22.53281 -0.02542806   0.4023801

#How does this compare to your answer from (b)?
# bootstrap estimated standard error is close to the estimated one.

#(g.4) Based on your bootstrap estimate from (c), provide a 95% confidence
#interval for the mean of medv. Compare it to the results obtained using 
#t.test(Boston$medv).

t.test(Boston$medv)
#One Sample t-test
#
#data:  Boston$medv
#t = 55.111, df = 505, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  21.72953 23.33608
#sample estimates:
#  mean of x 
#22.53281

c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119) #21.7062 23.3538
# The interval y very close to the other one thrown by the test.

#(g.5) Based on this data set, provide an estimate, u_med, for the median value
#of medv in the population.
median(medv)  #21.2

#(g.6) We now would like to estimate the standard error of u_med.
#Unfortunately, there is no simple formula for computing the standard error of the median.
#Instead, estimate the standard error of the median using the bootstrap.

bootstp.fn = function(data, index){
  miu = median(data[index])
  return (miu)
}
boot(medv, bootstp.fn, 1000)
#ORDINARY NONPARAMETRIC BOOTSTRAP
#
#
#Call:
#  boot(data = medv, statistic = bootstp.fn, R = 1000)
#
#
#Bootstrap Statistics :
#  original  bias    std. error
#t1*    21.2  -0.0184   0.3878039

#Comment on your findings.
# 21.2 estimated value, was expecting the same as last step.

#(g.6) Based on this data set, provide an estimate for the tenth percentile of medv in Boston suburbs.
#Call this quantity u0.1. (You can use the quantile() function.)
quantile(medv, c(0.1))  #10%
                        #12.75

#(g.7) Use the bootstrap to estimate the standard error of u0.1.
bootstpsd.fn = function(data, index){
  miu = quantile(data[index], c(0.1))
  return (miu)
}
boot(medv, bootstpsd.fn, 1000)
#ORDINARY NONPARAMETRIC BOOTSTRAP
#
#
#Call:
#  boot(data = medv, statistic = bootstpsd.fn, R = 1000)
#
#
#Bootstrap Statistics :
#  original  bias    std. error
#t1*    12.75 0.00485   0.5025052

#Comment on your findings.
#Then again is the value obtained by the test. Small Std Err.