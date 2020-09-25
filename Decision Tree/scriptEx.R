library(ISLR)
attach(Carseats)

#(a) Split the data set into a training set and a test set.
set.seed(401) #Yeah, like the error.
training = sample(nrow(Carseats), nrow(Carseats) / 2)
Carseats.training = Carseats[training, ]
Carseats.test = Carseats[-training, ]


#(b) Fit a regression tree to the training set.
#    Plot the tree, and interpret the results.
#    What test MSE do you obtain?
library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.training)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.5)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats) ^ 2) #MSE obtained of [1] 2.916513