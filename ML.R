## Machine learning with R

mtcars
head(mtcars)

op <- par(mar = c(10, 4, 4, 2) + 0.1)
barplot(mtcars$mpg, names.arg = row.names(mtcars), las = 2, ylab = "Fuel")

pairs(mtcars[1:7], lower.panel = NULL)

plot(y = mtcars$mpg, x = mtcars$wt, xlab = "Vehicle Weight",
     ylab = "Vehicle Fuel Efficiency in Miles per Gallon")

mt.model <- lm(formula = mpg ~ wt, data = mtcars)
summary(mt.model)

v1 = c(1, 2, 3)
v2 = c("Jerry", "George", "Elaine")
v3 = c(TRUE, FALSE, TRUE)

data_frame = data.frame(v1, v2, v3)

str(data_frame)

plot(y = mtcars$mpg, x = mtcars$disp, xlab = "Engine Size (cubic inches)",
     ylab = "Fuel Efficiency (Miles per Gallon)")

model <- lm(mtcars$mpg ~ mtcars$disp)
coef(model)
-0.04121*200 + 29.5998

coef(model)[2] * 200 + coef(model)[1]

split_size = 0.8
sample_size = floor(split_size * nrow(mtcars))
set.seed(123)
train_indices <- sample(seq_len(nrow(mtcars)), size = sample_size)
train <- mtcars[train_indices, ]
test <- mtcars[-train_indices, ]
model2 <- lm(mpg ~ disp, data = train)
new.data <- data.frame(disp = test$disp)
test$output <- predict(model2, new.data)
sqrt(sum(test$mpg - test$output)^2/nrow(test))

## Logistic Regression

plot(x = mtcars$mpg, y = mtcars$am, xlab = "Fuel Efficiency (Miles per Gallon)",
     ylab = "Vehicle Transmission Type (0 = Automatic, 1 = Manual)")

# This plot of vehicle transmission type as a function of fuel efficiency is very
# different looking than the plot of efficiency versus engine size.

library(caTools)

Label.train = train[, 9]
Data.train = train[, -9]

model = LogitBoost(Data.train, Label.train)
Data.test = test
Lab = predict(model, Data.test, type = "raw")
data.frame(row.names(test), test$mpg, test$am, Lab)

plot(x = iris$Petal.Length, y = iris$Petal.Width, xlab = "Petal Length",
     ylab = "Petal Width")

data = data.frame(iris$Petal.Length, iris$Petal.Width)
iris.kmeans <- kmeans(data, 2)
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans$cluster,
     xlab = "Petal Length", ylab = "Petal Width")
points(iris.kmeans$centers, pch = 8, cex = 2)

iris.kmeans3 <- kmeans(data, 3)
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster,
     xlab = "Petal Length", ylab = "Petal Width")
points(iris.kmeans3$centers, pch = 8, cex = 2)

par(mfrow = c(1, 2))

plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster,
     xlab = "Petal Length", ylab = "Petal Width", main = "Model Output")

plot(x = iris$Petal.Length, y = iris$Petal.Width,
     pch = as.integer(iris$Species),
     xlab = "Petal Length", ylab = "Petal Width", main = "Actual Data")

table(iris.kmeans3$cluster, iris$Species)

library(party)

tree <- ctree(mpg ~ ., data = mtcars)
plot(tree)

tree.train <- ctree(mpg ~ ., data = train)
plot(tree.train)

test$mpg.tree <- predict(tree.train, test)
test$class <- predict(tree.train, test, type = "node")
data.frame(row.names(test), test$mpg, test$mpg.tree, test$class)

# Random Forests

library(randomForest)

par(mfrow = c(1,1))
mtcars.rf <- randomForest(mpg ~ ., data = mtcars, ntree = 1000,
                          keep.forest = FALSE, importance = FALSE)
plot(mtcars.rf, log = "y", title = "", col = "blue")

# Random forest algorithms are much more difficult to show in a visualization;
# however, we can easily show how the error in the model evolves over the course of how
# many trees we introduce into the model

# Neural Networks

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

set.seed(123)
library(nnet)
iris.nn <- nnet(Species ~ ., data = iris, size = 2)

table(iris$Species, predict(iris.nn, iris, type = "class"))

# Support Vector Machines
## Vous pouvez faire la classification SVM d'une manière très similaire à la classification 
## de réseau neuronal, comme nous l'avons vu précédemment:

library(e1071)
iris.svm <- svm(Species ~ ., data = iris)
table(iris$Species, predict(iris.svm, iris, type = "class"))

# Unsupervised Clustering Methods

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2), matrix(rnorm(100,
                                                                mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
plot(x)

cl <- kmeans(x, 2)
plot(x, pch = cl$cluster)

cl[2]

# Sampling Statistics and Model Training in R

iris.df <- data.frame(iris)
sample.index <- sample(1:nrow(iris.df), nrow(iris) * 0.75, replace = FALSE)
head(iris[sample.index, ])

summary(iris)
summary(iris[sample.index,])

sys.sample = function(N, n) {
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k * (n - 1), k)
}
systematic.index <- sys.sample(nrow(iris), nrow(iris) * 0.75)
summary(iris[systematic.index, ])

# Training and Test Sets: Regression Modeling

set.seed(123)
x <- rnorm(100, 2, 1)
y = exp(x) + rnorm(5, 0, 2)
plot(x, y)
linear <- lm(y ~ x)
abline(a = coef(linear[1], b = coef(linear[2], lty = 2)))

## Randomized data with a linear fit attached; the linear fit comes close to fit‐
## ting some data points, but not all (the further you extend X out, the more likely it is that
## your linear fit won’t approximate the data very well)

summary(linear)

data <- data.frame(x, y)
data.samples <- sample(1:nrow(data), nrow(data) * 0.7, replace = FALSE)
training.data <- data[data.samples, ]
test.data <- data[-data.samples, ]
train.linear <- lm(y ~ x, training.data)
train.output <- predict(train.linear, test.data)
RMSE.df = data.frame(predicted = train.output, actual = test.data$y,
                     SE = ((train.output - test.data$y)^2/length(train.output)))
head(RMSE.df)

sqrt(sum(RMSE.df$SE))

train.quadratic <- lm(y ~ x^2 + x, training.data)
summary(train.quadratic)
quadratic.output <- predict(train.quadratic, test.data)
RMSE.quad.df = data.frame(predicted = quadratic.output, actual = test.data$y,
                          SE = ((quadratic.output - test.data$y)^2/length(train.output)))
head(RMSE.quad.df)
sqrt(sum(RMSE.quad.df$SE))

train.polyn <- lm(y ~ poly(x, 4), training.data)
summary(train.polyn)
plot(train.polyn$residuals)
plot(train.polyn)
polyn.output <- predict(train.polyn, test.data)
RMSE.polyn.df = data.frame(predicted = polyn.output, actual = test.data$y,
                           SE = ((polyn.output - test.data$y)^2/length(train.output)))
head(RMSE.polyn.df)

sqrt(sum(RMSE.quad.df$SE))


# Training and Test Sets: Classification Modeling

iris.df <- iris
iris.df$Species <- as.character(iris.df$Species)
iris.df$Species[iris.df$Species != "setosa"] <- "other"
iris.df$Species <- as.factor(iris.df$Species)
iris.samples <- sample(1:nrow(iris.df), nrow(iris.df) * 0.7,
                       replace = FALSE)
training.iris <- iris.df[iris.samples, ]
test.iris <- iris.df[-iris.samples, ]

library(randomForest)

iris.rf <- randomForest(Species ~ ., data = training.iris)
iris.predictions <- predict(iris.rf, test.iris)
table(iris.predictions, test.iris$Species)

# Validation 
set.seed(123)
x <- rnorm(100, 2, 1)
y = exp(x) + rnorm(5, 0, 2)
data <- data.frame(x, y)
data.shuffled <- data[sample(nrow(data)), ]
folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
errors <- c(0)
for (i in 1:10) {
  fold.indexes <- which(folds == i, arr.ind = TRUE)
  test.data <- data[fold.indexes, ]
  training.data <- data[-fold.indexes, ]
  train.linear <- lm(y ~ x, training.data)
  train.output <- predict(train.linear, test.data)
  errors <- c(errors, sqrt(sum(((train.output - test.data$y)^2/length(train.output)))))
}
errors[2:11]
mean(errors[2:11])

# Regression in a Nutshell (La régression en quelques mots)

model <- lm(mtcars$mpg ~ mtcars$disp)
summary(model)

plot(y = mtcars$mpg, x = mtcars$disp, xlab = "Engine Size (cubic inches)",
     ylab = "Fuel Efficiency (Miles per Gallon)", main = "Fuel Efficiency From
the `mtcars` Dataset")

abline(a = coef(model[1]), b = coef(model)[2], lty = 2)

## Multivariate Regression

lm.wt <- lm(mpg ~ disp + wt, data = mtcars)
summary(lm.wt)

lm.cyl <- lm(mpg ~ disp + wt + cyl, data = mtcars)
summary(lm.cyl)

lm.cyl.wt <- lm(mpg ~ wt + cyl, data = mtcars)
summary(lm.cyl.wt)
plot(lm.cyl.wt)

lm.all <- lm(mpg ~ ., data = mtcars)
summary(lm.all)

library(lasso2)

lm.lasso <- l1ce(mpg ~ ., data = mtcars)
summary(lm.lasso)$coefficients

lm.lasso2 <- l1ce(mpg ~ cyl + hp + wt + am + carb, data = mtcars)
summary(lm.lasso2)$coefficients

lm.lasso3 <- l1ce(mpg ~ cyl + hp + wt, data = mtcars)
summary(lm.lasso3)$coefficients

lm.lasso4 <- l1ce(mpg ~ cyl + wt, data = mtcars)
summary(lm.lasso4)$coefficients

# Polynomial Regression

pop <- data.frame(uspop)
pop$uspop <- as.numeric(pop$uspop)
pop$year <- seq(from = 1790, to = 1970, by = 10)
plot.ts(y = pop$uspop, x = pop$year, main = "United States Population From 1790 to
1970", xlab = "Year", ylab = "Population")

## The plotted population of the United States in decades from 1790 to 1970

plot(y = pop$uspop, x = pop$year, main = "United States Population From 1790 to
1970", xlab = "Year", ylab = "Population")

## Regression 

lm1 <- lm(pop$uspop ~ pop$year)
summary(lm1)

plot(y = pop$uspop, x = pop$year, main = "United States Population From 1790 to
1970", xlab = "Year", ylab = "Population") ## Population data with a linear model fit

abline(a = coef(lm1[1]), b = coef(lm1)[2], lty = 2, col = "red")

lm2 <- lm(pop$uspop ~ poly(pop$year, 2))
summary(lm2)

plot(y = pop$uspop, x = pop$year, main = "United States Population From 1790 to
1970",
     xlab = "Year", ylab = "Population")

pop$lm2.predict = predict(lm2, newdata = pop)

lines(sort(pop$year), fitted(lm2)[order(pop$year)], col = "blue",
      lty = 2)

par(mfrow = c(2, 3))
plot(resid(lm1), main = "Degree 1", xlab = "Sequential Year", ylab = "Fit Residual")
plot(resid(lm2), main = "Degree 2", xlab = "Sequential Year", ylab = "Fit Residual")
plot(resid(lm3), main = "Degree 3", xlab = "Sequential Year", ylab = "Fit Residual")
plot(resid(lm4), main = "Degree 4", xlab = "Sequential Year", ylab = "Fit Residual")
plot(resid(lm5), main = "Degree 5", xlab = "Sequential Year", ylab = "Fit Residual")
plot(resid(lm6), main = "Degree 6", xlab = "Sequential Year", ylab = "Fit Residual")

c(sum(abs(resid(lm1))), sum(abs(resid(lm2))), sum(abs(resid(lm3))),
  sum(abs(resid(lm4))), sum(abs(resid(lm5))), sum(abs(resid(lm6))))

# Root-Mean-Square Error

uspop.2020 <- data.frame(year = c(1980, 1990, 2000, 2010), uspop = c(226.5,
                                                                     249.6, 282.2, 309.3))
uspop.2020.predict <- uspop.2020
pop2 <- data.frame(uspop)
pop2$uspop <- as.numeric(pop$uspop)
pop2$year <- seq(from = 1790, to = 1970, by = 10)

uspop.2020.predict$lm1 <- predict(lm(uspop ~ poly(year, 1), data = pop2),
                                  uspop.2020)
uspop.2020.predict$lm2 <- predict(lm(uspop ~ poly(year, 2), data = pop2),
                                  uspop.2020)
uspop.2020.predict$lm3 <- predict(lm(uspop ~ poly(year, 3), data = pop2),
                                  uspop.2020)
uspop.2020.predict$lm4 <- predict(lm(uspop ~ poly(year, 4), data = pop2),
                                  uspop.2020)
uspop.2020.predict$lm5 <- predict(lm(uspop ~ poly(year, 5), data = pop2),
                                  uspop.2020)
uspop.2020.predict$lm6 <- predict(lm(uspop ~ poly(year, 6), data = pop2),
                                  uspop.2020)

table((summary(lm1)$coefficients[, 4]) < 0.05)
summary(lm1)$r.squared

model.order <- c(1,2,3,4,5,6)
coef.true <- c(
  table((summary(lm1)$coefficients[,4])<0.05) - 1
  ,table((summary(lm2)$coefficients[,4])<0.05) - 1
  ,table((summary(lm3)$coefficients[,4])<0.05)[2] - 1
    ,table((summary(lm4)$coefficients[,4])<0.05)[2] - 1
    ,table((summary(lm5)$coefficients[,4])<0.05)[2] - 1
    ,table((summary(lm6)$coefficients[,4])<0.05)[2] - 1
)

coef.false <- c(
  0
  ,0
  ,table((summary(lm3)$coefficients[,4])<0.05)[1]
  ,table((summary(lm4)$coefficients[,4])<0.05)[1]
  ,table((summary(lm5)$coefficients[,4])<0.05)[1]
  ,table((summary(lm6)$coefficients[,4])<0.05)[1]
)

# Logistic Regression (y = mx + b)

data <- data.frame(tumor.size <- c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                                   20), malignant <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1))
tumor.lm <- lm(malignant ~ tumor.size, data = data)
par(mfrow = c(1,1))
plot(y = data$malignant, x = data$tumor.size, main = "Tumor Malignancy by Size",
     ylab = "Type (0 = benign, 1 = cancerous)", xlab = "Tumor Size")
abline(a = coef(tumor.lm[1]), b = coef(tumor.lm[2]))
coef(tumor.lm)

summary(tumor.lm)$r.squared

# The Decision Boundary

plot(y = data$malignant, x = data$tumor.size, main = "Tumor Malignancy by Size",
     ylab = "Type (0 = benign, 1 = cancerous)", xlab = "Tumor Size")
abline(v = 4.5)

# The Sigmoid Function (The sigmoid function is the basis for logistic regression)

e <- exp(1)
curve(1/(1 + e^-x), -10, 10, main = "The Sigmoid Function", xlab = "Input",
      ylab = "Probability")

lengths <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
t1 = -4.5
t2 = 1
g = t1 + t2 * lengths
s = 1/(1 + e^-g)
data.frame(lengths, g, s)

plot(y = s, x = lengths, pch = 1, main = "Sigmoid Function Inputs and Rounding
Estimates",
     xlab = "Tumor Lengths", ylab = "Probability of Class 1 Typification")
points(y = round(s), x = lengths, pch = 3)

# Binary Classification

plot(iris$Sepal.Length ~ iris$Sepal.Width, main = "Iris Flower Sepal Length vs
Sepal Width",
     xlab = "Sepal Width", ylab = "Sepal Length")

iris.binary <- iris
iris.binary$binary <- as.numeric(iris[, 5] == "setosa")
iris.logistic <- glm(binary ~ Sepal.Width + Sepal.Length, data = iris.binary,
                     family = "binomial")
iris.logistic

slope.iris <- coef(iris.logistic)[2]/(-coef(iris.logistic)[3])
int.iris <- coef(iris.logistic)[1]/(-coef(iris.logistic)[3])
slope.iris
int.iris

iris.binary$binary[iris.binary$binary == 0] <- 2
plot(Sepal.Length ~ Sepal.Width, data = iris.binary, pch = (binary),
     main = "Iris Flower Sepal Length vs Sepal Width", xlab = "Sepal Width",
     ylab = "Sepal Length")
abline(a = int.iris, b = slope.iris, col = "red")

# Multiclass Classification

multi <- data.frame(x1 = c(0.03, 0.24, 0.21, 0, 0, 0.23, 0.6,
                           0.64, 0.86, 0.77), 
                    x2 = c(0.07, 0.06, 0.19, 1.15, 0.95, 1,
                            0.81, 0.64, 0.44, 0.74), lab = c(1, 1, 1, 2, 2, 2, 3, 3,
                                                                      3, 3))
plot(x2 ~ x1, pch = lab, cex = 2, data = multi,
     main = "Multi-Class Classification",
     xlab = "x", ylab = "y")

par(mfrow = c(1, 3))
multi$lab2 <- c(1, 1, 1, 4, 4, 4, 4, 4, 4, 4)
plot(x2 ~ x1, pch = lab2, cex = 2, data = multi,
     main = "Multi-Class Classification",
     xlab = "x", ylab = "y")

multi$lab3 <- c(4, 4, 4, 2, 2, 2, 4, 4, 4, 4)
plot(x2 ~ x1, pch = lab3, cex = 2, data = multi,
     main = "Multi-Class Classification",
     xlab = "x", ylab = "y")

multi$lab4 <- c(4, 4, 4, 4, 4, 4, 3, 3, 3, 3)
plot(x2 ~ x1, pch = lab4, cex = 2, data = multi,
     main = "Multi-Class Classification",
     xlab = "x", ylab = "y")

library(nnet)

multi.model <- multinom(lab ~ x2 + x1, data = multi, trace = F)
multi.model

multi.int.1 <- -coef(multi.model)[1]/coef(multi.model)[3]
multi.slope.1 <- -coef(multi.model)[5]/coef(multi.model)[3]

multi.int.2 <- -coef(multi.model)[2]/coef(multi.model)[4]
multi.slope.2 <- -coef(multi.model)[6]/coef(multi.model)[4]

par(mfrow = c(1,1))
plot(x2 ~ x1, pch = lab, cex = 2, data = multi,
     main = "Multi-Class Classification",
     xlab = "x", ylab = "y")

abline(multi.int.1, multi.slope.1)
abline(multi.int.2, multi.slope.2)


# Logistic Regression with Caret

library(caret)

data("GermanCredit")
GermanM1

Train <- createDataPartition(GermanCredit$Class, p = 0.6, list = FALSE)
training <- GermanCredit[Train, ]
testing <- GermanCredit[-Train, ]
mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate +
                   Housing.Own + CreditHistory.Critical, data = training, method = "glm",
                 family = "binomial")
predictions <- predict(mod_fit, testing[, -10])
table(predictions, testing[, 10])

mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate +
                   Housing.Own + CreditHistory.Critical, data = training,
                 method = "LogitBoost",
                 family = "binomial")
predictions <- predict(mod_fit, testing[, -10])
table(predictions, testing[, 10])

# Neural Networks in a Nutshell

## Single-Layer Neural Networks

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
logic <- data.frame(x1, x2)
logic$AND <- as.numeric(x1 & x2)
logic

library(neuralnet)

set.seed(123)
AND <- c(rep(0, 3), 1)
binary.data <- data.frame(expand.grid(c(0, 1), c(0, 1)), AND)
net <- neuralnet(AND ~ Var1 + Var2, binary.data, hidden = 0,
                 err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best") # Building a Simple Neural Network by Using R

prediction(net)

# Multiple Compute Outputs neural

set.seed(123)
AND <- c(rep(0, 7), 1)
OR <- c(0, rep(1, 7))
binary.data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0,
                                                          1)), AND, OR)
net <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary.data,
                 hidden = 0, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best") 
prediction(net)
## Neural networks can be more complex than logistic regression in terms of multiple compute outputs

# Hidden Compute Nodes

set.seed(123)
AND <- c(rep(0, 7), 1)
binary.data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0,
                                                          1)), AND, OR)
net <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 1,
                 err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best") # A neural network with three inputs, one hidden layer, 
                        # and one output layer (3:1:1)
# H1 = 8.57 - 3.5Var1 - 3,5Var2 - 3.6Var3 

set.seed(123)
net2 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 2,
                  err.fct = "ce", linear.output = FALSE)
plot(net2, rep = "best") # Visualized here is a 3:4:1 neural network architecture

set.seed(123)
net4 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 4, err.fct = "ce", 
                  linear.output = FALSE)
plot(net4, rep = "best") # A neural network with four compute nodes in a single hidden layer

net8 = neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 8, err.fct = "ce",
                 linear.output = FALSE)
plot(net8, rep = "best") # An overfitting 3:8:1 neural network model

set.seed(123)
net <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary.data,
                 hidden = 6, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best")

# We can also design neural networks to have multiple compute outputs, while
# still having multiple compute nodes in the hidden layer

## Multilayer Neural Networks

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
logic <- data.frame(x1, x2)
logic$AND <- as.numeric(x1 & x2)
logic$OR <- as.numeric(x1 | x2)
logic

logic$AND <- as.numeric(x1 & x2) + 1
logic$OR <- as.numeric(x1 | x2) + 1
par(mfrow = c(2, 1))
plot(x = logic$x1, y = logic$x2, pch = logic$AND, cex = 2,
     main = "Simple Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5,
                                                           1.5))
plot(x = logic$x1, y = logic$x2, pch = logic$OR, cex = 2,
     main = "Simple Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5,
                                                           1.5))
## In these two cases of classification, we can separate the two classes by drawing 
## a straight line decision boundary

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
logic <- data.frame(x1, x2)
logic$AND <- as.numeric(x1 & x2)
logic$OR <- as.numeric(x1 | x2)
logic$XOR <- as.numeric(xor(x1, x2))
logic$XNOR <- as.numeric(x1 == x2)
logic

logic$XOR <- as.numeric(xor(x1, x2)) + 1
logic$XNOR <- as.numeric(x1 == x2) + 1
par(mfrow = c(2, 1))
plot(x = logic$x1, y = logic$x2, pch = logic$XOR, cex = 2, main = "Non-Linear
Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5,
                                                           1.5))
plot(x = logic$x1, y = logic$x2, pch = logic$XNOR, cex = 2, main = "Non-Linear
Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5,
                                                           1.5))
## Dans ces deux cas, aucune ligne droite ne peut séparer les deux classes; Cependant, 
## plusieurs lignes droites combinées peuvent former une courbe que vous pouvez utiliser comme 
## limite de décision non linéaire pour séparer les classes de données.

logic$XOR <- as.numeric(xor(x1, x2))
set.seed(123)
net.xor <- neuralnet(XOR ~ x1 + x2, logic, hidden = 0, err.fct = "ce",
                     linear.output = FALSE)
prediction(net.xor)
plot(net.xor, rep = "best")

## Le calcul d'une sortie non linéaire avec une seule couche masquée (dans ce cas, 
## la couche masquée est la couche de calcul) produit d'énormes erreurs.

set.seed(123)
and.net <- neuralnet(AND ~ x1 + x2, logic, hidden = 2, err.fct = "ce",
                     linear.output = FALSE)
and.result <- data.frame(prediction(and.net)$rep1)

or.net <- neuralnet(OR ~ x1 + x2, logic, hidden = 2, err.fct = "ce",
                    linear.output = FALSE)
or.result <- data.frame(prediction(or.net)$rep1)

as.numeric(xor(round(and.result$AND), round(or.result$OR)))

xor.data <- data.frame(and.result$AND, or.result$OR,
                       as.numeric(xor(round(and.result$AND),
                                      round(or.result$OR))))
names(xor.data) <- c("AND", "OR", "XOR")
xor.net <- neuralnet(XOR ~ AND + OR, data = xor.data, hidden = 0,
                     err.fct = "ce", linear.output = FALSE)
prediction(xor.net)

plot(xor.net, rep = "best")

## Vous pouvez contourner les limites de l'algorithme en calculant d'abord la couche unique, 
## puis en transmettant les résultats à une autre couche de calcul afin d'émuler un réseau de 
## neurones multicouches.

# Neural Networks for Regression

library(mlbench)

data(BostonHousing)

lm.fit <- lm(medv ~ ., data = BostonHousing)
lm.predict <- predict(lm.fit)

par(mfrow = c(1,1))
plot(BostonHousing$medv, lm.predict, main = "Linear regression predictions vs
actual",
     xlab = "Actual", ylab = "Prediction")

## Une façon de mesurer les performances d’un modèle consiste à comparer les résultats de ses 
## prédictions avec ce qu’ils sont réellement.

library(nnet)

nnet.fit1 <- nnet(medv ~ ., data = BostonHousing, size = 2)

nnet.predict1 <- predict(nnet.fit1)

plot(BostonHousing$medv, nnet.predict1, main = "Neural network predictions vs
actual",
     xlab = "Actual", ylab = "Prediction")

## Un élément à surveiller lors du changement de modèle est la nécessité de normaliser les données 
## en premier.

summary(BostonHousing$medv)
summary(BostonHousing$medv/50)

nnet.fit2 <- nnet(medv/50 ~ ., data = BostonHousing, size = 2,
                  maxit = 1000, trace = FALSE)
nnet.predict2 <- predict(nnet.fit2) * 50

plot(BostonHousing$medv, nnet.predict2, main = "Neural network predictions vs
actual with normalized response inputs",
     xlab = "Actual", ylab = "Prediction")

mean((lm.predict - BostonHousing$medv)^2)
mean((nnet.predict2 - BostonHousing$medv)^2)

## Alternativement, vous pouvez utiliser le puissant curseur R pour mieux ajuster votre modèle.

library(caret)

mygrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(4, 5, 6))
nnetfit <- train(medv/50 ~ ., data = BostonHousing, method = "nnet",
                 maxit = 1000, tuneGrid = mygrid, trace = F)
print(nnetfit)

iris.df <- iris
smp_size <- floor(0.75 * nrow(iris.df))
set.seed(123)
train_ind <- sample(seq_len(nrow(iris.df)), size = smp_size)
train <- iris.df[train_ind, ]
test <- iris.df[-train_ind, ]
iris.nnet <- nnet(Species ~ ., data = train, size = 4, decay = 0.0001,
                  maxit = 500, trace = FALSE)
predictions <- predict(iris.nnet, test[, 1:4], type = "class")
table(predictions, test$Species)

library(car)
library(caret)

trainIndex <- createDataPartition(Prestige$income, p = 0.7, list = F)
prestige.train <- Prestige[trainIndex, ]
prestige.test <- Prestige[-trainIndex, ]

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6,
                                                       7))
prestige.fit <- train(income ~ prestige + education, data = prestige.train,
                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F,
                      linout = 1)
prestige.predict <- predict(prestige.fit, newdata = prestige.test)
summary(prestige.test$income)

sqrt(mean((prestige.predict - prestige.test$income)^2))

prestige.fit <- train(income ~ prestige + education, data = prestige.train,
                      method = "neuralnet")
prestige.predict <- predict(prestige.fit, newdata = prestige.test)
sqrt(mean((prestige.predict - prestige.test$income)^2))

iris.caret <- train(Species ~ ., data = train, method = "nnet",
                    trace = FALSE)
predictions <- predict(iris.caret, test[, 1:4])
table(predictions, test$Species)

iris.caret.m <- train(Species ~ ., data = train, method = "multinom",
                      trace = FALSE)
predictions.m <- predict(iris.caret.m, test[, 1:4])
table(predictions.m, test$Species)


# Tree-Based Methods

library(caret)
library(randomForest)

cycling <- read.table("cycling.txt", sep = "\t", header = T)
fit <- randomForest(factor(Result) ~ ., data = cycling[2:5])
varImpPlot(fit)

library(rpart)

head(cu.summary)

fit <- rpart(
  Mileage ~ Price + Country + Reliability + Type,
  method = "anova", # method="class" for classificaiton tree
  data = cu.summary
)
plot(fit, uniform = TRUE, margin = 0.1)
text(fit, use.n = TRUE, all = TRUE, cex = .8)

# A simple decision tree plotted using the rpart() function from the rpart
# library; here, we start with feature Price and split accordingly

rsq.rpart(fit)[1]
plotcp(fit)

# You can add a dotted-line threshold that signifies the best cp value to choose
# —in this case, 0.058 and a tree size of 3

fit$cptable

fit.pruned <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,
                                                                "xerror"]), "CP"])
par(mfrow = c(1, 2))
plot(fit, uniform = TRUE, margin = 0.1, main = "Original Tree")
text(fit, use.n = TRUE, all = TRUE, cex = 0.8)
plot(fit.pruned, uniform = TRUE, margin = 0.1, main = "Pruned Tree")
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

## Decision Trees for Regression

cu.summary.complete <- cu.summary[complete.cases(cu.summary),]

data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) *
                         0.7, replace = FALSE)

training.data <- cu.summary.complete[data.samples, ]

test.data <- cu.summary.complete[-data.samples, ]

fit <- rpart(
  Mileage~Price + Country + Reliability + Type,
  method="anova", # method="class" for classification tree
  data=training.data
)

fit.pruned<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

prediction <- predict(fit.pruned, test.data)

output <- data.frame(test.data$Mileage, prediction)

RMSE = sqrt(sum((output$test.data.Mileage - output$prediction)^2) /
              nrow(output))
RMSE

# Decision Trees for Classification

cu.summary.complete <- cu.summary[complete.cases(cu.summary),
                                  ]
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) *
                         0.7, replace = FALSE)
training.data <- cu.summary.complete[data.samples, ]

test.data <- cu.summary.complete[-data.samples, ]

fit <- rpart(Type ~ Price + Country + Reliability + Mileage,
             method = "class", data = training.data)

fit.pruned <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,
                                                                "xerror"]), "CP"])

prediction <- predict(fit.pruned, test.data, type = "class")

table(prediction, test.data$Type)

# Conditional Inference Trees

library(party)

fit2 <- ctree(Mileage ~ Price + Country + Reliability + Type,
              data = na.omit(cu.summary))
plot(fit2)  # A decision tree plotted using the ctree() function from the party library

fit3 <- ctree(Type ~ Price + Country + Reliability + Mileage, data = na.omit(cu.summary))
plot(fit3)

# Other Advanced Methods

library(e1071)
library(caret)

breast_cancer <- data.frame(read.table("breast_cancer.txt", header = T,
                                       sep = "\t"))
names(breast_cancer) <- c("SampleCodeNumber", "ClumpThickness",
                          "UniformityofCellSize", "UniformityofCellShape", "MarginalAdhesion",
                          "SingleEpithelialCellSize", "BareNuclei", "BlandChromatin",
                          "NormalNucleoli", "Mitoses", "Class")

breast_cancer <- data.frame(sapply(breast_cancer, as.factor))

breast_cancer_features <- breast_cancer[, 2:11]

nb.model <- naiveBayes(Class ~ ., data = breast_cancer_features)

print(nb.model)

breast_cancer_complete <-
  breast_cancer_features[complete.cases(breast_cancer_features),
                         ]
breast_cancer_complete$Class <- as.factor(breast_cancer_complete$Class)
data.samples <- sample(1:nrow(breast_cancer_complete),
                       nrow(breast_cancer_complete) *
                         0.7, replace = FALSE)
training.data <- breast_cancer_complete[data.samples, ]
test.data <- breast_cancer_complete[-data.samples, ]
nb.model <- naiveBayes(Class ~ ., data = training.data)
prediction.nb <- predict(nb.model, test.data)
table(test.data$Class, prediction.nb)

# Principal Component Analysis

head(mtcars)

pairs(mtcars[, 1:7], lower.panel = NULL)

pca = princomp(mtcars, scores = TRUE, cor = TRUE)
summary(pca)
par(mfrow = c(1,1))
plot(pca)

pca$loadings[, 1:5]

scores.df <- data.frame(pca$scores)
scores.df$car <- row.names(scores.df)

plot(x = scores.df$Comp.1, y = scores.df$Comp.2, xlab = "Comp1 (mpg,cyl)",
     ylab = "Comp2 (qsec, gear, am)")
text(scores.df$Comp.1, scores.df$Comp.2, labels = scores.df$car,
     cex = 0.7, pos = 3)

# Linear Discriminant Analysis

iris.pca <- prcomp(iris[, -5], center = T, scale. = T)
iris.pca$sdev^2/sum(iris.pca$sdev^2)

table(iris$Species)

library(MASS)

iris.lda <- lda(Species ~ ., data = iris, prior = c(1/3, 1/3,
                                                    1/3))
iris.lda$svd^2/sum(iris.lda$svd^2)

iris.lda$scaling

iris.lda.prediction <- predict(iris.lda, newdata = iris)
table(iris.lda.prediction$class, iris$Species)

combined <- data.frame(Species = iris[, "Species"], pca = iris.pca$x,
                       lda = iris.lda.prediction$x)
library(ggplot2)
library(gridExtra)

lda.plot <- ggplot(combined) + geom_point(aes(lda.LD1, lda.LD2,
                                              shape = Species)) + scale_shape_manual(values = c(0, 1, 2))
pca.plot <- ggplot(combined) + geom_point(aes(pca.PC1, pca.PC2,
                                              shape = Species)) + scale_shape_manual(values = c(0, 1, 2))
grid.arrange(pca.plot, lda.plot) # tests this by comparing the outputs from PCA compared to LDA

# Support Vector Machines (SVMs)

library("e1071")

s <- sample(150, 100)

col <- c("Petal.Length", "Petal.Width", "Species")

iris_train <- iris[s, col]

iris_test <- iris[-s, col]

svmfit <- svm(Species ~ ., data = iris_train, kernel = "linear",
              cost = 0.1, scale = FALSE)

plot(svmfit, iris_train[, col])

tuned <- tune(svm, Species ~ ., data = iris_train, kernel = "linear",
              ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tuned)

svmfit <- svm(Species ~ ., data = iris_train, kernel = "linear",
              cost = 1, scale = FALSE)
plot(svmfit, iris_train[, col])

plot(x = cats$Hwt, y = cats$Bwt, pch = as.numeric(cats$Sex))

library(MASS)
library(e1071)
data(cats)

model <- svm(Sex ~ ., data = cats)
print(model)
summary(model)
plot(model, cats)

data.samples <- sample(1:nrow(cats), nrow(cats) * 0.7, replace = FALSE)
training.data <- cats[data.samples, ]
test.data <- cats[-data.samples, ]
svm.cats <- svm(Sex ~ ., data = training.data)
prediction.svm <- predict(svm.cats, test.data[, -1], type = "class")
table(test.data[, 1], prediction.svm)
