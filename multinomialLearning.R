library(nnet)

data("iris")
head("iris")

summary(iris)

# full model
multinom.model <- multinom(Species~., data=iris)
summary(multinom.model)

# reduced model
multinom.model1 <- multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)
summary(multinom.model1)

# reduced model
multinom.model2 <- multinom(Species ~ Petal.Length+Petal.Width, data =iris)
summary(multinom.model2)