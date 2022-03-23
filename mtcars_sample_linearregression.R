library(corrplot)
library(RColorBrewer)
library(randomForest)

# loading 
data(mtcars)
head(mtcars, 6)

plot(mtcars$mpg, mtcars$wt, xlab="weight", ylab="mpg", main = "MTCARS MPG", col="Red")
hist(mtcars$wt, col = "blue")

# Not a good summary. Full model is not good.
lm_model_mtcars <- lm(mpg ~ ., data = mtcars)
summary(lm_model_mtcars)

# Reducing the model using data from the correlation matrix
lm_model_mtcars_1 <- lm(mpg ~ vs + qsec, data=mtcars)
summary(lm_model_mtcars_1)


# Mpg has a positive correlation with vs and qsec vasriables. 
M <-cor(mtcars)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# random forest
rf_model_1 <- randomForest(mpg~., data=mtcars)
summary(rf_model_1)

rf_model_2 <- randomForest(mpg~wt, data=mtcars)
summary(rf_model_2)

# creating a training and testing set
dt = sort(sample(nrow(mtcars), nrow(mtcars)*.7))
train <- mtcars[dt,]
test<- mtcars[-dt,]

# full model is still not great
lm_train <- lm(mpg ~., train)
summary(lm_train)
pred_lm = predict(lm_train, test$mpg)
mse = (pred_lm-test)**2
mse

# reduced model from before
lm_model_reduced_1 <- lm(mpg ~ vs, train)
summary(lm_model_reduced_1)
pred_lm1 = predict(lm_model_reduced_1, test)
mse1 = (pred_lm1- test)**2
mse1
plot(mtcars$vs, mtcars$mpg)

# reduced model from before
lm_model_reduced_2 <- lm(mpg ~qsec, train)
summary(lm_model_reduced_2)
pred_lm2 = predict(lm_model_reduced_2, test)
mse2 = (pred_lm2- test)**2
mse2
plot(mtcars$qsec, mtcars$mpg, main = "qsec vs mpg")
abline(lm_model_reduced_2, col="red"+
         
         
         )

# reduced model from before
lm_model_reduced_3 <- lm(mpg ~wt, train)
summary(lm_model_reduced_3)
pred_lm3 = predict(lm_model_reduced_3, test)
mse3 = (pred_lm3- test)**2
mse3
plot(mtcars$wt, mtcars$mpg, main = "wt vs mpg")
abline(lm_model_reduced_3, col="red")


