library(corrplot)
library(RColorBrewer)

# loading 
data(mtcars)
head(mtcars, 6)

?mtcars

nrow(mtcars)


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

