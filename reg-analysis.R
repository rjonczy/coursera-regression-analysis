# data transformation
data(mtcars)
str(mtcars)

mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

# data exploratory
str(mtcars[,c('mpg', 'am')])
mtcars[,c('mpg', 'am')]

cor(mtcars)['mpg',]
aggregate(mpg ~ am, data = mtcars, mean)

# hypothessi testing
# comment: hypotesis (based on plot2): manual transmission is better in MPG than automatic transmission
t.test(mtcars$mpg ~ mtcars$am, conf.level=0.95)

# The p-value 0.001374 less than 0.05 shows we have to reject null hypothesis. There is significant difference Automatic and Manual transmission.



# regression analysis

fit.1 <- lm(mpg ~ am, data = mtcars)
summary(fit)
summary(fit)$coef

# comment
# Based on the Model only 33.8% of the variance is explained using the coefficient of determination.

fit.step <- step(lm(mpg ~ ., data = mtcars), direction = "both")
summary(fit.step)
summary(fit.step)$coef

sort(summary(lm(mpg ~ ., data=mtcars))$coefficients[,4])
# comment
# The most significant is weight, transmission type, 1/4 mile time and horsepower.

# comment
# Based of the model 84 percent of variation of mpg vs tramission type is explained via the coefficient of determination, R2.

bestfit <- lm(mpg ~ am + wt + qsec, data = mtcars)
anova(fit.1, bestfit)
summary(bestfit)


# conclusion
# The model explains 84% of difference between mpg and tranmission type; furthermore with an exceptionally low P-value one can conclude that the null hypothesis:Automatic transmissions have a better MPG rating vs Manual transmissions can be rejected, i.e. automatic tranmissions do not achieve a better MPG rating. 
#R^2 is 0.84 means 84% of the variability explained by this model. Next, we compare linear model mpg ~ am with the best model using ANOVA a.







# Appendix
# plot1: mpg histogram and kernel density
par(mfrow = c(2,1))
hist(mtcars$mpg, breaks = 10, xlab = "MPG", main = "MPG histogram")
plot(density(mtcars$mpg), main = "kernel density", xlab = "MPG")

# plot2: mpg by transmission type
par(mfrow = c(1,1))
boxplot(mpg ~ am, data = mtcars,
        col = c("blue", "green"),
        xlab = "Transmission Type",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type")


# plot3: residuals
par(mfrow = c(2,2))
plot(fit.2)
