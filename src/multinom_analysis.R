library(nnet)

# get cleaned data
data <- t2dsci_data_wrangling("data/Assessment_Latest.csv")

# show table
with(data, table(X, Y))

# Set reference in response to Medium
data$Y <- relevel(data$Y, ref='Medium')

# Reg model
test <- multinom(Y~X, data=data)
summary(test)
broom::tidy(test)

# Calculate p-values (base is Medium)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Convert coefficients from log scale
exp(coef(test))
head(pp <- fitted(test))


df_pred <- data.frame(X = c("stats", "prog", "both", "neither"))
pred <- predict(test, newdata = df_pred, "probs")

# Probabilties
df_X <- data.frame(X = c("stats", "prog", "both", "neither"), each=45)
pp_X <- cbind(df_X, predict(test, newdata = df_X, "probs", se=TRUE))
by(pp_X[,3:7], pp_X$X, colMeans)


## melt data set to long for ggplot2
# c("Medium", "Easy", "Somewhat easy", "Somewhat hard", "Hard")
require(reshape2)
lpp <- melt(pp_X, id.vars = c("X"),
            value.name = "probability")
head(lpp)

lpp %>% filter(variable != 'each') %>%
  ggplot() +
    geom_bar(aes(x = X, y = probability), stat = 'identity') + facet_wrap(~variable)
