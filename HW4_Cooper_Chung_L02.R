# Cooper Chung (Redacted Student ID)
# Fall 2022 BTMA 431 HW4

# Load required file for Question 1
load("btma.431.736.f2018.v2.rda")

### Question 1 ###
# Put into environment as something more simple to understand
grade.model <- btma.431.736.f2018

# Extract rows with no missing data
grade.model <- grade.model[complete.cases(grade.model), ]

# Attach the model so it's easier to reference
attach(grade.model)


### Question 1a ###
# Linear model regressing on final raw score, using all the other columns as predictors
final.predict <- lm(final.raw.score.excluding.bonus ~ ., data = grade.model)

# The coefficient estimate for final.project is 0.40348, or 0.40 to two decimals
summary(final.predict)


### Question 1b ###
# Create new columns on the model converting the averages to percentages for both the Homework and Textbook Quizzes
grade.model$HW.percentage <- (grade.model$HW.average / 20) * 100
grade.model$TB.percentage <- (grade.model$textbook.quiz.average / 15) * 100

# Re-do the regression using these new percentages
final.predict.percentage <- lm(final.raw.score.excluding.bonus ~ final.project + post.retake.midterm + BANA + HW.percentage + TB.percentage, data = grade.model)

# Using percentages, we can see that the coefficient estimates and the Std. Error change for the Homework and Textbook, compared to if we used the averages instead.
summary(final.predict.percentage)


### Question 1c ###
# From the regression model performed in Question 1b, the P-Value of being a BANA student is 0.35741 (0.36 to two decimals). Since this is greater than 0.05, we can say that
# BANA students do not perform statistically better than non-BANA students. We do not have evidence to reject the hypothesis


### Question 1d ###
# Repeat the regression model performed in Question 1b, but include the interaction term between the post.retake.midterm score and BANA category
final.predict.BANA <- lm(final.raw.score.excluding.bonus ~ final.project + post.retake.midterm * BANA + HW.percentage + TB.percentage, data = grade.model)

# From this regression model, the P-Value of post.retake.midterm:BANAYes is 0.864794, or 0.86 to two decimals
summary(final.predict.BANA)


### Question 1e ###
# Repeat the regression model performed in Question 1b, but log() the response variable and the numeric predictors, and exclude the BANA category
final.predict.LOG <- lm(log(final.raw.score.excluding.bonus) ~ log(final.project) + log(post.retake.midterm) + log(HW.percentage) + log(TB.percentage), data = grade.model)

# From this regression model, the coefficient estimate of log(final.project) is 0.38065, or 0.38 to two decimals
summary(final.predict.LOG)

# To compare, repeat the regression model but get rid of all the log()'s
final.predict.noLOG <- lm(final.raw.score.excluding.bonus ~ final.project + post.retake.midterm + HW.percentage + TB.percentage, data = grade.model)

# As a result, the coefficient estimates and Std. error's are all similar but slightly off
summary(final.predict.noLOG)


### Question 2a ###
# Initiate marginal cost variable to use later
marginal.cost <- 1

# Create a dataframe with possible prices ranging from 1 to 9, in increments of 0.01. Rename this column to "prices"
price.totals <- as.data.frame(seq(from = 1, to = 9, by = 0.01))
names(price.totals) <- c("prices")

# Create new column in the same dataframe using the formula mentioned in the question to calculate the amount sold at each price level
price.totals$qsold <- 50 - (5 * price.totals$prices)

# Create new column in the same dataframe calculating the profit at each price level
price.totals$profit <- ((price.totals$qsold * price.totals$prices) - (marginal.cost * price.totals$qsold))

# Return the row which maximizes the profit column (optimal solution). In this case, the optimal price would be $5.50, with a profit of $101.25
price.totals[which.max(price.totals$profit),]


### Question 2b ###
# Repeat Question 1a but with different values of M
# M = 45
# For M = 45, the optimal price is $5.00, and the profit is $80.00
price.totals.45 <- as.data.frame(seq(from = 1, to = 9, by = 0.01))
names(price.totals.45) <- c("prices")
price.totals.45$qsold <- 45 - (5 * price.totals.45$prices)
price.totals.45$profit <- ((price.totals.45$qsold * price.totals.45$prices) - (marginal.cost * price.totals.45$qsold))
price.totals.45[which.max(price.totals.45$profit),]

# M = 55
# For M = 55, the optimal price is $6.00, and the profit is $125.00
price.totals.55 <- as.data.frame(seq(from = 1, to = 9, by = 0.01))
names(price.totals.55) <- c("prices")
price.totals.55$qsold <- 55 - (5 * price.totals.55$prices)
price.totals.55$profit <- ((price.totals.55$qsold * price.totals.55$prices) - (marginal.cost * price.totals.55$qsold))
price.totals.55[which.max(price.totals.55$profit), ]


### Question 2c ###
# Load library needed to create plot later on
library(ggplot2)

# Create a list of M values that we're interested in finding the optimal price of
m.values <- seq(from = 40, to = 60, by = 1)

# Create an empty data frame to append values to (used in function)
optimal.prices <- data.frame()

# Create function. To use m.op.finder, feed it the list of M values.
# The contents of this function essentially repeat what was done in Question 2b, but with different values of M
m.op.finder <- function(mvalue){
  price.total.m <- as.data.frame(seq(from = 1, to = 9, by = 0.01)) # Create a dataframe of prices
  names(price.total.m) <- c("prices")                              # Rename this column to something that makes more sense
  price.total.m$qsold <- mvalue - (5 * price.total.m$prices)       # Calculate the quantity sold, but utilizing the list of M values in the list given to the function
  price.total.m$profit <- ((price.total.m$qsold * price.total.m$prices) - (marginal.cost * price.total.m$qsold)) # Calculate profit
  optimal.prices <- append(optimal.prices, price.total.m[which.max(price.total.m$profit), 1])                    # Append the optimal price, to the empty dataframe created earlier
}

# Execute the function created earlier on the list of M values
optimal.price.list <- sapply(m.values, m.op.finder)

# Create a new dataframe consisting of the M values
m.values.df <- as.data.frame(m.values)

# Append the optimal prices from the optimal price list
m.values.df$optimal.prices <- unlist(optimal.price.list)

# Rename the columns so they make sense, and are easier to work with when plotting
names(m.values.df) <- c("mValues", "OptimalPrices")

# Create plot
ggplot(data = m.values.df, 
       aes(x = mValues, y = OptimalPrices)) +
  geom_line() + 
  labs(x = 'M Value', y = 'Optimal Price') +
  ggtitle("M Value vs Optimal Price") +
  theme(plot.title = element_text(hjust = 0.5))


### Question 2d ###
# Repeat Question 2c but this time with set M values of 45 and 55, K values of 2 - 8, and prices from 1 to 15
# Create list of K values
k.values <- seq(from = 2, to = 8, by = 1)

# Create 2 empty dataframes to put values into, one for each M value
optimal.prices.k.45 <- data.frame()
optimal.prices.k.55 <- data.frame()

# Function that takes a list of K values, and finds the optimal price for an M value of 45
k.finder.45 <- function(kValues){
  price.total.k.45 <- as.data.frame(seq(from = 1, to = 15, by = 0.01))
  names(price.total.k.45) <- c("prices")
  price.total.k.45$qsold <- 45 - (kValues * price.total.k.45$prices)
  price.total.k.45$profit <- ((price.total.k.45$qsold * price.total.k.45$prices) - (marginal.cost * price.total.k.45$qsold))
  optimal.prices.k.45 <- append(optimal.prices.k.45, price.total.k.45[which.max(price.total.k.45$profit), 1])
}

# Execute the function on the list of K values
k.optimal.45 <- sapply(k.values, k.finder.45)

# Same as the function above but for an M value of 55
k.finder.55 <- function(kValues){
  price.total.k.55 <- as.data.frame(seq(from = 1, to = 15, by = 0.01))
  names(price.total.k.55) <- c("prices")
  price.total.k.55$qsold <- 55 - (kValues * price.total.k.55$prices)
  price.total.k.55$profit <- ((price.total.k.55$qsold * price.total.k.55$prices) - (marginal.cost * price.total.k.55$qsold))
  optimal.prices.k.55 <- append(optimal.prices.k.55, price.total.k.55[which.max(price.total.k.55$profit), 1])
}

# Execute the function on the list of K values
k.optimal.55 <- sapply(k.values, k.finder.55)

# Turn the list of K values created earlier into a dataframe
k.values.df <- as.data.frame(k.values)

# Append the optimal prices of each level of K, for each level of M
k.values.df$OptimalPrice45 <- unlist(k.optimal.45)
k.values.df$OptimalPrice55 <- unlist(k.optimal.55)

# Create a plot with the requirements detailed in the question
k.values.plot = ggplot() + 
  geom_line(data = k.values.df, aes(x = k.values, y = OptimalPrice55, color = "M = 55"), size = 1) +
  geom_line(data = k.values.df, aes(x = k.values, y = OptimalPrice45, color = "M = 45"), size = 1) +
  scale_colour_manual(name = "Legend", values = c("M = 45" = "red", "M = 55" = "blue")) +
  ggtitle("K Values vs Optimal Price (For M levels 45 and 55)") +
  xlab('K Values') +
  ylab('Optimal Price')

# Print the plot
print(k.values.plot)


### Question 2e ###
# Load Jill's sales data
load("salesData.rda")

# Create the support tool function. Pass this function a data set with prices and quantities sold
support.tool <- function(dataset){
  dataset$profit <- ((dataset$quantity * dataset$price) - (marginal.cost * dataset$quantity))             # Calculate profits from each row by multiplying the price and the quantity sold
  poly.model <- lm(profit ~ price + I(price^2), data = dataset)                                           # Create a polynomial regression, using price, and price^2 (Quadratic relationship -> 2nd order term) as predictors of profit
  optimal.price <- round(-poly.model$coefficients[[2]] / (2 * poly.model$coefficients[[3]]), digits = 1)  # Using the coefficients from the regression, use -b/2a to find the x coordinate of the vertex, AKA the Optimal Price
  return(optimal.price)                                                                                   # Return the optimal price rounded to the nearest tenth
}

# From the given sales data, the optimal price for Jill would be $3.93, or $3.90 rounded to the nearest tenth, to two decimals
support.tool(salesData)