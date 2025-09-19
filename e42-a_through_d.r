### SETUP

library(foreign)
library(tidyverse)

data <- read.dta("Earnings_and_Height.dta")
head(data)
summary(data)



### SOLUTIONS

### START OF QUESTION A ###

# What is the median value of the height in the sample?
heights <- data$height # set a variable for reuse
median_height <- median(heights)
median_height # 67

### END OF QUESTION A ###


### START OF QUESTION B ###

# Estimate average earnings for workers whose height is at most 67 inches.
heights_at_most_67 <- filter(data, data$height <= 67)
avr_earnings_when_height_at_most_67 <- mean(heights_at_most_67$earnings)
avr_earnings_when_height_at_most_67 # 44488.44

# Estimate average earnings for workers whose height is greater than 67 inches.
heights_greater_than_67 <- filter(data, height > 67)
avr_earnings_when_height_greater_than_67 <- mean(heights_greater_than_67$earnings)
avr_earnings_when_height_greater_than_67 # 49987.88

# On average, do taller workers earn more than shorter workers?
# How much more? What is a 95% confidence interval for the difference in
# average earnings?

# for the sake of the problem, let's just assume group with heights
# less than 67 to be shorter, and vice versa
difference <- avr_earnings_when_height_greater_than_67 - avr_earnings_when_height_at_most_67
difference # 5499.44
t.test(heights_greater_than_67$earnings, heights_at_most_67$earnings)
# 95% confidence interval is [4706.237, 6292.64]

### END OF QUESTION B ###


### START OF QUESTION C ###

# Construct a scatterplot of annual earnings (Earnings) on height (Height)
plot(data$earnings ~ data$height,
      xlab = "Height",
      ylab = "Earnings",
      main = "Scatterplot of Earnings on Height",
      pch = 20)

# The points on the plot fall along horizontal lines
# because of the following in the data description:
# In the survey, labor earnings are reported in 23 brackets 
# (for example, $26,000-$30,00).  For each of these brackets 
# Professors Case and Paxson estimated a value of average earnings 
# based on information in the Current Population, and these average values 
# were assigned to all workers with incomes in the corresponding bracket
# 
#
# Link to data description: https://www.princeton.edu/~mwatson/Stock-Watson_3u/Students/EE_Datasets/Earnings_and_Height_Description.docx

### END OF QUESTION C ###


### START OF QUESTION D ###
# Run a regression of Earnings on Height.
linear_model = lm(earnings ~ height, data=data)
linear_model
abline(linear_model)

slope <- linear_model$coefficients["height"]
slope # 707.6716
intercept <- linear_model$coefficients["(Intercept)"]
intercept # -512.7336

# Predict the earnings of the following workers
predict_earning_by_height = function(h) {
  intercept + slope * h
}

predictions = c(predict_earning_by_height(67), predict_earning_by_height(70),
predict_earning_by_height(65))

predictions # 46901.26    49024.28    45485.92
### END OF QUESTION D ###
