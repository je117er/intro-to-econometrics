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


### START OF QUESTION E ###
# Suppose height were measured in centimeters instead of inches. Answer
# the following questions about the Earnings on Height (in cm) regression

# i. What is the estimated slope of the regression?
data$height_in_cm <- data$height * 2.54
linear_model_with_heights_measured_in_cm <- lm(earnings ~ height_in_cm, data=data)
linear_model_with_heights_measured_in_cm 

slope <- linear_model_with_heights_measured_in_cm$coefficients["height_in_cm"]
slope # 278.6108

## ii. What is the estimated intercept of the regression?
intercept <- linear_model_with_heights_measured_in_cm$coefficients["(Intercept)"]
intercept # -512.7336


### iii. What is the R^2 of the regression?
summary_stats_cm = summary(linear_model_with_heights_measured_in_cm)
summary_stats_cm
R_squared = summary_stats_cm$r.squared
R_squared # 0.0108753

### iv. What is the SE of the regression?
SE = summary_stats_cm$sigma
SE # 26777.24

### END OF QUESTION E ###


### START OF QUESTION F ###

# Run a regression of Earnings on Height, using data for female workers only
# i. What is the estimated slope?
?ifelse
?subset

linear_model_with_female_only_heights = lm(earnings ~ height, 
                                           data=data,
                                           subset = sex == '0:female')
linear_model_with_female_only_heights
slope_for_female_only_heights = linear_model_with_female_only_heights$coefficients["height"]
slope_for_female_only_heights # 511.2222

# ii. A randomly selected woman is 1 inch taller than the average
# woman in the sample. Would you predict her earnings to be higher
# or lower than the average earnings for women in the sample? By how much?

# Ans: Higher than the avr earnings for women in the sample, bc the slope is 
# positive. This woman's earnings will be exactly $(slope) more than the avr
# earnings for women in the sample
difference_in_earnings = slope_for_female_only_heights * 1
difference_in_earnings

### END OF QUESTION F ###


### START OF QUESTION G ###

# Run a regression of Earnings on Height, using data for male workers only
# i. What is the estimated slope?
linear_model_with_male_only_heights = lm(earnings ~ height, 
                                           data=data,
                                           subset = sex == '1:male')
linear_model_with_male_only_heights
slope_for_male_only_heights = linear_model_with_male_only_heights$coefficients["height"]
slope_for_male_only_heights # 1306.86

# ii. A randomly selected woman is 1 inch taller than the average
# woman in the sample. Would you predict her earnings to be higher
# or lower than the average earnings for women in the sample? By how much?

# Ans: Similar to the case of women's earnings, the guy's earnings will be
# higher than the avr earnings for the guys in the sample, bc the slope is 
# positive. This guy's earnings will be exactly $(slope) more than the avr
# earnings for women in the sample
difference_in_earnings_for_male = slope_for_male_only_heights * 1
difference_in_earnings_for_male

### END OF QUESTION G ###


### START OF QUESTION H ###

# Do you think that height is uncorrelated with other factors that cause
# earning? That is, do you think that the regression error term, ui has a
# conditional mean of 0 given Height (Xi)?

# Ans: Would have to be if the OLS assumptions were to be valid.
# Although in reality, I'd expect height to be dependent on other variables 
# as well, such as nutrition, family/education background, etc

### END OF QUESTION H ###