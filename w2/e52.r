## SETUP

library(readxl)

setwd("/Users/jodi/Downloads/Econometrics - Course material/intro-to-econometrics/data")

data <- read_excel("Growth.xlsx")
summary(data) # 65 countries

data_without_malta <- data[data$country_name != "Malta", ]
summary(data_without_malta) # 64 countries

# Run a regression on the data without Malta
linear_model = lm(growth ~ tradeshare, data=data_without_malta)
summary(linear_model)

slope = linear_model$coefficients["tradeshare"]
slope # 1.680905

?confint

### START OF QUESTION A ###

# Is the estimated regression slope statistically significant? That is, can you
# reject the null hypothesis H0: b1 = 0 vs. a two-sided alternative hypothesis 
# at the 10%, 5%, or 1% significance level?

# for the 10% significant level, slope is statistically significant 
# bc the range excludes 0
confint_90 = confint(linear_model, level = 0.9) # 0.03220284 3.329606

# for the 5% significant level, slope is NOT statistically significant 
# bc it the range includes 0
confint(linear_model, level = 0.95) # -0.2928046 3.654614

# similarly, for the 1% significant level, slope is NOT statistically significant 
# bc it the range includes 0
confint(linear_model, level = 0.99) # -0.9429897 4.304799

### END OF QUESTION A ###


### START OF QUESTION B ###

# What is the p-value associated with the coefficient’s t-statistic?

# For the intercept, the p-value is 0.1041, not significant 
# at the 5% level at all

# For the slope, the p-value is 0.0937, significant at the 10% level 

### END OF QUESTION B ###

### START OF QUESTION C ###

# Construct a 90% confidence interval for b1.
confint_90 # 0.03220284 3.329606

### END OF QUESTION C ###