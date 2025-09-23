## SETUP

library(readxl)

setwd("/Users/jodi/Downloads/Econometrics - Course material/intro-to-econometrics/data")

data <- read_excel("Growth.xlsx")
summary(data) # 65 countries

data_without_malta <- data[data$country_name != "Malta", ]
summary(data_without_malta) # 64 countries


### START OF QUESTION A ###

# Construct a table that shows the sample mean, standard deviation,
# and minimum and maximum values for the series Growth, TradeShare,
# YearsSchool, Oil, Rev_Coups, Assassinations, and RGDP60. Include the
# appropriate units for all entries.

?data.frame

get_values_from_condition = function(cond) {
  c(cond(data$growth), cond(data$tradeshare), cond(data$yearsschool), 
    cond(data$oil), cond(data$rev_coups), cond(data$assasinations), 
    cond(data$rgdp60))
}


table <- data.frame(row.names = c("growth", "trade_share", "years_school",
                                 "oil", "rev_coups", "assasinations", "rgdp60"),
                    "sample_mean" = get_values_from_condition(mean),
                    "sd" = get_values_from_condition(sd),
                    "min" = get_values_from_condition(min),
                    "max" = get_values_from_condition(max))
table

#                 sample_mean          sd        min          max
# growth           1.9427154    1.8971198  -2.811944    7.1568546
# trade_share     0.5647030    0.2892703   0.140502    1.9926157
# years_school     3.9850769    2.5420004   0.200000   10.0699997
# oil              0.0000000    0.0000000   0.000000    0.0000000
# rev_coups        0.1674501    0.2246798   0.000000    0.9703704
# assasinations    0.2775641    0.4915284   0.000000    2.4666667
# rgdp60        3103.7846487 2512.6568457 366.999939 9895.0039062

### END OF QUESTION A ###

### START OF QUESTION B ###
# Run a regression of Growth on TradeShare, YearsSchool, Rev_Coups,
# Assassinations, and RGDP60. 
model = lm(growth ~ tradeshare + yearsschool + rev_coups
           + assasinations + rgdp60, data=data)

summary(model)

# What is the value of the coefficient on Rev_Coups? 
rev_coups_coeff = model$coefficients["rev_coups"]
rev_coups_coeff # -2.157503

# Meaning of this coeff: For each additional revolution/coup, the growth 
# will decrease by 2.157503%. Since this is greater than the average growth rate,
# a country expericening conflict will have negative growth rate. On the other
# hand, it's difficult to gauge whether the value is large/small irl, since it 
# depends on the growth rate of the country without revolution + how large/long 
# the revolution is.

### END OF QUESTION B ###


### START OF QUESTION C ###

# Growth rate for a country that has average values for all regressors
avr_everything_growth_rate = predict(model,
                      newdata = data.frame(
                        "tradeshare" = table[2, 1],
                        "yearsschool" = table[3, 1],
                        "oil" = table[4, 1],
                        "rev_coups" = table[5, 1],
                        "assasinations" = table[6, 1],
                        "rgdp60" = table[7, 1]
                      ))

avr_everything_growth_rate # 1.942715, the exact same as the sample mean of 1.9427154

### END OF QUESTION C ###


### START OF QUESTION D ###

# Repeat (c), but now assume that the country’s value for TradeShare is
# one standard deviation above the mean.

trade_share_1_sd_above_growth_rate = predict(model,
                      newdata = data.frame(
                        "tradeshare" = table[2, 1] + table[2, 2],
                        "yearsschool" = table[3, 1],
                        "oil" = table[4, 1],
                        "rev_coups" = table[5, 1],
                        "assasinations" = table[6, 1],
                        "rgdp60" = table[7, 1]
                      ))

trade_share_1_sd_above_growth_rate # 2.394468, way higher than the sample mean of 1.9427154

### END OF QUESTION D ###


### START OF QUESTION E ###

# Oil is omitted from the regression bc it has a sample mean of 0.00
# If oil is included in the regression, nothing would change about the 
# estimated regressor, and all statistics for oil would be NA anw.

# Case in point:
model_with_oil = lm(growth ~ tradeshare + yearsschool + rev_coups
           + assasinations + rgdp60 + oil, data=data)
summary(model_with_oil)


#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.4897603  0.6895996   0.710 0.480372    
# tradeshare     1.5616957  0.7579475   2.060 0.043776 *  
# yearsschool    0.5748461  0.1393379   4.126 0.000118 ***
#  rev_coups     -2.1575029  1.1102915  -1.943 0.056769 .  
# assasinations  0.3540784  0.4773943   0.742 0.461218    
# rgdp60        -0.0004693  0.0001482  -3.167 0.002441 ** 
# oil                   NA         NA      NA       NA    


# compared with
summary(model)


#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.4897603  0.6895996   0.710 0.480372    
# tradeshare     1.5616957  0.7579475   2.060 0.043776 *  
# yearsschool    0.5748461  0.1393379   4.126 0.000118 ***
# rev_coups     -2.1575029  1.1102915  -1.943 0.056769 .  
# assasinations  0.3540784  0.4773943   0.742 0.461218    
# rgdp60        -0.0004693  0.0001482  -3.167 0.002441 ** 

### END OF QUESTION E ###