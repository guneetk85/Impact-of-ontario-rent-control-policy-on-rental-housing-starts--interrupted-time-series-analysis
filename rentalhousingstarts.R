# Research Project- Impact of Ontario rent control policy on rental housing
#starts
#Interrupted time series analysis


#the following packages need to be installed
install.packages("tableone") #create tableone package
install.packages("car") #car package
install.packages("lmtest") #linear test package
install.packages("prais") #prais winsten package
install.packages("ggplot2") #GG Plot package
install.packages("stargazer") #stargazer package

# activate the packages above

library(tableone)
library(car)
library(lmtest)
library(prais)
library(ggplot2)
library(stargazer)

names(HSR)

# 1] "cid"               "year"              "OHSrental_units"  
# [4] "time"              "rent_control_1975" "1975_trend"       
# [7] "1991_policy"       "1991_trend"        "1997_policy"      
# [10] "1997_pretrend"     "1997_trend"        "2017_policy"      
# [13] "2017_trend"       

View(HSR)

#First intervention-rent control-1975

attach(HSR)

#descriptive statistics


HSR$rent_control_1975= factor(HSR$rent_control_1975,
                              labels = c("before 1975","after 1975"))

HSR_1975_Mean<- tapply(HSR$OHSrental,HSR$rent_control_1975, mean)
HSR_1975_Mean

# before 1975  after 1975 
# 37640.667    7427.318 

#standard deviation of Rental housing starts before and after 1975
HSR_1975_SD<- tapply(HSR$OHSrental,HSR$rent_control_1975, sd)
HSR_1975_SD

# before 1975  after 1975 
# 8167.244    5014.570

#number of observations before and after 1975
HSR_1975_obs<-HSR_1975_SD<- tapply(HSR$OHSrental,HSR$rent_control_1975, length)
HSR_1975_obs

# before 1975  after 1975 
#       6          44

cbind(HSR_1975_Mean,HSR_1975_SD,HSR_1975_obs)
# 
#                HSR_1975_Mean  HSR_1975_SD   HSR_1975_obs
# before 1975     37640.667           6            6
# after 1975       7427.318          44           44

#Visualizing rent control-1975

plot(HSR$OHSrental_units, col="green", pch=19,
     main = "Rental housing starts after 1st intervention",
     ylab = "Rental housing starts in Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")
     axis(1, at=1:367, labels = HSR$year)
     abline(v=7, lty=2)
     

attach(HSR)

mod.1975=lm(HSR$OHSrental_units~time+rent_control_1975+`1975_trend`,
            data = HSR)
summary(mod.1975)


# Call:
#     lm(formula = HSR$OHSrental_units ~ time + rent_control_1975 + 
#            `1975_trend`, data = HSR)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -9056.5 -2856.5  -232.9  2769.3  9758.2 
# 
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  46494.5     3862.2  12.038 8.11e-16 ***
#     time                         -2529.7      991.7  -2.551   0.0141 *  
#     rent_control_1975after 1975 -17850.5     3261.1  -5.474 1.77e-06 ***
#     `1975_trend`                  2261.3      993.0   2.277   0.0275 *  
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4149 on 46 degrees of freedom
# (317 observations deleted due to missingness)
# Multiple R-squared:  0.873,	Adjusted R-squared:  0.8647 
# F-statistic: 105.4 on 3 and 46 DF,  p-value: < 2.2e-16


# At the beginning of time that is at time 0(before 1969) the rental starts 
# were 46494 units. Over time, the rental starts decreased by 2529 units 
# with each additional year, 
# but only up to the intervention (1975). The result is significant with 
# t value>1.96 and p value<0.05. At the intervention, 
# the rental starts declined by 17850 units. After the intervention 
# that is, after 1975, the rental starts increased by 2261 units per year. 
# The rental starts at the intervention and after the intervention 
# define a statistically significant relationship which is evident from 
# the respective t and p values.

# DURBIN-WATSON TEST(TO TEST SERIAL AUTOCORRELATION ) AND PRAIS WINSTON 
#TEST-RENT CONTROL LAW 1975
library(car)
library(lmtest)
library(prais)

durbin.watson(mod.1975, max.lag=50, alternative="two sided")
dt.1975<-dwt(mod.1975, max.lag=50)
dt.1975


# lag Autocorrelation D-W Statistic p-value
# 1    0.5782759960     0.7890363   0.000
# 2    0.1616196426     1.5821733   0.140
# 3    0.0511448200     1.7623631   0.506
# 4    0.0770248832     1.5690524   0.234
# 5    0.2331253943     1.2377566   0.010
# 6    0.1731105366     1.2535086   0.028
# 7    0.0404316917     1.5060481   0.306
# 8   -0.0452061222     1.6754990   0.738
# 9   -0.0199291212     1.6153746   0.698
# 10    0.0780562291     1.3235075   0.176
# 11   -0.0015978780     1.4820019   0.538
# 12   -0.0883027879     1.6516207   0.820
# 13   -0.0642318263     1.5927069   0.856
# 14   -0.1145563451     1.6631365   0.528
# 15   -0.2891414833     1.9682201   0.054
# 16   -0.3902371418     2.1662346   0.004
# 17   -0.3584469711     2.0885134   0.000
# 18   -0.2900448209     1.9351453   0.010
# 19   -0.1741716964     1.6662673   0.120
# 20   -0.1268409242     1.5217941   0.268
# 21   -0.1820826562     1.5869641   0.090
# 22   -0.1944267694     1.5509545   0.074
# 23   -0.1273001158     1.3366566   0.222
# 24   -0.0562332130     1.1352124   0.694
# 25    0.0051106976     0.9897786   0.934
# 26    0.0289834451     0.9192872   0.864
# 27   -0.0008456034     0.9196348   0.970
# 28   -0.0276939600     0.8932870   0.904
# 29   -0.0266494520     0.8305001   0.998
# 30    0.0046070570     0.7226736   0.808
# 31    0.0423977560     0.5972806   0.468
# 32    0.0615016194     0.5219411   0.402
# 33    0.0932027539     0.4419750   0.252
# 34    0.0855849651     0.4430697   0.340
# 35    0.0795920572     0.4508788   0.534
# 36    0.0652373120     0.4355016   0.654
# 37    0.0584772667     0.4188023   0.712
# 38    0.0568038075     0.4113773   0.916
# 39    0.0724354908     0.3763229   0.980
# 40    0.0569325588     0.4065149   0.704
# 41   -0.0279359850     0.4803556   0.298
# 42   -0.0396998700     0.4943130   0.150
# 43   -0.0057199621     0.4245284   0.180
# 44    0.0238156261     0.3526390   0.220
# 45    0.0728141293     0.1503644   0.922
# 46    0.0436032567     0.1896914   0.518
# 47   -0.0224985601     0.1803444   0.286
# 48   -0.0436308504     0.1818491   0.140
# 49   -0.0264648767     0.1073415   0.114
# 50              NA            NA      NA
# Alternative hypothesis: rho[lag] != 0
#The model suggests that there exists serial autocorrelation between 
#variables as the value of d-w statistic lies between 0 and 2 showing
#positive autocorrelation


#PLOTTING RESIDUALS

plot(residuals(mod.1975),
     type = 'o',
     pch=16,
     main = "1975 Rent Control Residuals",
     xlab = "time",
     ylab="Residuals",
     col="purple",
     xaxt="n")
abline(h=0, lty=2)
axis(1,at=1:367, labels=HSR$year)
acf(residuals(mod.1975), main="Residuals for 1975 rent control model")
acf(residuals(mod.1975), type = "partial", main="Residuals(partial)
    for 1975 rent control model")

#################################################
#SECOND INTERVENTION IN 1991, POLICY AMMENDENTMENT
##################################################

#Descriptive Statistics

attach(HSR)


HSR$RC1991=factor(HSR$`1991_policy`, labels=c("Before 1991","After 1991"))

HSR_1991_MEAN= tapply(HSR$OHSrental_units, HSR$RC1991, mean)
HSR_1991_MEAN

HSR_1991_SD= tapply(HSR$OHSrental_units, HSR$RC1991, sd)
HSR_1991_SD

#visualizing the 1991 policy impact

attach(HSR)

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot2:1991 policy",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=23, col="black",lty=2)                                                       



#linear models

mod.1991<-lm(HSR$OHSrental_units~time+`1991_policy`+`1991_trend`,data = HSR)
summary(mod.1991)




# Coefficients:
#               Estimate Std. Error t value    Pr(>|t|)    
# (Intercept)    50216.1     3809.8  13.181     < 2e-16 ***
#     time           -4525.0      744.5  -6.078 0.000000221 ***
#     `1991_policy`   3310.0      800.6   4.134    0.000149 ***
#     `1991_trend`    4397.7      721.3   6.097 0.000000207 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4754 on 46 degrees of freedom
# (317 observations deleted due to missingness)
# Multiple R-squared:  0.8333,	Adjusted R-squared:  0.8224 
# F-statistic: 76.62 on 3 and 46 DF,  p-value: < 2.2e-16


# The model suggests that, at the beginning of time that at time 0, 
# the rental starts were 37250 units. Over time, the rental starts 
# decreased by 1562 units with each additional year, but only up to 
# the intervention (1991). The result is significant with t value>1.96 
# and p value<0.05. At the intervention, the rental starts increased by 
# 1955 units but the difference is not statistically significant. 
# After the intervention that is, after 1991, the rental starts 
# increased by 1546 units per year. The rental starts after the 
# intervention define a statistically significant relationship 
# which is evident from the respective t and p values. 
# The model explains 75% of the variance.

#DURBIN WATSON TEST

durbinWatsonTest(mod.1991, max.lag = 50, alternative = "two sided")
dt.1991<- dwt(mod.1991, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.1991),
     type = 'o',
     pch=16,
     main = "1991 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

#residuals for 1991 policy
acf(residuals(mod.1991), main="Residuals for 1991 policy impact model")
acf(residuals(mod.1991),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")



####################################################################
#Third Intervention Policy Ammendment:1997
###################################################################

#Descriptive Statistics

attach(HSR)
names(HSR)
HSR$RC1997=factor(HSR$`1997_policy`, labels=c("Before 1997","After 1997"))

HSR_1997_MEAN= tapply(HSR$OHSrental_units, HSR$RC1997, mean)
HSR_1997_MEAN

# Before 1997  After 1997 
# 16646.893    3933.318 

HSR_1997_SD= tapply(HSR$OHSrental_units, HSR$RC1997, sd)
HSR_1997_SD

# Before 1997  After 1997 
# 12434.982    1803.472 

#Number of observations before and after 1997
HSR_1997_OBS= tapply(HSR$OHSrental_units, HSR$RC1997, length)
HSR_1997_OBS


# Before 1997  After 1997 
# 28          22 

#VISUALIZATION OF 1997 POLICY IMPACT

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot3:1997 policy impact",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=29, col="black",lty=2)            

#linear models- 1997 policy impact
mod.1997<-lm(HSR$OHSrental_units~time+`1997_policy`+`1997_trend`,data = HSR)
summary(mod.1997)

# Residuals:
#     Min       1Q   Median       3Q      Max 
# -15459.2  -1230.7    -79.3   1362.9  16598.2 
# 
# Coefficients:
#                Estimate Std. Error t value         Pr(>|t|)    
# (Intercept)    34445.8     2168.8  15.883          < 2e-16 ***
#     time           -1227.5      130.7  -9.395 0.00000000000285 ***
#     `1997_policy`    977.2     3209.6   0.304            0.762    
# `1997_trend`    1478.0      228.7   6.463 0.00000005839347 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5585 on 46 degrees of freedom
# (317 observations deleted due to missingness)
# Multiple R-squared:  0.7699,	Adjusted R-squared:  0.7549 
# F-statistic: 51.29 on 3 and 46 DF,  p-value: 1.028e-14

#DURBIN WATSON TEST

durbinWatsonTest(mod.1997, max.lag = 50, alternative = "two sided")
dt.1997<- dwt(mod.1997, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.1997),
     type = 'o',
     pch=16,
     main = "1997 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

acf(residuals(mod.1997), main="Residuals for 1997 policy impact model")
acf(residuals(mod.1997),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")

##############################################################
# 4th intervention-policy ammendement:2017
##############################################################

#Descriptive statistics-2017 Policy Impact


attach(HSR)
names(HSR)
HSR$RC2017=factor(HSR$`2017_policy`, labels=c("Before 2017","After 2017"))

#average rental housing starts before and after 2017
HSR_2017_MEAN= tapply(HSR$OHSrental_units, HSR$RC2017, mean)
HSR_2017_MEAN

# Before 2017  After 2017 
# 11230.42     6793.00 

#standard deviation of housing starts before and after 2017
HSR_2017_SD= tapply(HSR$OHSrental_units, HSR$RC2017, sd)
HSR_2017_SD

# Before 2017  After 2017 
# 11482.4715     21.2132 

#No. of observations before and after 2017
HSR_2017_OBS= tapply(HSR$OHSrental_units, HSR$RC2017, length)
HSR_2017_OBS

# Before 2017  After 2017 
# 48           2

#VISUALIZATION OF 2017 POLICY IMPACT

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot 4:2017 policy impact",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=49, col="black",lty=2)            

#linear models- 2017 policy impact
mod.2017<-lm(HSR$OHSrental_units~time+`2017_policy`+`2017_trend`,data = HSR)
summary(mod.2017)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   26452.92    2221.77  11.906 1.19e-15 ***
#     time           -621.33      78.94  -7.871 4.59e-10 ***
#     `2017_policy` 10770.08    7895.55   1.364    0.179    
# `2017_trend`    651.33   10715.08   0.061    0.952    
# ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7577 on 46 degrees of freedom
# (317 observations deleted due to missingness)
# Multiple R-squared:  0.5765,	Adjusted R-squared:  0.5488 
# F-statistic: 20.87 on 3 and 46 DF,  p-value: 0.00000001111

#DURBIN WATSON TEST

durbinWatsonTest(mod.2017, max.lag = 50, alternative = "two sided")
dt.2017<- dwt(mod.2017, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.2017),
     type = 'o',
     pch=16,
     main = "1997 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

acf(residuals(mod.2017), main="Residuals for 1997 policy impact model")
acf(residuals(mod.2017),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")

