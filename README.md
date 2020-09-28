# Impact-of-ontario-rent-control-policy-on-rental-housing-starts--interrupted-time-series-analysis

Research Background: In October 1975, rent control was implemented in Ontario by the NDP government in an election campaign as a promise to the tenants who complained about the arbitrary increase in rent. Rent control stated that rent could be increased once in 12 months and the rent increase is permissible at a rate prescribed by the government. In case, landlords want to increase the rents due to non coverage of maintenance and financing costs and non-fulfilment of investment returns with the income that they receive from the tenants, a review of the rent could be done on a case by case basis. In November 1991, the democratic government exempted the buildings that were built after 1991 from rent control for 5 years. In 1997, the conservative government permanently excluded the building built after 1991 from rent control. In April 2017, the government passed a regulation that all the rental buildings irrespective of the ones built before and after 1991 are under rent control. This paper analyzes the effect of rent control in 1975 and regulation changes in 1991,1997 and 2017 on the rental housing starts in Ontario.

Packages used:

1. Tableone
2. car
3. stargazer
4. ggplot2
5. lmtest
6. prais

Data: The data consists of rental housing starts of Ontario from 1969 to 2018. It is a time series data with time in consecutive years from 1969 to 2018. The data was obtained from statistics Canada housing starts data and the subcomponent is the rental housing starts, (SMITH, 1988) and the work done by (Smith & Tomlinson, 1981). The geography of analysis was limited to Ontario. The data consists of 4 interventions:

1975:  Rent control was imposed in Ontario
1991 : Rent control on buildings built after November 1991 was abolished for a period of 5 years by the Democratic Government 
1997 : Conservative government regulated that rent control is not applicable to rental units built after 1991
2017:  Rent control is applicable to all the rental units in Ontario irrespective of the ones built after 1991

Functions used:
1. Descriptive statistics
2. Regression models
3. cbind
4. Durbin watson test
5. Prais winstein test
6. tapply
7. acf and pacf plots
