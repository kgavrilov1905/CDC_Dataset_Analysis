ObesityModel
================
Kaiyan Li, Kirill Gavrilov, Victor Wang
28/11/2020

# Introduction

Center for Disease Control (CDC) collected data from distinct
demographic groups throughout 2011 to 2016 in all states of America.
Demographic groups include: age, income, education level, ethnicity and
gender. Each group was sampled for 9 different categories:

  - Percent of adults aged 18 years and older who have obesity
  - Percent of adults aged 18 years and older who have an overweight
    classification
  - Percent of adults who report consuming fruit less than one time
    daily
  - Percent of adults who report consuming vegetables less than one time
    daily
  - Percent of adults who engage in muscle-strengthening activities on 2
    or more days a week
  - Percent of adults who achieve at least 150 minutes a week of
    moderate-intensity aerobic physical activity or 75 minutes a week of
    vigorous-intensity aerobic activity (or an equivalent combination)
  - Percent of adults who achieve at least 150 minutes a week of
    moderate-intensity aerobic physical activity or 75 minutes a week of
    vigorous-intensity aerobic physical activity and engage in
    muscle-strengthening activities on 2 or more days a week
  - Percent of adults who achieve at least 300 minutes a week of
    moderate-intensity aerobic physical activity or 150 minutes a week
    of vigorous-intensity aerobic activity (or an equivalent
    combination)
  - Percent of adults who engage in no leisure-time physical activity

Our objective is to find whether or not there’s relationship between any
of these classifications. We would like to answer these questions:

1.  Is there relationship between *obesity rates* and *fruit/vegetable
    intake alongside physical activity*?
2.  Is there evidence that *obesity rates* in the USA are growing?
3.  Add more later

Firstly we need to analyze the original dataset, remove all the
unnecessary variables and observations, add a new point to the dataset
to make it unique. Since we are mainly going to be working with the
obesity percentage data points, we will fill in the missing value for
obesity % in Alabama in 2011, specifically for “Other” ethnicity.
Particular value was chosen by looking at near by states at that year
for that demographic.

``` r
library(tidyverse)
library(faraway)
cdc = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# Removing LocationAbbr, and renaming LocationDesc, DataSource, Topic, TopicID,
# ClassID, GeoLocation, Data_Value_Unit, Data_Value_Type, DataValueTypeID,
# Data_Value_Footnote_Symbol, StratificationCategoryId1, StratificationID1
cdc_adjusted = cdc %>% select(-YearEnd, -LocationAbbr, -Datasource, -Topic, -TopicID, 
    -ClassID, -GeoLocation, -Data_Value_Unit, -Data_Value_Type, -DataValueTypeID, 
    -Data_Value_Footnote_Symbol, -StratificationCategoryId1, -StratificationID1, 
    -Data_Value_Alt, -Data_Value_Footnote, -Low_Confidence_Limit, -High_Confidence_Limit) %>% 
    rename(Year = YearStart, Location = LocationDesc)

# Removing Virigin Islands because they have observations for only 2016
cdc_adjusted = cdc_adjusted[!(cdc_adjusted$Location == "Virgin Islands"), ]
cdc_adjusted[28, 5] = 30.2
cdc_adjusted[28, 6] = 64
```

We now want to analyze the ordinary least squares model that relates
obesity rates to people who report eating fruit and vegetable less than
1 time a day and engage in no physical activity

``` r
model <- lm(Data_Value36 ~ Data_Value18 + Data_Value19 + Data_Value47, data = total)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value19 + Data_Value47, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.772  -2.566   0.511   3.319  40.673 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.692237   0.541529  19.745   <2e-16 ***
    ## Data_Value18  0.201578   0.017340  11.625   <2e-16 ***
    ## Data_Value19 -0.009529   0.019091  -0.499    0.618    
    ## Data_Value47  0.378433   0.014444  26.201   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.728 on 3984 degrees of freedom
    ## Multiple R-squared:  0.2975, Adjusted R-squared:  0.297 
    ## F-statistic: 562.5 on 3 and 3984 DF,  p-value: < 2.2e-16

``` r
# Using Backward Elimination method, remove the variable Data_Value19
model <- lm(Data_Value36 ~ Data_Value18 + Data_Value47, data = total)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47, data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.819  -2.566   0.531   3.313  40.451 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.72451    0.53760   19.95   <2e-16 ***
    ## Data_Value18  0.19699    0.01470   13.40   <2e-16 ***
    ## Data_Value47  0.37565    0.01333   28.19   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.727 on 3985 degrees of freedom
    ## Multiple R-squared:  0.2975, Adjusted R-squared:  0.2971 
    ## F-statistic: 843.8 on 2 and 3985 DF,  p-value: < 2.2e-16

``` r
# Test if collinearity problem exists
vif(model)  # < 10, no serious collinearity problem
```

    ## Data_Value18 Data_Value47 
    ##     1.273901     1.273901

``` r
plot(model)
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->![](ObesityModel_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->![](ObesityModel_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->![](ObesityModel_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
# From the Residuals vs Fitted plot, the residuals are uncorrelated and the
# constant variance assumption is satisfied.  From the Normal Q-Q plot, the
# residuals are not normally distributed.
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
# Let's try to do robust model to see if maybe that model is more applicable in
# our case
model_robust = rlm(Data_Value36 ~ Data_Value18 + Data_Value47, data = total)
summary(model_robust)$coefficient
```

    ##                   Value Std. Error  t value
    ## (Intercept)  10.8018580 0.44410037 24.32301
    ## Data_Value18  0.2363781 0.01213918 19.47234
    ## Data_Value47  0.3281886 0.01100872 29.81171

``` r
detach("package:MASS", unload = TRUE)
```

    ## Warning: 'MASS' namespace cannot be unloaded:
    ##   namespace 'MASS' is imported by 'lme4' so cannot be unloaded

# Try to fit different models

\[ obesity =\beta_0 + \beta_1fruit + \beta_2exercise + \beta_iIndicator_i\]

``` r
model_age <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Age, data = total)
summary(model_age)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47 + Age, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.399  -2.248   0.264   2.594  41.094 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      9.81005    0.49760  19.715  < 2e-16 ***
    ## Data_Value18     0.30070    0.01531  19.637  < 2e-16 ***
    ## Data_Value47     0.25716    0.01400  18.363  < 2e-16 ***
    ## Age18 - 24     -11.90797    0.46225 -25.761  < 2e-16 ***
    ## Age25 - 34      -0.91786    0.42539  -2.158  0.03101 *  
    ## Age35 - 44       3.29889    0.41855   7.882 4.14e-15 ***
    ## Age45 - 54       4.22800    0.41625  10.157  < 2e-16 ***
    ## Age55 - 64       4.54163    0.41927  10.832  < 2e-16 ***
    ## Age65 or older  -1.29409    0.45873  -2.821  0.00481 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.094 on 3979 degrees of freedom
    ## Multiple R-squared:  0.4451, Adjusted R-squared:  0.444 
    ## F-statistic: 398.9 on 8 and 3979 DF,  p-value: < 2.2e-16

``` r
# The result is significantly different for each age group.
ggplot(data = total) + geom_point(mapping = aes(x = Data_Value47, y = Data_Value36, 
    color = Age))
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(data = total) + geom_point(mapping = aes(x = Data_Value18, y = Data_Value36, 
    color = Age))
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# The plot indicates different models should be used for each age group.

model_gender <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Gender, data = total)
summary(model_gender)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47 + Gender, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.787  -2.573   0.513   3.310  40.494 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.66262    0.54668  19.504   <2e-16 ***
    ## Data_Value18  0.19865    0.01515  13.112   <2e-16 ***
    ## Data_Value47  0.37492    0.01350  27.781   <2e-16 ***
    ## GenderFemale  0.33870    0.47229   0.717    0.473    
    ## GenderMale    0.03627    0.47308   0.077    0.939    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.728 on 3983 degrees of freedom
    ## Multiple R-squared:  0.2976, Adjusted R-squared:  0.2969 
    ## F-statistic: 421.9 on 4 and 3983 DF,  p-value: < 2.2e-16

``` r
model_income <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Income, data = total)
summary(model_income)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47 + Income, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.346  -2.425   0.410   3.067  40.883 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               9.95390    0.55815  17.834  < 2e-16 ***
    ## Data_Value18              0.19774    0.01447  13.661  < 2e-16 ***
    ## Data_Value47              0.40168    0.01397  28.752  < 2e-16 ***
    ## Income$15,000 - $24,999   0.05904    0.46698   0.126 0.899395    
    ## Income$25,000 - $34,999   0.36412    0.45967   0.792 0.428339    
    ## Income$35,000 - $49,999   1.57426    0.45722   3.443 0.000581 ***
    ## Income$50,000 - $74,999   2.93542    0.46043   6.375 2.03e-10 ***
    ## Income$75,000 or greater  1.56152    0.47567   3.283 0.001037 ** 
    ## IncomeData not reported  -5.13141    0.45784 -11.208  < 2e-16 ***
    ## IncomeLess than $15,000  -0.27993    0.47621  -0.588 0.556672    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.596 on 3978 degrees of freedom
    ## Multiple R-squared:  0.3306, Adjusted R-squared:  0.3291 
    ## F-statistic: 218.3 on 9 and 3978 DF,  p-value: < 2.2e-16

``` r
model_education <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Education, data = total)
summary(model_education)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47 + Education, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.559  -2.464   0.495   3.234  40.947 
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                                9.77045    0.58210  16.785  < 2e-16
    ## Data_Value18                               0.20038    0.01478  13.557  < 2e-16
    ## Data_Value47                               0.40766    0.01441  28.284  < 2e-16
    ## EducationCollege graduate                  0.15380    0.49183   0.313    0.755
    ## EducationHigh school graduate             -0.64718    0.46859  -1.381    0.167
    ## EducationLess than high school            -2.86070    0.49840  -5.740 1.02e-08
    ## EducationSome college or technical school  2.34485    0.46429   5.050 4.61e-07
    ##                                              
    ## (Intercept)                               ***
    ## Data_Value18                              ***
    ## Data_Value47                              ***
    ## EducationCollege graduate                    
    ## EducationHigh school graduate                
    ## EducationLess than high school            ***
    ## EducationSome college or technical school ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.687 on 3981 degrees of freedom
    ## Multiple R-squared:  0.308,  Adjusted R-squared:  0.307 
    ## F-statistic: 295.4 on 6 and 3981 DF,  p-value: < 2.2e-16

``` r
model_race <- lm(Data_Value36 ~ Data_Value18 + Data_Value47 + Race.Ethnicity, data = total)
summary(model_race)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Data_Value18 + Data_Value47 + Race.Ethnicity, 
    ##     data = total)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.324  -2.571   0.409   3.015  33.900 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  13.03003    0.46341  28.118
    ## Data_Value18                                  0.15927    0.01260  12.637
    ## Data_Value47                                  0.34645    0.01144  30.279
    ## Race.Ethnicity2 or more races                 2.51604    0.41681   6.036
    ## Race.EthnicityAmerican Indian/Alaska Native   5.24824    0.49213  10.664
    ## Race.EthnicityAsian                         -17.13510    0.50029 -34.250
    ## Race.EthnicityHawaiian/Pacific Islander       9.32529    1.61277   5.782
    ## Race.EthnicityHispanic                        0.28636    0.40241   0.712
    ## Race.EthnicityNon-Hispanic Black              6.61236    0.44386  14.898
    ## Race.EthnicityNon-Hispanic White             -0.63334    0.39592  -1.600
    ## Race.EthnicityOther                          -1.86872    0.69422  -2.692
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  < 2e-16 ***
    ## Data_Value18                                 < 2e-16 ***
    ## Data_Value47                                 < 2e-16 ***
    ## Race.Ethnicity2 or more races               1.72e-09 ***
    ## Race.EthnicityAmerican Indian/Alaska Native  < 2e-16 ***
    ## Race.EthnicityAsian                          < 2e-16 ***
    ## Race.EthnicityHawaiian/Pacific Islander     7.94e-09 ***
    ## Race.EthnicityHispanic                       0.47674    
    ## Race.EthnicityNon-Hispanic Black             < 2e-16 ***
    ## Race.EthnicityNon-Hispanic White             0.10975    
    ## Race.EthnicityOther                          0.00714 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.831 on 3977 degrees of freedom
    ## Multiple R-squared:  0.5011, Adjusted R-squared:  0.4999 
    ## F-statistic: 399.5 on 10 and 3977 DF,  p-value: < 2.2e-16

``` r
ggplot(data = total) + geom_point(mapping = aes(x = Data_Value47, y = Data_Value36, 
    color = Race.Ethnicity))
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
ggplot(data = total) + geom_point(mapping = aes(x = Data_Value18, y = Data_Value36, 
    color = Race.Ethnicity))
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

## Analysis of total obesity rates for every state

Analyzing the ordinary least squares model of how obesity rates relate
to the year in order to conclude growing obesity rate in the country.
Specifically, we will look at the “Total” category which represents the
final obesity percentage for each state in a particular year.

\[ obesity =\beta_0 + \beta_1year + \epsilon\]

``` r
Q36 = Q36 %>% select(Year, Location, Class36, Question36, Data_Value36, Sample_Size36, 
    Total) %>% arrange(Location)
Q36[Q36 == ""] = NA
Q36 = Q36 %>% drop_na()
model_all_states = lm(Data_Value36 ~ Year, data = Q36)
summary(model_all_states)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Year, data = Q36)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.2098 -2.4132  0.0902  2.1668  7.8435 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -870.6594   224.9770  -3.870 0.000132 ***
    ## Year           0.4467     0.1117   3.998 7.96e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.403 on 317 degrees of freedom
    ## Multiple R-squared:  0.048,  Adjusted R-squared:  0.04499 
    ## F-statistic: 15.98 on 1 and 317 DF,  p-value: 7.959e-05

P-value for the model does seem to indicated that there’s evidence of
significant relationship between obesity rates and year, however the
R-squared is extremely low. Let’s take a look at the residuals.

``` r
par(mar = c(4, 4, 0.1, 0.1))
plot(model_all_states, which = c(2, 2))
plot(model_all_states, which = c(4, 4))
```

<img src="ObesityModel_files/figure-gfm/unnamed-chunk-6-1.png" width="50%" /><img src="ObesityModel_files/figure-gfm/unnamed-chunk-6-2.png" width="50%" />
Standardized residuals do not look out of place for the most part. There
are a couple of observations that fall out of the straight normality
line like \#307 and \#35, however that is expected. Out of 319
observations, 1% of those are expected to be potential outliers
according Normal Distribution. Taking a look at the Cook’s distance to
inspect any influential points, we find that gladly there aren’t any.
Obeservation \#307 appears again with the largest Cook’s distance of
just about 0.025. In order for the point to be considered influential,
it’s Cook’s distance should be greater than 1.

Let’s reduce our set of obeservations to only the states that show
significant evidence of increase obesity rates. Visualizing will also
help us draw any conclusions.

``` r
# ggplot(Q36, aes(x = Year, y = Data_Value36, colour = Location)) + geom_line()

reg_coef_all_states = Q36 %>% group_by(Location) %>% summarize(slope = lm(Data_Value36 ~ 
    Year)$coef["Year"], pvalue = coef(summary(lm(Data_Value36 ~ Year)))[2, 4])

Q36_reduced = reg_coef_all_states %>% filter(pvalue < 0.05) %>% left_join(Q36)

ggplot(Q36, aes(x = Year, y = Data_Value36)) + geom_line(aes(group = Location), colour = "grey", 
    alpha = 0.4) + geom_smooth(Q36_reduced, mapping = aes(x = Year, y = Data_Value36), 
    colour = "red") + geom_line(Q36_reduced, mapping = aes(x = Year, y = Data_Value36, 
    colour = Location), alpha = 0.35) + geom_smooth(colour = "black") + ggtitle("2011-2016 National Obesity Rates") + 
    xlab("Year") + ylab("Obesity %")
```

![](ObesityModel_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
Particular image represents total obesity rates graph of observations
for all states. Lines in grey colour are the states that do not show
significant increase in obesity rates in 6 years. That was deducted by
building an individual linear model for each state versus year. If the
p-value was greater than 0.05, that state is considered to not have
increasing obesity rates. In contrary, coloured lines indicate growing
obesity rates. Further more, red and black lines show the full model
slope for both, all states and states with growing obesity rates
respectively.

We can investigate this further by looking more closely at the
observations. States that have relatively low obesity rates in 2011,
tend to maintain that trend and hold obesity rates constant. On the
other hand, states with higher obesity rates initially, show evidence of
increasing rates.

``` r
model_updated = lm(Data_Value36 ~ Year, data = Q36_reduced)
summary(model_updated)
```

    ## 
    ## Call:
    ## lm(formula = Data_Value36 ~ Year, data = Q36_reduced)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.8156 -1.5451 -0.3539  1.5240  5.9549 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1213.0936   251.0881  -4.831 3.65e-06 ***
    ## Year            0.6176     0.1247   4.953 2.16e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.475 on 134 degrees of freedom
    ## Multiple R-squared:  0.1547, Adjusted R-squared:  0.1484 
    ## F-statistic: 24.53 on 1 and 134 DF,  p-value: 2.164e-06

``` r
par(mar = c(4, 4, 0.1, 0.1))
plot(model_updated, which = c(2, 2))
plot(model_updated, which = c(4, 4))
```

<img src="ObesityModel_files/figure-gfm/unnamed-chunk-9-1.png" width="50%" /><img src="ObesityModel_files/figure-gfm/unnamed-chunk-9-2.png" width="50%" />
Taking a look at the updated model for obesity rates over the years,
there’s a substantial increase in the value of intercept coefficient and
slope remains relatively the same. Residuals appear to be in good shape
with slight deviation from the Normal line and do not violate the
independence assumption.
