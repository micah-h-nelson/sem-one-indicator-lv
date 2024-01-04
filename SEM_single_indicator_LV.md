SEM with a Single Indicator Latent Variable
================
Micah Nelson
2024-1-4

One advantage of structural equation modeling is the ability to
incorporate latent variables. Doing so helps to mitigate the effects of
measurement error in the model.

Generally, you need multiple indicators that tap into the same latent
variable to estimate a latent variable. However, if there is only a
single indicator of a latent variable, you can control for measurement
error if you have an estimate of the reliability of that measure.

This code shows how to do this using 2021 GSS data to assess whether
one’s perceived occupational stability (how easy one thinks it would be
to find a different job with similar pay and benefits) mediates the
relationship between having a 4-year college degree and self-rated
health.

Load in necessary packages and data

``` r
library(lavaan)
library(haven)
library(tidyverse)

data <- read_dta("gss2021.dta")
```

Clean the variables that will go into the model

``` r
data <- data |> 
  mutate(
    occ.worry = case_match(jobfind, 3 ~ 1,
                           2 ~ 2,
                           1 ~ 3),
    srh = case_match(health, 4 ~ 1,
                     3 ~ 2,
                     2 ~ 3,
                     1 ~ 4),
    college = ifelse(degree == 3 | degree == 4, 1, 0),
    male = ifelse(sex == 1, 1, 0),
    white = ifelse(race == 1 & hispanic == 1, 1, 0),
    black = ifelse(race == 2 & hispanic == 1, 1, 0),
    other.race = ifelse(race == 3 & hispanic == 1, 1, 0),
    hisp = ifelse(between(hispanic,2,50), 1, 0)) |> 
  select(occ.worry, srh, college, male, white, black, other.race, hisp, age) |> 
  drop_na()
```

Specify model structure

Formula for fixing residual variance of indicator is (1 -
reliability)\*sample variance. A paper by Bollen and Gutin
(<https://doi.org/10.1215/00703370-9368980>) indicates the reliability
of self-rated health is about 0.5 and the variance of self-rated health
in this sample is 0.444. That means we fix the residual variance of
self-rated health to 0.222 in the model.

``` r
model <- '
#Define latent health variable using self-rated health
health =~ srh

#Fix the residual variance of self-rated health
srh ~~ 0.222*srh

#Regression models to estimate path from college to occupational stability to health
health ~ b*occ.worry + age + c*college + black + other.race + hisp
occ.worry ~ a*college 

#Indirect effect
indirect := a*b

#Total effect
total := c + (a*b)
'
```

Fit the model and display results. Note that occ.worry is an ordinal
endogenous variable and needs to declared as ordered

``` r
fit <- sem(model, data,
           ordered = "occ.worry")

summary(fit, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 24 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        11
    ## 
    ##   Number of observations                          1327
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                26.018      25.648
    ##   Degrees of freedom                                 4           4
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.017
    ##   Shift parameter                                            0.067
    ##     simple second-order correction                                
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                                13.814      13.814
    ##   Degrees of freedom                                 1           1
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.000       0.000
    ##   Tucker-Lewis Index (TLI)                       0.570       0.578
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.064       0.064
    ##   90 Percent confidence interval - lower         0.042       0.042
    ##   90 Percent confidence interval - upper         0.089       0.089
    ##   P-value H_0: RMSEA <= 0.050                    0.133       0.142
    ##   P-value H_0: RMSEA >= 0.080                    0.158       0.149
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Delta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   health =~                                           
    ##     srh               1.000                           
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   health ~                                            
    ##     occ.worry  (b)    0.073    0.020    3.717    0.000
    ##     age               0.003    0.001    1.932    0.053
    ##     college    (c)    0.248    0.037    6.678    0.000
    ##     black            -0.097    0.066   -1.478    0.139
    ##     other.race        0.054    0.082    0.662    0.508
    ##     hisp              0.020    0.053    0.381    0.703
    ##   occ.worry ~                                         
    ##     college    (a)    0.108    0.064    1.700    0.089
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .srh               2.782    0.067   41.684    0.000
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     occ.worry|t1     -0.669    0.122   -5.464    0.000
    ##     occ.worry|t2      0.586    0.124    4.730    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .srh               0.222                           
    ##    .occ.worry         1.000                           
    ##    .health            0.198    0.015   13.069    0.000
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     indirect          0.008    0.005    1.544    0.123
    ##     total             0.255    0.037    6.865    0.000
