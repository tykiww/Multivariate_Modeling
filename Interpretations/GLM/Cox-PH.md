---
layout: post
title: "Cox PH Customer Churng"
fb-img: 
comments: true
---

How long is it going to take!? 

##*Part 1*

![](https://images.baklol.com/Waiting-for-Wife-in-a-Cloth-Store-0380668311499600589.jpg)

Duration models are an interesting bunch. Mostly used for econometrics, survival analysis, and mechanical failure. However, outside of these realms, it is not so commonly heard of. Today, we will go through a basic understanding of what survival analysis is and how it can be used to predict customer churn.

### Survival Analysis

The name speaks for itself. How long does an HIV patient have until they hit clinical latency? What is the expected mechanical failure rate of a specific piece of HVAC equipment? Survival anaysis is a principal method to measure time to death or a specific occurance of an event. These methods are especially useful for studying longditudinal censored data. Longditudinal, meaning time-weighted long-term data; censored, meaning determined whether they are at any specific stage of interest (e.g. "A patient who does not experience the event of interest (i.e. sick, dead,...) for the duration of the study is said to be right censored" [source](https://www.cscu.cornell.edu/news/statnews/stnews78.pdf)). Once you have identified the condition of an individual, you can use it to your advantage to introduce a correct amount of bias (information) into your model. Using the bias, we are better able to determine the time to a certain event. If you want to learn even more, the previous mentioned source will go deeper in to the types of parametric, non-parametric, and partial-parametric methods that are commonly used.

### Cox Hazards Regression (CPH)

Recently (10/02/2019), I was reading the book [Legislating in the Dark: Information and Power in the House of Representatives](https://www.amazon.com/Legislating-Dark-Information-Representatives-American/dp/022628171X) and came accross one of his analyses using CPH. I was familiar with the method and had used it before, but always applied towards clinical trial data.The author applied the method to study the variables that decreased congressional bill layover times. In other words, what effects could be determined to make a bill pass faster? I was intrigued by the application and suddenly wondered how this could be applied to consumer analysis.

In summary, the Cox Proportional Hazards Regression is a partial-parametric technique used to measure an event occurance (survival time) on several covariates (factors, kinda). Semi-Parametric meaning some parts of the model use distributional assumptions, but not all. In some cases, if they are unknown they can be taken into account afterwards.

#### Key Assumptions

There are several assumptions to make, but not as bad as a regular OLS.

- Hazards are Proportional

There are two ways to look at it. Hazards are proportional, or a non-significant relationship between residuals and time. 

Hazards are talking about the cumulative hazard function (almost like a cumulative distribution function but NOT a probability). The hazard function is a measure of risk similar to the survival function (1 - probability distribution of success). If the risk of hazard is proportional to time, this assumption is met. Another simple way of saying this is varying levels of factors have a generally proportional relationship of decay with time. Factor levels should not screw with the magnitude and direction of decay. Just the rate of decay. This is a very very very important concept that cannot be broken. If it is, your power to detect any difference may be nonexistant. There are methods to fix this, but we won't cover that here. Details will be posted at the bottom. This can be shown using Kaplan Meier curves, which we will go over later.

On the other hand, we can also do another test to plot the residuals over time. If there seems to be no significant relationship, we are also good to go!

- Covariates are Linear (additive) but multiplicative on hazard

This just means that each effect is additive to the overall dependent variable. Just like a regular regression. The way to interpret the coefficient (exponentiated) is 
Steps taken to diagnose these are the same as an OLS. To figure it out, just plot the residuals just as you would a regular regression. 

- Independence

Independence seems to be standard with most models. Independent data makes for easier computation. This is extremely important, so don't go around applying the CPH regression until after you check for it.

### Analysis

For this entry, we will be studying the customer lifecycle of a telecomunications company using a duration model. I am certain this model has been applied in this setting but the only examples of the Cox regression really shows up with clinical survival. I also understand that the gold standard for customer church is usually the pareto-nbd. However, what's the fun in taking other people's work?? Let's give something new a try.

**DISCLAIMER: Model assumptions will NOT hold for this dataset. Scroll to the bottom and I will discuss how to take care of these. Or just follow along and you will find out as we move on.**

Data comes from [Treselle Systems](https://github.com/treselle-systems) on customer management. Their analysis uses a logistic regression. Not the most ideal because it does not take into account censorship. Those that have not YET dropped out are considered successes. The time parameter is cut too short. We need a model that understands that the lifecycle of certain customers have not yet finished.

```r
library(tidyverse, quietly = TRUE)
url <- "https://raw.githubusercontent.com/treselle-systems/customer_churn_analysis/master/WA_Fn-UseC_-Telco-Customer-Churn.csv"
data <- read_csv(url)
skimr::skim(data)
```

    ## Skim summary statistics
    ##  n obs: 7043 
    ##  n variables: 21 
    ## 
    ## -- Variable type:character -----------------------------------------------------
    ##          variable missing complete    n min max empty n_unique
    ##             Churn       0     7043 7043   2   3     0        2
    ##          Contract       0     7043 7043   8  14     0        3
    ##        customerID       0     7043 7043  10  10     0     7043
    ##        Dependents       0     7043 7043   2   3     0        2
    ##  DeviceProtection       0     7043 7043   2  19     0        3
    ##            gender       0     7043 7043   4   6     0        2
    ##   InternetService       0     7043 7043   2  11     0        3
    ##     MultipleLines       0     7043 7043   2  16     0        3
    ##      OnlineBackup       0     7043 7043   2  19     0        3
    ##    OnlineSecurity       0     7043 7043   2  19     0        3
    ##  PaperlessBilling       0     7043 7043   2   3     0        2
    ##           Partner       0     7043 7043   2   3     0        2
    ##     PaymentMethod       0     7043 7043  12  25     0        4
    ##      PhoneService       0     7043 7043   2   3     0        2
    ##   StreamingMovies       0     7043 7043   2  19     0        3
    ##       StreamingTV       0     7043 7043   2  19     0        3
    ##       TechSupport       0     7043 7043   2  19     0        3
    ## 
    ## -- Variable type:numeric -------------------------------------------------------
    ##        variable missing complete    n    mean      sd    p0    p25     p50     p75    p100     hist
    ##  MonthlyCharges       0     7043 7043   64.76   30.09 18.25  35.5    70.35   89.85  118.75 ▇▁▃▂▆▅▅▂
    ##   SeniorCitizen       0     7043 7043    0.16    0.37  0      0       0       0       1    ▇▁▁▁▁▁▁▂
    ##          tenure       0     7043 7043   32.37   24.56  0      9      29      55      72    ▇▃▃▂▂▃▃▅
    ##    TotalCharges      11     7032 7043 2283.3  2266.77 18.8  401.45 1397.47 3794.74 8684.8  ▇▃▂▂▁▁▁▁

Some of the yes/no binary data can be cleansed first. Then we can remove some NA data. I'm not dying to keep it. Don't really feel like doing more.

```r
# binary removal [-2] applies to already separated 0-1 data
condition <- data[,apply(data,2,function(x) length(unique(x)))==2]
later <- colnames(data)[!colnames(data) %in% (condition %>% names)]
binary <- ifelse(condition=="Yes"| condition=="Female" | condition == 1,1,0) 
new_data <- cbind(select(data,later),binary) %>% as_tibble
# NA removal
new_data <- na.omit(new_data)
# Customer ID Removal
new_data <- select(new_data,-customerID)
```

We will now move on to applying the model

#### Exploring Proportional Hazards.

Here, Kaplan Meier curves can be applied using the tenure, churn, and an interest column. Realize here that the cutoff for churn is already identified. In a realistic scenario, you'll have to identify that threshold yourself.


Let's take for example the `StreamingTV` column. This covariate can be evaluated by passing it through the `survfit()` function and plotted using `ggsurvplot`. The output shows the probability of survival given time with a p-value indicating a significant difference between factor levels. If any of your are shaking your head and wondering why this seems so similar to a chi-square test, it is. Don't worry, it's just generalized.

```r
library(survival)
library(survminer)
surv_object <- Surv(time = new_data$tenure , event = new_data$Churn)
sf <- survfit(surv_object ~ StreamingTV,data = new_data)
ggsurvplot(sf,data = new_data, pval = TRUE)
```

We see that the streaming tv and non-streaming tv factor levels seem proportional. It is good for use in our regression. However, as time increases, the probability of losing customers out of no internet service doesn't change at all. This is definitely useful information for management, but not so much for our model. We will be removing the factor level No interent service for this variable (you'll notice that pattern is consistent with the rest of the covariates).

Another example is the `gender` column. Here you see a practically an overlapping set of curves that show an equal decrease in probability over time (p = 0.47).

```r
surv_object <- Surv(time = new_data$tenure , event = new_data$Churn)
sf <- survfit(surv_object ~ gender,data = new_data)
ggsurvplot(sf,data = new_data, pval = TRUE)
```

Usually, if the curves overlap, it is a bad sign. This means that there is an interaction with time. However, the p-value seems insignificant rightso , they are probably just overlapping each other because they are practically the same. We will keep this in our analysis.

The last one we will look at is the `PaperlessBilling` column.

```r
surv_object <- Surv(time = new_data$tenure , event = new_data$Churn)
sf <- survfit(surv_object ~ PaperlessBilling,data = new_data)
ggsurvplot(sf,data = new_data, pval = TRUE)
```

Here you see a significant difference and curves pointed outwards from each other. Those that do use paperless billing are more likely to drop out over time compared to those that receive paper bills. Unfortunately though, we will have to remove this column completely. Breaks assumptions..

So far, things aren't looking too good in terms of proportionality. There are some major fixes we have to make.

So, as you see, there is a tedious aspect of going through every column and comparing tests. However, there is a faster way to do this. So from here, I will be cleaning the data in accordance with some findings made outside of the shown analysis. The code will be documented below to make sure you have the same analysis moving forward.

```r
# Removing all individuals that do not have internet/phone service (most likely will remove some columns)
update <- new_data[rowSums(new_data == "No internet service" |new_data ==  "No phone service")==0,]
later <- colnames(update)[!colnames(update) %in% colnames(update[c(4:9,18)])]
changed <- ifelse(update[4:9]=="Yes",1,0)
updated_data <- cbind(select(update,later),changed) %>% as_tibble
```

#### Exploring Proportional Hazards.

Let's fit this model using some fixed effects. Of course, that is assuming that an individual using an electronic check will always use an electronic check, but as long as we are constrained to this study we should probably be okay.

```r
# fixed effects
library(fastDummies)
results <- fastDummies::dummy_cols(updated_data,remove_first_dummy = TRUE)[-c(2:5)]

# Creating a formula and removing tenure and churn
form <- as.formula(paste("Surv(tenure,Churn) ~ `",paste(colnames(results)[-c(1,9)], collapse='` + `'),'`', sep = ""))

# Model saved
res.cox <- coxph(form, data =  results)

# Extracting coefficients
summary_cox <- summary(res.cox)$coefficients

# This prints the coefficients that are statistically NON significant (p-val for HR=1)
rownames(summary_cox)[summary_cox[,5]> 0.05 ]

# You can extract the GLOBAL p-value to see if the whole model itself is useful.
test.ph <- cox.zph(res.cox)


summary_cox
test.ph$table[nrow(test.ph$table),] # For global p-value
# After removing variables and running this twice, just gender is only non-significant. Not a very systematic approach..
```

    ##                                                    coef exp(coef)     se(coef)            z     Pr(>|z|)
    ## MonthlyCharges                             0.0308926501 1.0313748 2.390050e-02   1.29255269 1.961658e-01
    ## TotalCharges                              -0.0026090761 0.9973943 6.156906e-05 -42.37641327 0.000000e+00
    ## gender                                    -0.0005601731 0.9994400 5.070122e-02  -0.01104851 9.911847e-01
    ## SeniorCitizen                              0.0219086069 1.0221504 6.016222e-02   0.36415890 7.157394e-01
    ## Partner                                   -0.1387318996 0.8704614 5.975493e-02  -2.32168131 2.025010e-02
    ## Dependents                                -0.0635806632 0.9383984 7.625004e-02  -0.83384428 4.043687e-01
    ## PaperlessBilling                           0.2419451026 1.2737243 6.357728e-02   3.80552774 1.415022e-04
    ## OnlineSecurity                            -0.2853001304 0.7517886 1.401955e-01  -2.03501698 4.184916e-02
    ## OnlineBackup                              -0.0442105506 0.9567525 1.329484e-01  -0.33253921 7.394821e-01
    ## DeviceProtection                           0.0896837005 1.0938283 1.331643e-01   0.67348150 5.006410e-01
    ## TechSupport                               -0.0538225235 0.9476003 1.384679e-01  -0.38870030 6.974979e-01
    ## StreamingTV                                0.3283748342 1.3887094 2.466224e-01   1.33148827 1.830284e-01
    ## StreamingMovies                            0.3196928238 1.3767048 2.444960e-01   1.30755859 1.910231e-01
    ## MultipleLines_Yes                          0.1145737730 1.1213954 1.306648e-01   0.87685243 3.805668e-01
    ## `InternetService_Fiber optic`              1.6090037678 4.9978297 6.023297e-01   2.67130081 7.555790e-03
    ## `Contract_Month-to-month`                  0.8594848795 2.3619437 1.092248e-01   7.86895294 3.576217e-15
    ## `Contract_Two year`                       -1.1802141612 0.3072129 2.020265e-01  -5.84187730 5.161582e-09
    ## `PaymentMethod_Electronic check`           0.0490063977 1.0502271 8.147007e-02   0.60152637 5.474895e-01
    ## `PaymentMethod_Credit card (automatic)`   -0.3148097377 0.7299277 1.015159e-01  -3.10108764 1.928112e-03
    ## `PaymentMethod_Bank transfer (automatic)` -0.3141833879 0.7303851 9.993798e-02  -3.14378353 1.667788e-03

    ## GLOBAL                                                            NA            1641.432    0.00e+00     # the model itself is not good for 
                                                                                                                # interpretation. Bad fit.

<hr>

Well... That sucks? It looks like many of the covariates are statistically significant. This is bad news! The null hypothesis for this test is that the hazard ratio is 1 (the `exp(coef)` column). If the ratio is 1, we know that proportionality is met. However, most of them are statistically significant. If you happen to parse the model even further by non-significant items, you'll end up with just gender being the only covariate and nothing interesting learned. Furthermore, the `GLOBAL` at the bottom shows a significant p-value. This means the model itself is just not a great fit.

If we were wanting to interpret the model, we would say something like this: 

For the `gender` column (Female 1, Male 0), men are the baseline. So the expected hazard is `exp(-0.0005) = 0.999` times lower in men as compared to women, holding all else constant. In other words, men are less likely to cancel by .001 times less than women, a negligable amount.

For the `MonthlyCharges` column, a one unit increase in monthly charge is expected to increase the hazard by 1.03 times. Increasing the monthly charge a unit, holding all else constant, will likely cause an individual to cancel by 1.03 times. If we were to multiply 1.03 by the typical amount, we see this here. 

Now what do we do? If the model doesn't fit very well...

1) You can make every non-significant variable a time-interacted variable `covariate*tenure` in the regression model and try again
2) You can create 'valid' time-intervals for each covariate and specify proportionality for only those intervals

But do we really want to go through all that trouble??

NO WAY!

There is a better third method. Luckily, I have written a [part II](https://tykiww.github.io/tykiww.github.io/2019-11-05-Duration-2/) to address this issue.



*If you are worried more about how to interpret this model regardless of it not working, click [here](http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Survival/BS704_Survival6.html) for some help.*


#### Conclusion

This model isn't completely garbage. If the individual coefficient is significantly different from zero, I'll keep the variable in the model. This shows that the hazard increases or decreases for every unit (continuous) of your variable. Maybe it won't improve the model fit much, but it definitely tells us something regarding the behavior of the risk factors. Prediction isn't the concern here. It is interpretation.

Here, we were able to see the Kaplan Meier curves to propose possible interactions of factor levels on time. This can be important information for management. We also have coefficients that roughly explain to us causes for decline (even though the model didn't fit so well). 

Let's move on to a [better](https://tykiww.github.io/tykiww.github.io/2019-11-05-Duration-2/) solution for this problem.


*TL;DR. Cox Proportional Hazards, without valid assumptions is powerless. Time to move on to a more informed survival model!


