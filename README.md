
ROCit 2.1.1
=====

Author
=======

Md Riaz Ahmed Khan `mdriazahmed.khan@jacks.sdstate.edu`
Thomas Brandenburger, PhD `thomas.brandenburger@sdstate.edu`


Maintainer
===============
Md Riaz Ahmed Khan `mdriazahmed.khan@jacks.sdstate.edu`


ROCit
=====

The goal of ROCit is to easily compute, summarize, and visualize the performance of a binary classifier. ROCit package provides flexibility to easily evaluate threshold-bound metrics, such as, accuracy, misclassification rate, sensitivity, specificity, F-Score. . Also, ROC curve, along with AUC, can be obtained using different methods, such as empirical, binormal and non-parametric. ROCit encompasses a wide variety of methods for constructing confidence interval of ROC curve and AUC. ROCit features the option of constructing empirical gains table, which is a handy tool for direct marketing. The package offers options for commonly used visualization, such as, ROC curve, KS plot, lift plot.

Example
=======

This is a basic example which shows the workflow of assessing a binary classifier.

``` r
library(ROCit)
data("Loan")
summary(Loan$Status)
#>  CO  FP 
#> 131 769
```

Make a response
===============

-   Label 1 for charged off, 0 for fully paid

``` r
class <- ifelse(Loan$Status == "FP", 0, 1)
```

Use score as a predictor of defaulting
======================================

``` r
score <- Loan$Score
```

Calculate threshold-bound metrics
=================================

``` r
m<- measureit(score = score, class = class,
                     measure = c("ACC", "SENS", "SPEC", "FSCR"))

mymetrics <- as.data.frame(cbind(Cutoff = m$Cutoff, Depth = m$Depth,
                                 Accuracy = m$ACC, Sensitivity = m$SENS,
                                 Specificity = m$SPEC, `F-Score` = m$FSCR))

head(mymetrics)
#>     Cutoff       Depth  Accuracy Sensitivity Specificity    F-Score
#> 1      Inf 0.000000000 0.8544444 0.000000000   1.0000000        NaN
#> 2 269.1711 0.001111111 0.8555556 0.007633588   1.0000000 0.01515152
#> 3 267.3342 0.002222222 0.8544444 0.007633588   0.9986996 0.01503759
#> 4 266.5938 0.003333333 0.8533333 0.007633588   0.9973992 0.01492537
#> 5 265.2553 0.004444444 0.8522222 0.007633588   0.9960988 0.01481481
#> 6 263.8303 0.005555556 0.8533333 0.015267176   0.9960988 0.02941176
tail(mymetrics)
#>         Cutoff     Depth  Accuracy Sensitivity Specificity   F-Score
#> 897 103.236891 0.9955556 0.1500000           1  0.00520156 0.2551120
#> 898  93.941202 0.9966667 0.1488889           1  0.00390117 0.2548638
#> 899  70.836943 0.9977778 0.1477778           1  0.00260078 0.2546161
#> 900  -2.673036 0.9988889 0.1466667           1  0.00130039 0.2543689
#> 901  -5.449033 1.0000000 0.1455556           1  0.00000000 0.2541222
#> 902       -Inf 1.0000000 0.1455556           1  0.00000000 0.2541222
```

ROC curve
=========

``` r
rocit_object_empirical <- rocit(score = score, class = class)
summary(rocit_object_empirical)
#>                                       
#>  Empirical ROC curve                  
#>  Number of postive responses :  131   
#>  Number of negative responses :  769  
#>  Area under curve :  0.666315925311945
```

Plot ROC curve
==============

``` r
# plot(rocit_object_empirical, YIndex = F, values = F)
```

Confidence interval of AUC
==========================

``` r
ci.auc <- ciAUC(rocit_object_empirical, level = 0.95)
print(ci.auc)
#>                                                           
#>    estimated AUC : 0.666315925311945                      
#>    AUC estimation method : empirical                      
#>                                                           
#>    CI of AUC                                              
#>    confidence level = 95%                                 
#>    lower = 0.612575921206968     upper = 0.720055929416921
```

Confidence interval of ROC curve
================================

``` r
ci.roc <- ciROC(rocit_object_empirical, level = 0.9)
# plot(ci.roc)
```

Gains table
===========

``` r
gainstable <- gainstable(rocit_object_empirical, ngroup = 8)
print(gainstable, maxdigit = 2)
#>   Bucket Obs CObs Depth Resp CResp RespRate CRespRate CCapRate Lift CLift
#> 1      1 112  112  0.12   30    30     0.27      0.27     0.23 1.84  1.84
#> 2      2 113  225  0.25   25    55     0.22      0.24     0.42 1.52  1.68
#> 3      3 113  338  0.38   21    76     0.19      0.22     0.58 1.28  1.54
#> 4      4 112  450  0.50   18    94     0.16      0.21     0.72 1.10  1.44
#> 5      5 112  562  0.62   10   104     0.09      0.19     0.79 0.61  1.27
#> 6      6 113  675  0.75   16   120     0.14      0.18     0.92 0.97  1.22
#> 7      7 113  788  0.88    5   125     0.04      0.16     0.95 0.30  1.09
#> 8      8 112  900  1.00    6   131     0.05      0.15     1.00 0.37  1.00
# plot(gainstable)
```
