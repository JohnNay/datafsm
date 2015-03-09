<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build Status](https://travis-ci.org/JohnNay/fsm.png?branch=master)](https://travis-ci.org/JohnNay/fsm)

This R package -- by John Nay and Jonathan Gilligan -- implements our method for automatically generating models of dynamic decision-making that both have strong predictive power and are interpretable in human terms. This is useful for designing empirically grounded agent-based simulations and for gaining direct insight into observed dynamic processes. We use an efficient model representation and a genetic algorithm-based estimation process to generate simple deterministic approximations that explain most of the structure of complex stochastic processes. This method, implemented in C++ and R, scales well to large data sets. We have applied the package to empirical data, and demonstrated the method's ability to recover known data-generating processes by simulating data with agent-based models and correctly deriving the underlying decision models for multiple agent models and degrees of stochasticity. These applications of the package are described in a 2015 working paper by John Nay and Jonathan Gilligan: \`\`Data-driven Dynamic Decision Models''.

This work is supported by NSF grants EAR-1416964 and EAR-1204685.

Install and load the latest release of the package from GitHub:

``` {.r}
install.packages("devtools")
devtools::install_github("JohnNay/fsm")
library(fsm)
```

Check out the documentation for the main function of the package:

``` {.r}
?evolve_model
```

Create data and estimate model with that data:

``` {.r}
cdata <- data.frame(period = 1:5, outcome = c(1,2,1,1,1),
my.decision1 = c(1,0,1,1,1), other.decision1 = c(0,0,0,1,1))
(result <- evolve_model(cdata))
```

Use data from this package to estimate a model:

``` {.r}
data(ipd_data)
percent_train <- 0.80
ipd_samples <- nrow(ipd_data)
ntrain <- as.integer(percent_train * ipd_samples)
# Back up to just before a new interaction starts:
ntrain <- ntrain - ipd_data[ntrain,"period"]
train_indices <- 1:ntrain
test_indices <- 1:ipd_samples
test_indices <- test_indices[! test_indices %in% train_indices]
train_data <- ipd_data[train_indices, ]
test_data <- ipd_data[test_indices,]
```

Use the summary method on the output of the evolve\_model function:

``` {.r}
summary(evolve_model(train_data, test_data, run = 5))
```
