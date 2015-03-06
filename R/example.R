# rm(list=ls())
# load("~/Documents/Rprojects/ga_fsm/genetic_abm/final.Rda")
# index <- rep(TRUE, nrow(final))
# for (i in 1:(nrow(final)-1)){
#         ifelse(final$period[i]==1 & final$period[i+1]==1, index[i] <- FALSE, index[i] <- TRUE)
# }
# final <- final[index, ] # drops all games that are only 1 period long
# ipd_data <- data.frame(outcome = as.integer(ifelse(final[, "my.decision"]=="coop", 1, 2)),
#                    period = as.integer(final[ , "period"]),
#                    my.decision1 = as.integer(ifelse(final[, "my.decision1"]==1, 1, 0)),
#                    other.decision1 = as.integer(ifelse(final[, "other.decision1"]==1, 1, 0)))
# devtools::use_data(ipd_data)
#
# # 80% of the data for training
# percent_train <- 0.80
# ipd_samples <- nrow(data)
# ntrain <- as.integer(percent_train * ipd_samples)
# # Back up to just before a new game starts.
# ntrain <- ntrain - data[ntrain,"period"]
# stopifnot(data[ntrain + 1,1] == 1)
# # train_indices <- sample(ipd_samples, ntrain, FALSE )
# train_indices <- 1:ntrain
# test_indices <- 1:ipd_samples
# test_indices <- test_indices[! test_indices %in% train_indices]
# train_data <- data[train_indices, ]
# # 20% of the data for testing
# test_data <- data[test_indices,]
# # Make sure that test data starts with the first round of a new game.
# stopifnot(test_data[1,1] == 1)
#
# evolved_model <- evolve_model(data, parallel=T)
# summary(evolved_model)
#
# library(fsm)
# data(ipd_data)
# summary(evolve_model(ipd_data))
