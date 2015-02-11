#' @useDynLib fsm
#' @importFrom Rcpp sourceCpp

# # create "fitnessC"
# Rcpp::sourceCpp("/Users/johnnaymacbook/Documents/Rprojects/ga_fsm/genetic_abm/fitness.cpp")
# # load data from fsm package
# data(data); data (outcome)
# # 80% of the data for training
# train_data <- data[1:108306, ]
# train_outcome <- outcome[1:108306]
# # 20% of the data for testing
# test_data <- data[108307:nrow(data), ]
# test_outcome <- outcome[108307:nrow(data)]
#
# evolved_models_empirical_data <- evolve_model(data = train_data, outcome = train_outcome,
#                                               actions = 2, states = 2, seed = 13,
#                                               fitness_func = fitnessC, cols = c(1, 2, 1, 2),
#                                               test_data =  test_data, test_outcome = test_outcome,
#                                               popSize = 175, maxiter = 55, run = 25, parallel = TRUE)
# print(evolved_models_empirical_data)
# show(evolved_models_empirical_data)
# summary(evolved_models_empirical_data)
#
# data <- data.frame(y = evolved_models_empirical_data@varImp, x= c("cc", "dc", "cd", "dd"))
# p <- ggplot(data, aes(x = x, y=y)) + geom_bar(stat="identity") + coord_flip() +
#          ylab("Relative Importance") +
#          xlab("Variables")
# p
#
# plot(evolved_models_empirical_data@GA,
#      cex.points = 1.2, cex.lab = 1.1, cex.axis = 1.1,
#      col = c("green3", "dodgerblue3", adjustcolor("green3", alpha.f = 0.1)),
#      pch = c(16, 1), lty = c(1,2), grid = graphics:::grid)

# source("abm/helper_funcs.R") # for tft.f and grim.f
# tft <- rep(NA, nrow(data)); grim <- rep(NA, nrow(data))
# for (i in seq(nrow(data))){
#         tft[i] <- tft.f(period = data[i, 1], phase = tft.phase, other.prev = data[i, 3])[1]
#         tft.phase <- tft.f(data[i, 1], tft.phase, data[i, 3])[2]
#         grim[i] <- grim.f(data[i, 1], grim.phase, data[i, 3], data[i, 2])[1]
#         grim.phase <- grim.f(data[i, 1], grim.phase, data[i, 3], data[i, 2])[2]
# }
# grim_accuracy <- sum(ifelse( grim == outcome , 1 , 0)) / length(outcome)
# tft_accuracy <- sum(ifelse( tft == outcome , 1 , 0)) / length(outcome)
# model_accuracy <- evolved_models_empirical_data@predictive


# # gaControl("binary") # shows the default setting used, e.g.: gabin_Population
#
# source("compareFSM.R") # gives the complexity() and compare funcs and tft state matrix
# # complexity(evolved.models$decoded[[2]]) # this only works for states=2 bc those give
# # strings of length 10 and the max this can handle is 12.
# ##############################################################################
