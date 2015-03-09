library(fsm)
context("Main evolve_model function")

test_that("evolve_model() returns correct objects", {
        cdata <- data.frame(period = 1:5, outcome = c(1,2,1,1,1),
                            my.decision1 = c(1,0,1,1,1), other.decision1 = c(0,0,0,1,1))
        result <- evolve_model(cdata)
        expect_is(result, "ga_fsm")
})

