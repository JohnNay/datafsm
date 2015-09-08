## ----load.data.fsm-------------------------------------------------------
# Load and attach datafsm into your R session, making its functions available:
library(datafsm)

## ----load.printr, include=FALSE------------------------------------------
#library(printr)

## ----cite.datafsm, eval=TRUE, include=TRUE-------------------------------
citation("datafsm")

## ----fake.data, eval=TRUE, include=TRUE----------------------------------
seed <- 1900
set.seed(seed)
cdata <- data.frame(outcome = NA,
                    period = rep(1:10, 2000),
                    my.decision1 = NA,
                    other.decision1 = NA)
#
# Prisoner's dilemma
#
pd_outcome <- function(player_1, player_2) {
  #
  # 1 = C
  # 2 = D
  #
  player_1  + 1
}

tit_for_tat <- function(last_round_self, last_round_opponent) {
    last_round_opponent
}

noisy_tit_for_tat <- function(last_round_self, last_round_opponent) {
  if (runif(1,0,1) <= 0.1) {
    sample(0:1,1)
  } else {
    last_round_opponent
  }
}

for (i in seq_along(cdata$period)) {
  if (cdata$period[i] == 1) {
    my.decision <- sample(0:1,1, prob = c(0.9,0.1))
    other.decision <- sample(0:1,1)
    cdata[i, "outcome"] <- pd_outcome(my.decision, other.decision)
  } else{
    my.last <- my.decision
    other.last <- other.decision
    my.decision <- tit_for_tat(my.last, other.last)
    other.decision <- noisy_tit_for_tat(other.last, my.last)
    cdata[i,c("outcome", "my.decision1", "other.decision1")] <- c(pd_outcome(my.decision, other.decision), my.last, other.last)
  }
}

## ----cdata.table, eval=TRUE, echo=FALSE, results='asis'------------------
knitr::kable(head(cdata, 11))

## ----evolve.model, eval=TRUE, include=TRUE, message=FALSE, warning=FALSE, results='hide'----
res <- evolve_model(cdata, seed = seed)

## ----plot.fsm, eval=TRUE, include=TRUE, fig.width=5, fig.height=4, fig.cap="Result of `plot()` method call on `ga_fsm` object."----
summary(res)
plot(res, action_label = ifelse(action_vec(res)==1, "C", "D"), 
     transition_label = c('cc','dc','cd','dd'))

## ----plot.evolution, eval=TRUE, include=TRUE, fig.width=8, fig.height=6, fig.cap="Result of `plot()` method call on ga object, which is obtained by calling `estimation_details()` on `ga_fsm` object."----
suppressMessages(library(GA))
plot(estimation_details(res))

## ----get.help, eval=TRUE, include=TRUE, render.args = list(help = list(sections = list("usage", "arguments", "details", "references")))----
?evolve_model

## ----session.info, eval=TRUE, include=TRUE-------------------------------
sessionInfo()

