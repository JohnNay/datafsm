## ------------------------------------------------------------------------
# Load and attach datafsm into your R session, making its functions available:
library(datafsm)

## ---- include=FALSE------------------------------------------------------
#library(printr)

## ---- eval=TRUE, include=TRUE--------------------------------------------
citation("datafsm")

## ---- eval=TRUE, include=TRUE--------------------------------------------
cdata <- data.frame(period = rep(1:10, 1000),
                    outcome = rep(1:2, 5000),
                    my.decision1 = sample(1:0, 10000, TRUE),
                    other.decision1 = sample(1:0, 10000, TRUE))

## ---- eval=TRUE, echo=FALSE, results='asis'------------------------------
knitr::kable(head(cdata, 11))

## ---- eval=TRUE, include=TRUE, message=FALSE, warning=FALSE, results='hide'----
res <- evolve_model(cdata)

## ---- eval=TRUE, include=TRUE, fig.width=5, fig.height=5, fig.cap="Result of `plot()` method call on `ga_fsm` object."----
summary(res)
plot(res, action_label = c("C", "D"))

## ---- eval=TRUE, include=TRUE, fig.width=8, fig.height=6, fig.cap="Result of `plot()` method call on ga object, which is obtained by calling `estimation_details()` on `ga_fsm` object."----
suppressMessages(library(GA))
plot(estimation_details(res))

## ---- eval=TRUE, include=TRUE, render.args = list(help = list(sections = list("usage", "arguments", "details", "references")))----
?evolve_model

## ---- eval=TRUE, include=TRUE--------------------------------------------
sessionInfo()

