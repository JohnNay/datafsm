# Precompiled vignettes that take a long time to compile
# Must manually move image files from datafsm/ to datafsm/vignettes/ after knit

library(knitr)
knit("vignettes/FRD_vignette.Rmd.orig", "vignettes/FRD_vignette.Rmd")
