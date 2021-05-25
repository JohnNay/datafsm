# Precompiled vignettes that take a long time to compile
# Must manually move image files from datafsm/ to datafsm/vignettes/ after knit

library(knitr)
prebuild_vignettes <- function() {
  old_wd <- getwd()
  if (! basename(getwd()) == "vignettes" && dir.exists("./vignettes")) {
    setwd("vignettes")
    on.exit(setwd(old_wd))
  }
  knit("FRD_vignette.Rmd.orig", "FRD_vignette.Rmd")
}

prebuild_vignettes()
