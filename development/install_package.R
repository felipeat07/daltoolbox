library(devtools)

devtools::install_github("cefet-rj-dal/dal", force=TRUE)
library(dal)

devtools::install_github("cefet-rj-dal/dal", force=TRUE, dependencies = TRUE, build_vignettes = TRUE)
library(dal)
utils::browseVignettes()
