library(devtools)

devtools::install_github("cefet-rj-dal/dal", force=TRUE)
library(dal)

devtools::install_github("cefet-rj-dal/dal", force=TRUE, dep = FALSE, build_vignettes = TRUE)
library(dal)
utils::browseVignettes()
