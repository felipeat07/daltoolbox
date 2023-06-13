if (!exists("repos_name"))
  repos_name <<- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <<- repos
}

loadlibrary_internal <- function(packagename)
{
  if (!require(packagename, character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename, character.only = TRUE)
  }
}

loadlibrary <- function(packagename)
{
  suppressWarnings(loadlibrary_internal(packagename))
}


load_daltoolbox_internal <- function() {
  if (!require("daltoolbox", character.only = TRUE))
  {
    library(devtools)

    devtools::install_github("cefet-rj-dal/daltoolbox", force=TRUE, dep=FALSE, upgrade="never")

    library(daltoolbox)
  }
}


load_daltoolbox <- function()
{
  suppressWarnings(load_daltoolbox_internal())
}
