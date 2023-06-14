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
  suppressPackageStartupMessages(loadlibrary_internal(packagename))
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
  suppressPackageStartupMessages(load_daltoolbox_internal())
}


if (FALSE) {
  library(devtools)
  load_all()
}

if (FALSE) {
  library(devtools)
  check()
  load_all()
}

if (FALSE) {
  library(devtools)
  suppressWarnings(check(vignettes = FALSE))
  load_all()
}

if (FALSE) {
  library(devtools)
  document()
  load_all()
}

if (FALSE) {
  library(devtools)
  devtools::build_manual()
}

if (FALSE) {
  library(devtools)
  usethis::use_readme_rmd()
}

if (FALSE) {
  library(devtools)
  devtools::build_readme()
}

if (FALSE) {
  pkgdown::build_site()  ## para ver o resultado, o site fica muito legal
  #usethis::use_pkgdown_github_pages()
}

if (FALSE) {
  devtools::install(dependencies = TRUE, build_vignettes = TRUE)
  utils::browseVignettes()
}

