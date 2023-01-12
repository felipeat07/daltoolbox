## global reference to scipy (will be initialized in .onLoad)
scipy <- NULL

#'@import reticulate
.onLoad <- function(libname, pkgname) {
  ## use superassignment to update global reference to scipy
#  scipy <<- reticulate::import("scipy", delay_load = TRUE)
  reticulate::source_python('./inst/python/ts_tlstm.py', envir=parent.frame())

}


## Pacotes instalados no Python (instalação feita via prompt),até conseguir dar source no arquivo:
# python3 -m pip install --upgrade pip setuptools wheel
# apt install python3.10-venv
# pip3 install --upgrade pip
# pip3 install -U torch
# pip3 install -U pyreadr
# pip3 install -U matplotlib

## Depois disso é possível rodar o exemplo:
# https://nbviewer.org/github/eogasawara/mydal/blob/main/examples_timeseries/ts_tlstm.ipynb

#'@import reticulate
#'@export
load_python_file <- function() {
#  path <- paste(system.file(package="dal"), "ts_tlstm.py", sep="/")
#  reticulate::py_run_file(system.file("python", "ts_tlstm.py", package = "dal"))
#  source_python('~/Downloads/dal/inst/python/ts_tlstm.py')
#  reticulate::source_python('./inst/python/ts_tlstm.py')
  reticulate::source_python('./inst/python/ts_tlstm.py', envir=parent.frame())
}

#'@export
install_python_dependencies <- function(method = "auto", conda = "auto") {
  reticulate::py_install("torch", method = method, conda = conda)
  reticulate::py_install("pandas", method = method, conda = conda)
  reticulate::py_install("numpy", method = method, conda = conda)
  reticulate::py_install("matplotlib", method = method, conda = conda)
  reticulate::py_install("os", method = method, conda = conda)
  reticulate::py_install("sys", method = method, conda = conda)
  reticulate::py_install("random", method = method, conda = conda)
}
