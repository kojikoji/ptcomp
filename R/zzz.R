safe_import_from_path <- function(module.name, libname, pckgname){
  tryCatch(
    assign(paste("py", module.name, sep="_"), reticulate::import_from_path(module.name, path = file.path(libname, pckgname, "src")), pos = .GlobalEnv),
    error = function(e){
      packageStartupMessage("Install python dependencies...")
      reticulate::py_install(c("numpy", "numba", "scipy", "progressbar2", "pandas"))
      assign(module.name, reticulate::import_from_path(module.name, path = file.path(libname, pckgname, "src")), pos = .GlobalEnv)
      
    })
}


.onLoad <- function(libname, pckgname){
  packageStartupMessage("Import python module in this repository") 
  reticulate::use_condaenv("r-reticulate")
  reticulate::use_virtualenv("r-reticulate")
  importlib <<- reticulate::import("importlib", delay_load = TRUE)
  py_builtin <<- reticulate::import_builtins()
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  py_builtin <<- reticulate::import_builtins()
  safe_import_from_path("test_ret", libname, pckgname)
}
