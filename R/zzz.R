safe_import_from_path <- function(module.name, libname, pckgname){
  tryCatch(
    assign(paste("py", module.name, sep="_"), reticulate::import_from_path(module.name, path = file.path(libname, pckgname, "src")), pos = .GlobalEnv),
    error = function(e){
      packageStartupMessage("Install python dependencies...")
      reticulate::py_install(c("numpy", "GPy"))      
    })
}


.onLoad <- function(libname, pckgname){
  packageStartupMessage("Import python module in this repository") 
  # reticulate::use_condaenv("r-reticulate")
  # reticulate::use_virtualenv("r-reticulate")
  reticulate::py_install(c("numpy", "GPy", "matplotlib"))
  importlib <<- reticulate::import("importlib", delay_load = TRUE)
  py_builtin <<- reticulate::import_builtins()
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  GPy <<- reticulate::import("GPy", delay_load = TRUE)
  py_builtin <<- reticulate::import_builtins()
}
