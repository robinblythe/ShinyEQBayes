# run.R

if (!requireNamespace("renv")) {
  install.packages("renv")
}
renv::autoload()
shiny::runApp(utils::getSrcDirectory(function(){}))
