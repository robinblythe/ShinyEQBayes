# run.R

if (!requireNamespace("renv")) {
  install.packages("renv")
}
renv::load(utils::getSrcDirectory(function(){}))
shiny::runApp(utils::getSrcDirectory(function(){}))
