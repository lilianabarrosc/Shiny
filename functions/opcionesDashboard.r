
#titulo de la pagina
head <- function() {
  dashboardHeader(title = ":)")
}

#opciones del sidebar
sidebar <- function() {
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data"),
    menuItem("Analysis", tabName = "analysis", icon("cog", lib = "glyphicon"),
             menuSubItem("Visualization", tabName = "visualization", icon = shiny::icon("angle-double-right")),
             menuSubItem("Missing Values", tabName = "mvalues", icon = shiny::icon("angle-double-right")),
             menuSubItem("Noise removal", tabName = "nremoval", icon = shiny::icon("angle-double-right")),
             menuSubItem("Normalization", tabName = "normalization", icon = shiny::icon("angle-double-right")),
             menuSubItem("Dimensionality reduction", tabName = "dreduccion", icon = shiny::icon("angle-double-right")),
             menuSubItem("Outlier detection", tabName = "odetection", icon = shiny::icon("angle-double-right"))
             ),
    menuItem("Train", tabName = "train", icon("cog", lib = "glyphicon"),
             menuSubItem("Polynomial", tabName = "polynomial", icon = shiny::icon("angle-double-right")),
             menuSubItem("Neuronal", tabName = "neuronal", icon = shiny::icon("angle-double-right")),
             menuSubItem("SVM", tabName = "svm", icon = shiny::icon("angle-double-right")),
             menuSubItem("Function", tabName = "function", icon = shiny::icon("angle-double-right"))
             ),
    menuItem("Predict", tabName = "predict"),
    menuItem("Validation", tabName = "validation", icon("cog", lib = "glyphicon"),
             menuSubItem("Ten fold cross", tabName = "tenfc", icon = shiny::icon("angle-double-right")),
             menuSubItem("Test/traning", tabName = "test", icon = shiny::icon("angle-double-right")),
             menuSubItem("5X2CV", tabName = "5x2")
             ),
    menuItem("Report", tabName = "report")
  ))
}